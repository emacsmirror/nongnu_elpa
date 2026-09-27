;;; vm-menu-test.el --- Tests for vm-menu.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

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

;;; The folder menu built from a directory tree.  Two functions had one test
;;; between them, that they were bound.

(ert-deftest vm-menu-test-tree-menu-makes-an-item-per-file ()
  "Each file becomes [NAME (FUNCTION FULL-NAME) SELECTABLE].
The menu shows the base name and acts on the whole path, which is the point
of the exercise: a folder menu you can read, that visits the right file."
  (should (equal (vm-menu-hm-tree-make-menu
                  '("/mail/inbox" "/mail/archive") 'vm-visit-folder t)
                 (list (vector "inbox" '(vm-visit-folder "/mail/inbox") t)
                       (vector "archive" '(vm-visit-folder "/mail/archive") t))))
  ;; and the selectable flag is passed through as it stands
  (should (equal (vm-menu-hm-tree-make-menu '("/mail/inbox") 'vm-visit-folder nil)
                 (list (vector "inbox" '(vm-visit-folder "/mail/inbox") nil)))))

(ert-deftest vm-menu-test-tree-menu-nests-a-directory ()
  "A directory becomes a submenu named after itself.
A directory is a list whose first element is its own name."
  (should (equal (vm-menu-hm-tree-make-menu
                  '("/mail/inbox" ("/mail/lists" "/mail/lists/emacs"))
                  'vm-visit-folder t)
                 (list (vector "inbox" '(vm-visit-folder "/mail/inbox") t)
                       (cons "lists"
                             (list (vector "emacs"
                                           '(vm-visit-folder "/mail/lists/emacs")
                                           t)))))))

(ert-deftest vm-menu-test-tree-menu-can-leave-out-hidden-directories ()
  "With NO-HIDDEN-DIRS, a directory whose name begins with a dot is left out.
Without it, it is kept: the argument is what decides, not the dot."
  (let ((tree '(("/mail/.old" "/mail/.old/1996") ("/mail/lists" "/mail/lists/emacs"))))
    (should (= 1 (length (vm-menu-hm-tree-make-menu tree 'vm-visit-folder t t))))
    (should (= 2 (length (vm-menu-hm-tree-make-menu tree 'vm-visit-folder t nil))))))

(ert-deftest vm-menu-test-tree-menu-leaves-out-what-the-regexps-match ()
  "A file matching one of RE-HIDDEN-FILE-LIST is left out.
That is how the auto-save and index files beside a folder stay off the menu."
  (should (equal (vm-menu-hm-tree-make-menu
                  '("/mail/inbox" "/mail/inbox.crash" "/mail/#inbox#")
                  'vm-visit-folder t nil '("\\.crash\\'" "/#[^/]*#\\'"))
                 (list (vector "inbox" '(vm-visit-folder "/mail/inbox") t)))))

(ert-deftest vm-menu-test-tree-menu-can-offer-the-directory-itself ()
  "With INCLUDE-CURRENT-DIR a submenu gets a `.' item for the directory.
Visiting a directory is how VM asks for a folder inside it."
  (let ((menu (vm-menu-hm-tree-make-menu
               '(("/mail/lists" "/mail/lists/emacs"))
               'vm-visit-folder t nil nil t)))
    (should (equal (car (car menu)) "lists"))
    (should (equal (car (cdr (car menu)))
                   (vector "." '(vm-visit-folder "/mail/lists") t)))
    (should (= (length (cdr (car menu))) 2))))

;;; The folder-menu commands that touch the disk (emacs-vm/vm#654)

(defmacro vm-menu-test--in-a-folder-directory (&rest body)
  "Run BODY with `vm-folder-directory' a temporary directory holding a folder.
FOLDER is that folder's file name and DIR the directory.  Rebuilding and
installing the menu is stubbed out: these commands are being tested for
what they do to the disk."
  (declare (indent 0) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-folders" t)))
          (folder (expand-file-name "inbox" dir))
          (vm-folder-directory dir))
     (unwind-protect
         (progn
           (with-temp-file folder (insert "From a@b Mon Jan  1 00:00:00 2024\n"))
           (cl-letf (((symbol-function 'vm-menu-hm-make-folder-menu) #'ignore)
                     ((symbol-function 'vm-menu-hm-install-menu) #'ignore))
             ,@body))
       (delete-directory dir t))))

(defun vm-menu-test--as-read-file-name (typed)
  "A `read-file-name' stub that answers TYPED as the real one would.

The real prompt resolves what is typed against the DIR argument it is
given, so a stub that ignores DIR cannot see a command passing the wrong
one -- which is the whole point of these tests."
  (lambda (_prompt &optional dir &rest _)
    (expand-file-name typed (or dir default-directory))))

(ert-deftest vm-menu-test-renaming-a-folder-lands-beside-it ()
  "REGRESSION: a name typed at the rename prompt names a folder in the same
directory.

The prompt was given `(directory-file-name folder)' as its directory, which
for a file is that same file, so \"archive\" resolved to inbox/archive and
the rename failed.  Typing a full path worked, which is presumably why this
went unnoticed."
  (vm-menu-test--in-a-folder-directory
    (cl-letf (((symbol-function 'read-file-name)
               (vm-menu-test--as-read-file-name "archive")))
      (vm-menu-hm-rename-folder folder))
    (should (file-exists-p (expand-file-name "archive" dir)))
    (should-not (file-exists-p folder))))

(ert-deftest vm-menu-test-renaming-a-folder-that-is-not-there-is-refused ()
  "A folder that does not exist is reported rather than renamed to nothing."
  (vm-menu-test--in-a-folder-directory
    (let ((text-quoting-style 'grave))
      (let ((err (should-error
                  (vm-menu-hm-rename-folder (expand-file-name "absent" dir))
                  :type 'error)))
        (should (string-match-p "does not exist"
                                (error-message-string err)))))))

(ert-deftest vm-menu-test-deleting-a-folder-asks-first ()
  "The delete is a query: answering no leaves the folder where it was."
  (vm-menu-test--in-a-folder-directory
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (vm-menu-hm-delete-folder folder))
    (should (file-exists-p folder))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (vm-menu-hm-delete-folder folder))
    (should-not (file-exists-p folder))))

(ert-deftest vm-menu-test-deleting-a-folder-that-is-not-there-is-refused ()
  "Deleting something absent is reported, and nothing is asked."
  (vm-menu-test--in-a-folder-directory
    (let ((asked nil)
          (text-quoting-style 'grave))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (setq asked t))))
        (let ((err (should-error
                    (vm-menu-hm-delete-folder (expand-file-name "absent" dir))
                    :type 'error)))
          (should (string-match-p "does not exist"
                                  (error-message-string err)))))
      (should-not asked))))

(ert-deftest vm-menu-test-creating-a-directory-makes-it ()
  "`vm-menu-hm-create-dir' creates the directory named at its prompt."
  (vm-menu-test--in-a-folder-directory
    (cl-letf (((symbol-function 'read-file-name)
               (vm-menu-test--as-read-file-name "archive")))
      (vm-menu-hm-create-dir dir))
    (should (file-directory-p (expand-file-name "archive" dir)))))

(ert-deftest vm-menu-test-creating-a-directory-defaults-to-the-folder-directory ()
  "With no parent given the new directory goes in `vm-folder-directory'."
  (vm-menu-test--in-a-folder-directory
    (cl-letf (((symbol-function 'read-file-name)
               (vm-menu-test--as-read-file-name "archive")))
      (vm-menu-hm-create-dir nil))
    (should (file-directory-p (expand-file-name "archive" dir)))))

;;; The image menu and the ImageMagick option (emacs-vm/vm#676)

(ert-deftest vm-menu-test-the-image-menu-follows-the-current-option ()
  "REGRESSION: the image entries are enabled by whether ImageMagick is
available, not by whether the obsolete override was set.

They asked `(stringp vm-imagemagick-convert-program)', which is nil unless
somebody set the option that was replaced in 8.4.0 -- so a reader who set
`vm-imagemagick-program', which is the one the manual documents and the one
found automatically, had every image entry greyed out."
  (let ((enablers
         (seq-filter (lambda (form) (and (consp form)
                                         (memq (car form) '(stringp vm-imagemagick-available-p))))
                     (apply #'append
                            (mapcar (lambda (entry) (and (vectorp entry) (append entry nil)))
                                    (cdr vm-menu-image-menu))))))
    (should enablers)
    (dolist (form enablers)
      (should-not (equal form '(stringp vm-imagemagick-convert-program))))))

(ert-deftest vm-menu-test-imagemagick-is-available-from-the-current-option ()
  "`vm-imagemagick-available-p', which those entries now ask, is true when
only the current option is set."
  (let ((vm-imagemagick-program "/usr/bin/magick")
        (vm-imagemagick-convert-program nil))
    (should (vm-imagemagick-available-p)))
  (let ((vm-imagemagick-program nil)
        (vm-imagemagick-convert-program nil))
    (should-not (vm-imagemagick-available-p)))
  ;; and the obsolete override still works for anyone who set it
  (let ((vm-imagemagick-program nil)
        (vm-imagemagick-convert-program "/usr/bin/convert"))
    (should (vm-imagemagick-available-p))))

(ert-deftest vm-menu-test-image-converters-come-from-the-current-option ()
  "REGRESSION: the image type converters are built from whichever
ImageMagick VM has.

`vm-mime-image-type-converter-alist' was built from the obsolete override
alone, so it was empty for everyone who set only `vm-imagemagick-program',
and VM had no way to convert an image type it cannot display."
  (let ((converters (vm-mime-image-type-converters "/usr/bin/magick")))
    (should (= (length converters) 7))
    (should (member '("image" "image/png" "/usr/bin/magick - png:-") converters))
    (should (member '("image" "image/jpeg" "/usr/bin/magick - jpeg:-") converters)))
  ;; and with no ImageMagick there is nothing to offer
  (should-not (vm-mime-image-type-converters nil)))

(provide 'vm-menu-test)

;;; vm-menu-test.el ends here