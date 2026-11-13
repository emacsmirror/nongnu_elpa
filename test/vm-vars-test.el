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
documented alias is still defined under its name."
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

;;; Face specs name attributes GNU Emacs has (emacs-vm/vm#854)
;;
;; Five specs named XEmacs face attributes: `:strikethru', XEmacs's spelling
;; of `:strike-through'; `:strike-trhough', a typo for it; and `:dim', which
;; GNU Emacs has no equivalent of.  Each was ignored, so nothing rendered as
;; it read, and each cost a byte-compilation warning.

(defconst vm-vars-test--face-attribute-keywords
  '(:inherit :extend :family :foundry :width :height :weight :slant
    :foreground :distant-foreground :background :underline :overline
    :strike-through :box :inverse-video :stipple :font :bold :italic)
  "The attribute keywords a `defface' spec may name.
The byte compiler's own list, which is what emits the warning this guards
against.  `:bold' and `:italic' are the obsolete spellings of `:weight' and
`:slant'; the compiler takes them and so does this.")

(defun vm-vars-test--display-attributes (entry)
  "The attribute plist of one defface spec ENTRY, whichever syntax it uses.
Old is (DISPLAY (ATTS...)), new is (DISPLAY ATTS...), and the compiler tells
them apart the same way."
  (let ((atts (cdr entry)))
    (if (and (consp atts) (null (cdr atts))) (car atts) atts)))

(defun vm-vars-test--spec-keywords (face)
  "Every attribute keyword FACE's defface spec names, across its displays."
  (let ((found nil))
    (dolist (entry (get face 'face-defface-spec) found)
      (let ((atts (vm-vars-test--display-attributes entry)))
        (while (consp atts)
          (when (keywordp (car atts)) (push (car atts) found))
          (setq atts (cddr atts)))))))

(defun vm-vars-test--faces-vm-defines ()
  "Every face VM has a `defface' spec for, by name."
  (let ((faces nil))
    (mapatoms (lambda (symbol)
                (when (and (get symbol 'face-defface-spec)
                           (string-prefix-p "vm-" (symbol-name symbol)))
                  (push symbol faces))))
    (sort faces #'string<)))

(defun vm-vars-test--attributes-emacs-lacks (face)
  "Those of FACE's spec keywords that are not face attributes, as (FACE KEY)."
  (let ((absent nil))
    (dolist (keyword (vm-vars-test--spec-keywords face) absent)
      (unless (memq keyword vm-vars-test--face-attribute-keywords)
        (push (list face keyword) absent)))))

(ert-deftest vm-vars-test-every-face-spec-names-attributes-that-exist ()
  "REGRESSION: no defface spec of VM's names an attribute GNU Emacs lacks.
Such a spec is dropped where it stands, so the face renders as if the
display it names had said nothing, and byte-compiling the file warns.
Answers with every offender at once, not the first."
  (require 'vm-net)
  (require 'vm-epg)
  (require 'vm-pcrisis)
  (let ((faces (vm-vars-test--faces-vm-defines)))
    (should (> (length faces) 15))      ; it found them, not an empty run
    (should (equal nil
                   (mapcan #'vm-vars-test--attributes-emacs-lacks faces)))))

;;; Options read, and what VM works out kept apart from them (emacs-vm/vm#945)

(ert-deftest vm-vars-test-the-spool-files-come-from-the-environment ()
  "REGRESSION: `vm-spool-files\=' reads MAILPATH and MAIL without writing
the option of that name.  It used to store what it found there, so Customize
reported the option as changed outside Customize and a save wrote the
environment into the reader\='s custom file."
  (let ((vm-spool-files nil)
        (vm-spool-files-from-environment nil)
        (process-environment (append '("MAILPATH=/nonexistent/spool/mailpath"
                                       "MAIL=/nonexistent/spool/mail")
                                     process-environment)))
    (should (equal (vm-spool-files) '("/nonexistent/spool/mailpath")))
    (should-not vm-spool-files)
    (should (equal vm-spool-files-from-environment
                   '("/nonexistent/spool/mailpath"))))
  ;; MAIL where MAILPATH says nothing
  (let ((vm-spool-files nil)
        (vm-spool-files-from-environment nil)
        (process-environment (append '("MAILPATH=" "MAIL=/nonexistent/spool/mail")
                                     process-environment)))
    (should (equal (vm-spool-files) '("/nonexistent/spool/mail")))
    (should-not vm-spool-files))
  ;; and the option wins wherever it is set
  (let ((vm-spool-files '("/home/reader/INBOX"))
        (vm-spool-files-from-environment nil)
        (process-environment (append '("MAILPATH=/nonexistent/spool/mailpath")
                                     process-environment)))
    (should (equal (vm-spool-files) '("/home/reader/INBOX")))))

(ert-deftest vm-vars-test-a-grep-that-signalled-is-not-run-again ()
  "REGRESSION: a grep that signals sets `vm-grep-program-unusable\=' and not
the option, which used to be set to nil: the reader\='s own program name was
gone for the rest of the session and Customize reported it changed."
  (let ((vm-grep-program "grep")
        (vm-grep-program-unusable nil))
    (should (equal (vm-grep-program) "grep"))
    (setq vm-grep-program-unusable t)
    (should-not (vm-grep-program))
    (should (equal vm-grep-program "grep")))
  (let ((vm-grep-program nil)
        (vm-grep-program-unusable nil))
    (should-not (vm-grep-program))))

(ert-deftest vm-vars-test-a-bug-report-asks-for-trace-buffers-of-its-own ()
  "REGRESSION: how many trace buffers a bug report keeps is read before the
option, and the start commands no longer set the option to 20 and leave it
there."
  (let ((vm-imap-keep-trace-buffer 1)
        (vm-imap-bug-report-trace-count nil))
    (should (equal (vm-imap-keep-trace-buffer) 1))
    (setq vm-imap-bug-report-trace-count 20)
    (should (equal (vm-imap-keep-trace-buffer) 20))
    (should (equal vm-imap-keep-trace-buffer 1)))
  (let ((vm-pop-keep-trace-buffer nil)
        (vm-pop-bug-report-trace-count nil))
    (should-not (vm-pop-keep-trace-buffer))
    (setq vm-pop-bug-report-trace-count 20)
    (should (equal (vm-pop-keep-trace-buffer) 20))
    (should-not vm-pop-keep-trace-buffer)))

(provide 'vm-vars-test)

;;; vm-vars-test.el ends here
