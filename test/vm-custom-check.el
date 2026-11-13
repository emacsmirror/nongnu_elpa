;;; vm-custom-check.el --- check VM defcustoms against their types -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Prints every VM `defcustom' whose `:type' does not accept its own default
;; value, every option that declares no `:type' at all, every customize group
;; with nothing in it and every one that cannot be reached from `vm', and exits
;; non-zero if there are any of the first.  Run by
;; `vm-custom-test-every-type-accepts-its-own-default', and by hand as
;;
;;     emacs -Q --batch -L lisp -l test/vm-custom-check.el
;;
;; This is a program rather than part of the test because it has to load every
;; VM module to see every declaration, and those loads add hooks and rewrite
;; menus.  Doing that inside the suite would leave the state behind for the
;; tests that follow, so the test runs this in an Emacs of its own.

;;; Code:

(require 'wid-edit)
;; The `hook' widget and the other Customize types are defined in cus-edit, not
;; in wid-edit; converting one without it fails rather than saying it cannot.
(require 'cus-edit)
(require 'seq)

(defvar vm-custom-check-minimum 300
  "Fewer declarations than this means the load went wrong, not that VM is small.")

(defvar vm-custom-check-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory this file is in, recorded while it is being loaded.
`load-file-name' is bound only during the load, so a function that asks
for it afterwards gets nil.")

(defun vm-custom-check-lisp-directory ()
  "Return the lisp directory of the tree this file is in."
  (expand-file-name "../lisp" vm-custom-check-directory))

(defun vm-custom-check-load-everything ()
  "Load every VM module, so that every `defcustom' has been seen."
  (require 'vm)
  ;; Some types name functions from libraries VM does not load itself, and the
  ;; `function' widget matches a symbol only once it is `fboundp'.  Without
  ;; this, `vm-dnd-protocol-alist' reads as a mismatch and is not one.
  (require 'dnd nil t)
  (dolist (file (directory-files (vm-custom-check-lisp-directory)
                                 nil "\\`vm-.*\\.el\\'"))
    (let ((feature (intern (file-name-sans-extension file))))
      (unless (memq feature '(vm-autoloads vm-cus-load vm-version-conf))
        ;; Not merely a missing file: vm-w3m.el pushes onto a w3m variable as
        ;; it loads, so without w3m installed it raises rather than failing to
        ;; be found.  Either way, what it declares goes unchecked.
        (ignore-errors (require feature nil t))))))

(defun vm-custom-check-options ()
  "Return every VM user option, aliases resolved, sorted by name.
An obsolete name is a `defvaralias' and has no type of its own: the
option it points at has one, and `customize-option' follows the alias
before it builds anything.  So the alias is not an untyped option and is not
a second option either, and resolving it here says so once
(emacs-vm/vm#837)."
  (let ((out nil))
    (mapatoms
     (lambda (sym)
       (when (and (string-prefix-p "vm" (symbol-name sym))
                  (custom-variable-p sym)
                  (boundp sym))
         (push (indirect-variable sym) out))))
    (sort (delete-dups out)
          (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun vm-custom-check-customs ()
  "Return every VM option that has a `:type', sorted by name."
  (seq-filter (lambda (sym) (get sym 'custom-type))
              (vm-custom-check-options)))

(defun vm-custom-check-groups ()
  "Return every VM customize group, sorted by name."
  (let ((out nil))
    (mapatoms
     (lambda (sym)
       (when (and (string-prefix-p "vm" (symbol-name sym))
                  (get sym 'group-documentation))
         (push sym out))))
    (sort out (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun vm-custom-check-empty-groups ()
  "Return every VM customize group that holds nothing, sorted by name.
Customize shows such a group in its parent and then shows the reader an
empty page, which says the options are somewhere they are not."
  (seq-remove (lambda (sym) (get sym 'custom-group))
              (vm-custom-check-groups)))

(defun vm-custom-check-reachable-groups ()
  "Return every VM customize group reachable from `vm'.
A group is reached through its parent's `custom-group' property, which is
where `defgroup' records a `:group' argument, so this walks the tree the
reader walks in Customize."
  (let ((seen nil)
        (todo (list 'vm)))
    (while todo
      (let ((group (pop todo)))
        (unless (memq group seen)
          (push group seen)
          (dolist (entry (get group 'custom-group))
            (when (eq (cadr entry) 'custom-group)
              (push (car entry) todo))))))
    seen))

(defun vm-custom-check-unreachable-groups ()
  "Return every VM customize group Customize cannot be navigated to, sorted.
A group whose `:group' names a group that does not exist, or that names no
group at all, is still defined and still holds its options, and no path from
`vm' leads to it: the options in it are reachable only by name."
  (let ((reachable (vm-custom-check-reachable-groups)))
    (seq-remove (lambda (sym) (memq sym reachable))
                (vm-custom-check-groups))))

(defun vm-custom-check-untyped ()
  "Return every VM option that declares no `:type', sorted by name.
Customize gives a raw sexp editor for one of those, which asks the reader
to know the structure the code wants and gives no help with it."
  (seq-remove (lambda (sym) (get sym 'custom-type))
              (vm-custom-check-options)))


;;; Code that writes a user option

;; A `setq' of a `defcustom' leaves Customize reporting the option as "CHANGED
;; outside Customize", and a save there writes into the reader's custom file a
;; value they never chose.  It also throws away what they did choose.  Nine
;; commands and one load-time registration did it (emacs-vm/vm#945, and
;; emacs-vm/vm#943 before it); each now reads the option and keeps what it
;; works out somewhere of its own.
;;
;; This reads the sources rather than watching values change, because the
;; write happens where a test is not: in a command nothing calls, on a path a
;; server failure takes, at load time.

(defvar vm-custom-check-allowed-option-writes
  '((vm-toggle-best-mime . vm-mime-alternative-show-method)
    (vm-summary-faces-mode . vm-summary-enable-faces)
    (vm-serial-set-token . vm-serial-token-alist)
    (vm-serial-set-mails . vm-serial-mails-alist)
    (vm-pcrisis-my-identities . vm-pcrisis-conditions)
    (vm-pcrisis-my-identities . vm-pcrisis-default-rules)
    (vm-pcrisis-my-identities . vm-pcrisis-actions)
    (bbdb/vm-set-virtual-folder-alist . vm-virtual-folder-alist)
    (bbdb/vm-set-virtual-folder-alist-by-mail-alias . vm-virtual-folder-alist))
  "Definitions allowed to set an option: setting it is what they are for.
Each entry is (DEFINITION . OPTION).  A toggle or a setter that a reader
runs to change that setting is not the thing this looks for, which is an
option written as a side effect of doing something else.")

(defvar vm-custom-check-options-table nil
  "The options to look for writes of, as a hash table.")

(defvar vm-custom-check-file nil "The file being scanned.")
(defvar vm-custom-check-definition nil "The definition being scanned.")
(defvar vm-custom-check-line 0 "Where that definition ends.")
(defvar vm-custom-check-writes nil "What has been found so far.")

(defun vm-custom-check-build-options-table ()
  "Return the VM user options a write would reach, as a hash table.
An option made buffer-local by `make-variable-buffer-local' is left out: a
`setq' of one of those makes a buffer-local binding and the default value,
which is what Customize shows and saves, is not touched."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (sym (vm-custom-check-options))
      (unless (local-variable-if-set-p sym)
        (puthash sym t table)))
    table))

(defun vm-custom-check-allowed-p (option)
  "Return non-nil if the definition being scanned may write OPTION."
  (let ((entry (cons vm-custom-check-definition option)))
    (member entry vm-custom-check-allowed-option-writes)))

(defun vm-custom-check-note-write (place how bound)
  "Record a write of PLACE by a form of type HOW, unless BOUND holds it."
  (when (and (symbolp place)
             (gethash place vm-custom-check-options-table)
             (not (memq place bound))
             (not (vm-custom-check-allowed-p place)))
    (push (format "%s:%d %s %s %s"
                  vm-custom-check-file vm-custom-check-line
                  vm-custom-check-definition how place)
          vm-custom-check-writes)))

(defun vm-custom-check-binding-names (bindings)
  "Return the variables BINDINGS, a `let' binding list, binds."
  (delq nil (mapcar (lambda (b) (cond ((symbolp b) b)
                                      ((consp b) (car b))))
                    (and (listp bindings) bindings))))

(defun vm-custom-check-local-names (form)
  "Return the variables FORM calls `make-local-variable' on directly.
A `setq' after one of those sets the buffer-local binding, not the default
value, which is what `vm-startup-apply-summary' and
`vm-mime-fragment-composition' do."
  (let ((out nil)
        (rest form))
    (while (consp rest)
      (let ((f (car rest)))
        (when (and (consp f) (eq (car f) 'make-local-variable)
                   (consp (cadr f)) (eq (car (cadr f)) 'quote))
          (push (cadr (cadr f)) out)))
      (setq rest (cdr rest)))
    out))

(defun vm-custom-check-walk-list (forms bound)
  "Walk each of FORMS, which may be an improper list, with BOUND bound."
  (let ((rest forms))
    (while (consp rest)
      (vm-custom-check-walk (car rest) bound)
      (setq rest (cdr rest)))))

(defun vm-custom-check-walk-let (form bound)
  "Walk a `let' or `let*' FORM, whose bindings are added to BOUND."
  (let ((inner (append (vm-custom-check-binding-names (cadr form)) bound)))
    (dolist (b (and (listp (cadr form)) (cadr form)))
      (when (consp b) (vm-custom-check-walk (cadr b) bound)))
    (vm-custom-check-walk-list (cddr form) inner)))

(defun vm-custom-check-walk-function (form bound)
  "Walk a `defun' or `lambda' FORM, whose arguments are added to BOUND."
  (let* ((args (if (eq (car form) 'lambda) (cadr form) (nth 2 form)))
         (inner (append (and (listp args)
                             (seq-filter #'symbolp args))
                        bound)))
    (vm-custom-check-walk-list (cddr form) inner)))

(defun vm-custom-check-walk-setq (form bound)
  "Walk a `setq' or `setq-default' FORM, noting what it writes."
  (let ((rest (cdr form))
        (how (symbol-name (car form))))
    (while rest
      (vm-custom-check-note-write (car rest) how bound)
      (vm-custom-check-walk (cadr rest) bound)
      (setq rest (cddr rest)))))

(defun vm-custom-check-walk-place (form bound)
  "Walk a form that writes a place: `push', `setf', `add-to-list' and so on."
  (let ((head (car form))
        (how (symbol-name (car form))))
    (cond ((memq head '(push cl-pushnew))
           (vm-custom-check-note-write (nth 2 form) how bound)
           (vm-custom-check-walk (nth 1 form) bound))
          ((eq head 'add-to-list)
           (let ((q (nth 1 form)))
             (when (and (consp q) (eq (car q) 'quote))
               (vm-custom-check-note-write (cadr q) how bound)))
           (vm-custom-check-walk (nth 2 form) bound))
          (t
           (let ((rest (cdr form)))
             (while rest
               (vm-custom-check-note-write (car rest) how bound)
               (setq rest (if (eq head 'setf) (cddr rest) (cdr rest)))))))))

(defun vm-custom-check-walk (form bound)
  "Walk FORM, looking for a write of an option not named in BOUND."
  (when (consp form)
    (let ((head (car form))
          (bound (append (vm-custom-check-local-names form) bound)))
      (cond
       ((memq head '(let let*)) (vm-custom-check-walk-let form bound))
       ((memq head '(lambda defun defmacro defsubst cl-defun cl-defmacro
                     iter-defun cl-iter-defun))
        (vm-custom-check-walk-function form bound))
       ((memq head '(setq setq-default)) (vm-custom-check-walk-setq form bound))
       ((memq head '(push cl-pushnew add-to-list setf cl-incf cl-decf))
        (vm-custom-check-walk-place form bound))
       (t (vm-custom-check-walk-list form bound))))))

(defun vm-custom-check-scan-file (file)
  "Scan FILE for writes of a user option, adding to `vm-custom-check-writes'."
  (setq vm-custom-check-file (file-name-nondirectory file))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((eof (make-symbol "eof"))
          (form nil))
      (while (not (eq eof (setq form (condition-case nil
                                         (read (current-buffer))
                                       (end-of-file eof)))))
        (when (consp form)
          (setq vm-custom-check-line (line-number-at-pos)
                vm-custom-check-definition (if (symbolp (nth 1 form))
                                               (nth 1 form)
                                             (car form)))
          (vm-custom-check-walk form nil))))))

(defun vm-custom-check-option-writes ()
  "Return every write of a VM user option in the lisp tree, and how many
files were read, as (WRITES . FILES).  The generated files are not read:
nothing is written by hand in them."
  (let ((files (seq-remove
                (lambda (f) (member (file-name-nondirectory f)
                                    '("vm-autoloads.el" "vm-cus-load.el"
                                      "vm-version-conf.el")))
                (directory-files (vm-custom-check-lisp-directory)
                                 t "\\`vm-.*\\.el\\'"))))
    (setq vm-custom-check-options-table (vm-custom-check-build-options-table)
          vm-custom-check-writes nil)
    (dolist (file files)
      (vm-custom-check-scan-file file))
    (cons (nreverse vm-custom-check-writes) (length files))))

(defun vm-custom-check-report ()
  "Print the mismatches and how many variables were checked.
Return a cons of (CHECKED . MISMATCHES)."
  (let ((customs (vm-custom-check-customs))
        (mismatched nil))
    (dolist (sym customs)
      (let ((type (get sym 'custom-type))
            (value (default-value sym)))
        (condition-case err
            (unless (widget-apply (widget-convert type) :match value)
              (push (format "%s: type %S rejects default %S" sym type value)
                    mismatched))
          (error (push (format "%s: type %S could not be converted (%s)"
                               sym type (error-message-string err))
                       mismatched)))))
    (dolist (line (nreverse mismatched))
      (princ (format "MISMATCH %s\n" line)))
    (dolist (sym (vm-custom-check-untyped))
      (princ (format "UNTYPED %s\n" sym)))
    (dolist (sym (vm-custom-check-empty-groups))
      (princ (format "EMPTYGROUP %s\n" sym)))
    (dolist (sym (vm-custom-check-unreachable-groups))
      (princ (format "UNREACHABLE %s\n" sym)))
    (let ((found (vm-custom-check-option-writes)))
      (dolist (line (car found))
        (princ (format "OPTIONWRITE %s\n" line)))
      (princ (format "scanned %d VM lisp files\n" (cdr found))))
    (princ (format "checked %d VM defcustoms\n" (length customs)))
    (princ (format "checked %d VM customize groups\n"
                   (length (vm-custom-check-groups))))
    (cons (length customs) (length mismatched))))

(when noninteractive
  (vm-custom-check-load-everything)
  (let* ((result (vm-custom-check-report))
         (checked (car result))
         (mismatches (cdr result)))
    (kill-emacs (cond ((< checked vm-custom-check-minimum) 2)
                      ((> mismatches 0) 1)
                      (t 0)))))

(provide 'vm-custom-check)

;;; vm-custom-check.el ends here
