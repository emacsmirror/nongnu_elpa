;;; jabber-reload.el --- Reload Jabber source files safely  -*- lexical-binding: t; -*-

;;; Commentary:

;; Reload Jabber in source dependency order while restoring live function and
;; keymap bindings if any file fails to load.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defconst jabber-reload--load-time-forms
  '(and condition-case condition-case-unless-debug cond
    eval-and-compile eval-when-compile if let let* or progn unless when))

(defun jabber-reload--source-files (root)
  "Return reloadable Jabber source files below ROOT."
  (seq-remove
   (lambda (file) (string-suffix-p "/jabber-autoloads.el" file))
   (sort (file-expand-wildcards
          (expand-file-name "lisp/*.el" root))
         #'string<)))

(defun jabber-reload--read-forms (file)
  "Read and return all Lisp forms in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (done forms)
      (while (not done)
        (let ((start (point)))
          (condition-case error-data
              (push (read (current-buffer)) forms)
            (end-of-file
             (if (jabber-reload--ignorable-tail-p start)
                 (setq done t)
               (signal (car error-data) (cdr error-data)))))))
      (nreverse forms))))

(defun jabber-reload--ignorable-tail-p (start)
  "Return non-nil when buffer text after START is whitespace or comments."
  (save-excursion
    (goto-char start)
    (with-syntax-table emacs-lisp-mode-syntax-table
      (forward-comment (point-max)))
    (eobp)))

(defun jabber-reload--quoted-symbol (form)
  "Return the symbol quoted by FORM, or nil."
  (when (and (consp form)
             (eq (car form) 'quote)
             (symbolp (cadr form)))
    (cadr form)))

(defun jabber-reload--load-time-children (form)
  "Return load-time child forms of FORM."
  (pcase (car-safe form)
    ((or 'let 'let*)
     (append (delq nil
                   (mapcar (lambda (binding)
                             (and (consp binding) (cadr binding)))
                           (cadr form)))
             (cddr form)))
    ((or 'condition-case 'condition-case-unless-debug)
     (cons (caddr form) (mapcan #'cdr (cdddr form))))
    ('cond (apply #'append (cdr form)))
    ((pred (lambda (head) (memq head jabber-reload--load-time-forms)))
     (cdr form))))

(defun jabber-reload--requires (form)
  "Return features required while evaluating FORM at top level."
  (if (eq (car-safe form) 'require)
      (let ((feature (jabber-reload--quoted-symbol (cadr form))))
        (and feature (list feature)))
    (mapcan #'jabber-reload--requires
            (jabber-reload--load-time-children form))))

(defun jabber-reload--provide (form)
  "Return the feature provided directly by FORM, or nil."
  (when (eq (car-safe form) 'provide)
    (jabber-reload--quoted-symbol (cadr form))))

(defun jabber-reload--map-symbol (form)
  "Return the keymap variable declared by FORM, or nil."
  (pcase (car-safe form)
    ((or 'defvar-keymap 'keymap-popup-define) (cadr form))
    ('define-derived-mode
     (intern (concat (symbol-name (cadr form)) "-map")))
    ('defvar
     (let ((symbol (cadr form)))
       (when (and (symbolp symbol)
                  (string-match-p "\\(?:-map\\|keymap\\)\\'" (symbol-name symbol)))
         symbol)))))

(defun jabber-reload--record (file)
  "Return source metadata for FILE."
  (let* ((forms (jabber-reload--read-forms file))
         (provides (delq nil (mapcar #'jabber-reload--provide forms)))
         (requires (delete-dups (mapcan #'jabber-reload--requires forms)))
         (maps (delete-dups (delq nil
                                  (mapcar #'jabber-reload--map-symbol forms)))))
    (list :file file :provides provides :requires requires :maps maps)))

(defun jabber-reload--provider-alist (records)
  "Return an alist mapping features to files in RECORDS."
  (let (providers)
    (dolist (record records providers)
      (dolist (feature (plist-get record :provides))
        (when-let* ((previous (alist-get feature providers)))
          (error "Feature %s is provided by %s and %s"
                 feature previous (plist-get record :file)))
        (push (cons feature (plist-get record :file)) providers)))))

(defun jabber-reload--dependencies (records providers)
  "Return file dependencies from RECORDS and PROVIDERS."
  (mapcar
   (lambda (record)
     (let* ((file (plist-get record :file))
            (dependencies
             (delq nil
                   (mapcar (lambda (feature)
                             (alist-get feature providers))
                           (plist-get record :requires)))))
       (cons file (delete file (delete-dups dependencies)))))
   records))

(defun jabber-reload--topological-order (files dependencies)
  "Order FILES according to DEPENDENCIES."
  (let ((remaining (copy-sequence files))
        ordered)
    (while remaining
      (let ((ready (seq-filter
                    (lambda (file)
                      (not (seq-intersection
                            (alist-get file dependencies nil nil #'equal)
                            remaining)))
                    remaining)))
        (unless ready
          (error "Reload dependency cycle: %s"
                 (mapconcat #'file-name-nondirectory remaining ", ")))
        (setq ordered (append ordered ready)
              remaining (seq-difference remaining ready))))
    ordered))

(defun jabber-reload--plan (root)
  "Return a dependency-ordered reload plan for ROOT."
  (let* ((records (mapcar #'jabber-reload--record
                          (jabber-reload--source-files root)))
         (providers (jabber-reload--provider-alist records))
         (files (mapcar (lambda (record) (plist-get record :file)) records)))
    (list :files (jabber-reload--topological-order
                  files (jabber-reload--dependencies records providers))
          :maps (delete-dups
                 (mapcan (lambda (record)
                           (copy-sequence (plist-get record :maps)))
                         records)))))

(defun jabber-reload--snapshot-maps (maps)
  "Return the current binding state of MAPS."
  (mapcar (lambda (symbol)
            (list symbol (boundp symbol)
                  (and (boundp symbol) (symbol-value symbol))))
          maps))

(defun jabber-reload--snapshot-functions ()
  "Return the current function state of interned Jabber symbols."
  (let (snapshots)
    (mapatoms
     (lambda (symbol)
       (when (string-prefix-p "jabber-" (symbol-name symbol))
         (let ((bound (fboundp symbol)))
           (push (list symbol bound
                       (and bound (symbol-function symbol)))
                 snapshots)))))
    snapshots))

(defun jabber-reload--restore-functions (snapshots)
  "Restore Jabber function SNAPSHOTS and remove new definitions."
  (let ((saved-symbols (mapcar #'car snapshots)))
    (mapatoms
     (lambda (symbol)
       (when (and (string-prefix-p "jabber-" (symbol-name symbol))
                  (fboundp symbol)
                  (not (memq symbol saved-symbols)))
         (fmakunbound symbol)))))
  (dolist (snapshot snapshots)
    (if (cadr snapshot)
        (fset (car snapshot) (caddr snapshot))
      (fmakunbound (car snapshot)))))

(defun jabber-reload--restore-maps (snapshots)
  "Restore keymap SNAPSHOTS."
  (dolist (snapshot snapshots)
    (if (cadr snapshot)
        (set (car snapshot) (caddr snapshot))
      (makunbound (car snapshot)))))

(defun jabber-reload--capture-buffer-maps (maps)
  "Capture live buffers using a conventional old map from MAPS."
  (let (buffers)
    (dolist (buffer (buffer-list) buffers)
      (with-current-buffer buffer
        (let ((symbol (intern-soft (concat (symbol-name major-mode) "-map"))))
          (when (and (memq symbol maps)
                     (boundp symbol)
                     (eq (current-local-map) (symbol-value symbol)))
            (push (list buffer symbol (current-local-map)) buffers)))))))

(defun jabber-reload--set-buffer-maps (buffers old)
  "Set BUFFERS to their saved or current map bindings.
When OLD is non-nil, restore the saved map objects."
  (dolist (entry buffers)
    (when (buffer-live-p (car entry))
      (with-current-buffer (car entry)
        (use-local-map
         (if old (caddr entry) (symbol-value (cadr entry))))))))

(defun jabber-reload--validate-maps (maps)
  "Signal an error unless MAPS are bound to keymaps."
  (dolist (symbol maps)
    (unless (and (boundp symbol) (keymapp (symbol-value symbol)))
      (error "Reload did not define keymap %s" symbol))))

;;;###autoload
(defun jabber-reload (root)
  "Reload the Jabber checkout at ROOT in dependency order.
Restore Jabber function and keymap bindings if any file fails."
  (let* ((plan (jabber-reload--plan root))
         (files (plist-get plan :files))
         (maps (plist-get plan :maps))
         (functions (jabber-reload--snapshot-functions))
         (snapshots (jabber-reload--snapshot-maps maps))
         (buffers (jabber-reload--capture-buffer-maps maps))
         (old-binding (lookup-key ctl-x-map (kbd "C-j")))
         succeeded)
    (unwind-protect
        (progn
          (mapc #'makunbound maps)
          (mapc #'load-file files)
          (jabber-reload--validate-maps maps)
          (jabber-reload--set-buffer-maps buffers nil)
          (setq succeeded t)
          (list :loaded (length files) :rebound (length buffers)))
      (unless succeeded
        (jabber-reload--restore-functions functions)
        (jabber-reload--restore-maps snapshots)
        (define-key ctl-x-map (kbd "C-j") old-binding)
        (jabber-reload--set-buffer-maps buffers t)))))

(provide 'jabber-reload)

;; Local Variables:
;; no-update-autoloads: t
;; End:
;;; jabber-reload.el ends here
