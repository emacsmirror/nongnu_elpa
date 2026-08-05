;;; jabber-test-reload.el --- Tests for Jabber live reload  -*- lexical-binding: t; -*-

;;; Commentary:

;; Dependency ordering and rollback for live reload.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'jabber-reload)

(defconst jabber-test-reload--root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name))))

(defvar jabber-test-reload-mode-map (make-sparse-keymap))

(defun jabber-test-reload--position (suffix files)
  "Return the position of SUFFIX in FILES."
  (seq-position files suffix
                (lambda (file expected)
                  (string-suffix-p expected file))))

(defun jabber-test-reload--clean-emacs-eval (form)
  "Evaluate FORM in a clean child Emacs and require success."
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory)
            nil (current-buffer) nil
            "-Q" "--batch"
            "-L" (expand-file-name "lisp" jabber-test-reload--root)
            "--eval" "(setq load-prefer-newer t)"
            "--eval" (prin1-to-string form))))
      (unless (zerop status)
        (ert-fail (buffer-string))))))

(defun jabber-test-reload--generated-autoloads ()
  "Return symbols exported by the generated Jabber autoload file."
  (delq nil
        (mapcar
         (lambda (form)
           (and (eq (car-safe form) 'autoload)
                (jabber-reload--quoted-symbol (cadr form))))
         (jabber-reload--read-forms
          (expand-file-name "lisp/jabber-autoloads.el"
                            jabber-test-reload--root)))))

(ert-deftest jabber-test-reload-clean-source-boundaries ()
  "Load source directly while preserving optional and cyclic boundaries."
  (jabber-test-reload--clean-emacs-eval
   '(progn
      (require 'jabber-console)
      (when (featurep 'jabber-chatbuffer)
        (error "Console source eagerly loaded chat-buffer support"))
      (jabber-chat-ewoc-unregister-node nil)
      (unless (featurep 'jabber-chatbuffer)
        (error "Console truncation boundary did not load chat-buffer support"))
      (require 'jabber-roster-menu)
      (when (featurep 'jabber-omemo-trust)
        (error "Roster source eagerly loaded OMEMO trust support"))
      (require 'jabber-chat-commands)
      (when (or (featurep 'jabber-autoloads)
                (featurep 'jabber-omemo)
                (featurep 'jabber-omemo-trust)
                (featurep 'jabber-openpgp)
                (featurep 'jabber-openpgp-legacy))
        (error "Source load activated generated or optional features"))))
  (jabber-test-reload--clean-emacs-eval
   '(progn
      (require 'jabber-bookmarks)
      (when (featurep 'jabber-muc)
        (error "Bookmark source eagerly loaded MUC"))
      (unless (stringp (jabber-muc-get-buffer "room@example.org"))
        (error "Bookmark-to-MUC boundary returned no buffer name"))
      (unless (featurep 'jabber-muc)
        (error "Bookmark-to-MUC boundary did not load MUC")))))

(ert-deftest jabber-test-reload-generated-autoload-contract ()
  "Export public package entries without private runtime helpers."
  (let ((autoloads (jabber-test-reload--generated-autoloads)))
    (dolist (function '(jabber-muc-get-buffer
                        jabber-message-thread-browse
                        jabber-omemo-show-fingerprints
                        jabber-roster-popup))
      (should (memq function autoloads)))
    (dolist (function '(jabber-chat--insert-backlog-chunked
                        jabber-omemo--send-chat))
      (should-not (memq function autoloads)))))

(ert-deftest jabber-test-reload-orders-real-source-graph ()
  "Order the current source tree by its declared dependencies."
  (let* ((source-files
          (jabber-reload--source-files jabber-test-reload--root))
         (records (mapcar #'jabber-reload--record source-files))
         (providers (jabber-reload--provider-alist records))
         (dependencies (jabber-reload--dependencies records providers))
         (files (plist-get (jabber-reload--plan jabber-test-reload--root)
                           :files)))
    (should (= (length files) (length source-files)))
    (should-not (seq-some
                 (lambda (file)
                   (string-suffix-p "jabber-autoloads.el" file))
                 files))
    (dolist (entry dependencies)
      (dolist (dependency (cdr entry))
        (should (< (seq-position files dependency)
                   (seq-position files (car entry))))))))

(ert-deftest jabber-test-reload-scans-supported-load-time-forms ()
  "Find requirements in every supported load-time container."
  (dolist (case
           '(((and (require 'jabber-and)) jabber-and)
             ((condition-case nil nil
                  (error (require 'jabber-condition)))
              jabber-condition)
             ((condition-case-unless-debug nil nil
                  (error (require 'jabber-condition-debug)))
              jabber-condition-debug)
             ((cond (t (require 'jabber-cond))) jabber-cond)
             ((eval-and-compile (require 'jabber-eval-and)) jabber-eval-and)
             ((eval-when-compile (require 'jabber-eval-when)) jabber-eval-when)
             ((if t (require 'jabber-if)) jabber-if)
             ((let ((x (require 'jabber-let))) x) jabber-let)
             ((let (x) (require 'jabber-let-bare)) jabber-let-bare)
             ((let* ((x (require 'jabber-let-star))) x) jabber-let-star)
             ((let* (x) (require 'jabber-let-star-bare))
              jabber-let-star-bare)
             ((or (require 'jabber-or)) jabber-or)
             ((progn (require 'jabber-progn)) jabber-progn)
             ((unless nil (require 'jabber-unless)) jabber-unless)
             ((when t (require 'jabber-when)) jabber-when)))
    (should (memq (cadr case)
                  (jabber-reload--requires (car case))))))

(ert-deftest jabber-test-reload-rejects-malformed-source-before-loading ()
  "Reject truncated source before reloading any file."
  (let* ((root (make-temp-file "jabber-reload-" t))
         (directory (expand-file-name "lisp" root))
         (file (expand-file-name "jabber-broken.el" directory))
         load-started)
    (unwind-protect
        (progn
          (make-directory directory)
          (write-region "(provide 'jabber-prefix)\n(defun broken ("
                        nil file nil 'silent)
          (cl-letf (((symbol-function 'load-file)
                     (lambda (_file) (setq load-started t))))
            (should-error (jabber-reload root) :type 'end-of-file)
            (should-not load-started)))
      (delete-directory root t))))

(ert-deftest jabber-test-reload-rejects-dependency-cycle ()
  "Report every file remaining in a dependency cycle."
  (let ((condition
         (should-error
          (jabber-reload--topological-order
           '("/tmp/a.el" "/tmp/b.el")
           '(("/tmp/a.el" "/tmp/b.el")
             ("/tmp/b.el" "/tmp/a.el")))
          :type 'error)))
    (should (string-match-p "a\\.el, b\\.el" (error-message-string condition)))))

(ert-deftest jabber-test-reload-rejects-duplicate-provider ()
  "Reject two source files that provide the same feature."
  (should-error
   (jabber-reload--provider-alist
    '((:file "/tmp/a.el" :provides (jabber-test-feature))
      (:file "/tmp/b.el" :provides (jabber-test-feature))))
   :type 'error))

(ert-deftest jabber-test-reload-restores-maps-after-failure ()
  "Restore key bindings and exact map objects after a load error."
  (let* ((old-map jabber-test-reload-mode-map)
         (old-binding (lookup-key ctl-x-map (kbd "C-j")))
         condition)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-reload--plan)
                   (lambda (_root)
                     '(:files ("good.el" "bad.el")
                       :maps (jabber-test-reload-mode-map))))
                  ((symbol-function 'load-file)
                   (lambda (file)
                     (if (equal file "good.el")
                         (progn
                           (setq jabber-test-reload-mode-map
                                 (make-sparse-keymap))
                           (define-key ctl-x-map (kbd "C-j") #'ignore))
                       (error "Reload failed")))))
          (setq condition (should-error (jabber-reload "/tmp")
                                        :type 'error))
          (should (equal (error-message-string condition) "Reload failed"))
          (should (eq jabber-test-reload-mode-map old-map))
          (should (eq (lookup-key ctl-x-map (kbd "C-j")) old-binding)))
      (setq jabber-test-reload-mode-map old-map)
      (define-key ctl-x-map (kbd "C-j") old-binding))))

(ert-deftest jabber-test-reload-restores-functions-after-failure ()
  "Restore command definitions used by live buffers after a load error."
  (let* ((command 'jabber-test-reload-command)
         (had-function (fboundp command))
         (saved-function (and had-function (symbol-function command)))
         (saved-map jabber-test-reload-mode-map)
         (old-map (make-sparse-keymap)))
    (unwind-protect
        (progn
          (fset command (lambda () (interactive) 'old))
          (define-key old-map (kbd "RET") command)
          (setq jabber-test-reload-mode-map old-map)
          (with-temp-buffer
            (setq major-mode 'jabber-test-reload-mode)
            (use-local-map old-map)
            (cl-letf (((symbol-function 'jabber-reload--plan)
                       (lambda (_root)
                         '(:files ("good.el" "bad.el")
                           :maps (jabber-test-reload-mode-map))))
                      ((symbol-function 'load-file)
                       (lambda (file)
                         (if (equal file "good.el")
                             (let ((new-map (make-sparse-keymap)))
                               (fset command
                                     (lambda () (interactive) 'new))
                               (define-key new-map (kbd "RET") command)
                               (setq jabber-test-reload-mode-map new-map))
                           (error "Reload failed")))))
              (should-error (jabber-reload "/tmp") :type 'error)
              (should (eq (current-local-map) old-map))
              (should (eq (funcall (local-key-binding (kbd "RET")))
                          'old)))))
      (setq jabber-test-reload-mode-map saved-map)
      (if had-function
          (fset command saved-function)
        (fmakunbound command)))))

(ert-deftest jabber-test-reload-rebinds-live-buffer-map ()
  "Replace the exact old mode map without replacing an equal copy."
  (let ((old-map jabber-test-reload-mode-map)
        (stale-map (copy-keymap jabber-test-reload-mode-map))
        (new-map (make-sparse-keymap)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq major-mode 'jabber-test-reload-mode)
            (use-local-map stale-map)
            (should-not
             (jabber-reload--capture-buffer-maps
              '(jabber-test-reload-mode-map))))
          (with-temp-buffer
            (setq major-mode 'jabber-test-reload-mode)
            (use-local-map old-map)
            (let ((buffers
                   (jabber-reload--capture-buffer-maps
                    '(jabber-test-reload-mode-map))))
              (setq jabber-test-reload-mode-map new-map)
              (jabber-reload--set-buffer-maps buffers nil)
              (should (eq (current-local-map) new-map))
              (should-not (eq (current-local-map) old-map)))))
      (setq jabber-test-reload-mode-map old-map))))

(provide 'jabber-test-reload)
;;; jabber-test-reload.el ends here
