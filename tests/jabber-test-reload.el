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
    (let* ((dependency-paths
            (mapcar
             (lambda (library)
               (file-name-directory
                (or (locate-library library)
                    (error "Cannot locate dependency: %s" library))))
             '("fsm" "keymap-popup")))
           (arguments
            (append
             '("-Q" "--batch")
             (mapcan (lambda (path) (list "-L" path)) dependency-paths)
             (list "-L" (expand-file-name "lisp" jabber-test-reload--root)
                   "--eval" "(setq load-prefer-newer t)"
                   "--eval" (prin1-to-string form))))
           (status
            (apply #'call-process
                   (expand-file-name invocation-name invocation-directory)
                   nil (current-buffer) nil arguments)))
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
  (skip-unless
   (file-readable-p
    (expand-file-name "lisp/jabber-autoloads.el"
                      jabber-test-reload--root)))
  (let ((autoloads (jabber-test-reload--generated-autoloads)))
    (dolist (function '(jabber-muc-get-buffer
                        jabber-message-thread-browse
                        jabber-omemo-show-fingerprints
                        jabber-roster-popup))
      (should (memq function autoloads)))
    (dolist (function '(jabber-chat--insert-backlog-chunked
                        jabber-omemo--send-chat))
      (should-not (memq function autoloads)))))

(ert-deftest jabber-test-reload-orders-styling-after-chat ()
  "Load styling only after the chat printer chain is initialized."
  (let ((files (plist-get (jabber-reload--plan jabber-test-reload--root)
                          :files)))
    (should (< (jabber-test-reload--position "jabber-chat.el" files)
               (jabber-test-reload--position "jabber-styling.el" files)))))

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

(ert-deftest jabber-test-reload-popup-source-preserves-bindings ()
  "Reload guarded launchers and shared maps without losing custom keys."
  (dolist (case '((jabber-bookmarks jabber-bookmarks-mode-map "a")
                  (jabber-omemo-trust jabber-omemo-trust-mode-map "t")
                  (jabber-keymap jabber-common-keymap "C-c C-i")
                  (jabber-keymap jabber-global-keymap "C-c")
                  (jabber-chat-commands jabber-chat-mode-map "RET")
                  (jabber-chat jabber-chat-url-keymap "+")))
    (jabber-test-reload--clean-emacs-eval
     `(progn
        (setq jabber-db-path nil)
        (require ',(car case))
        (let ((map ,(cadr case)))
          (keymap-set map ,(caddr case) #'ignore)
          (keymap-set map "z" #'ignore)
          (keymap-set map "h" #'ignore)
          (define-key ctl-x-map (kbd "C-j") #'ignore)
          (load ,(expand-file-name (format "lisp/%s.el" (car case))
                                 jabber-test-reload--root)
              nil t t)
          (unless (and (eq map ,(cadr case))
                       (eq (keymap-lookup map ,(caddr case)) 'ignore)
                       (eq (keymap-lookup map "z") 'ignore)
                       (eq (keymap-lookup map "h") 'ignore)
                       (eq (lookup-key ctl-x-map (kbd "C-j")) 'ignore))
            (error "Source reload lost custom bindings: %S" ',case)))))))

(ert-deftest jabber-test-reload-popup-supported-loader-preserves-bindings ()
  "Keep actual source maps and their live buffer owners on full reload."
  (jabber-test-reload--clean-emacs-eval
   `(progn
      (setq jabber-db-path nil)
      (require 'jabber)
      (require 'jabber-omemo-trust)
      (load-file ,(expand-file-name "admin/jabber-reload.el"
                                   jabber-test-reload--root))
      (let* ((symbols '(jabber-bookmarks-mode-map jabber-omemo-trust-mode-map
                        jabber-common-keymap jabber-global-keymap
                        jabber-chat-mode-map jabber-chat-url-keymap))
             (maps (mapcar #'symbol-value symbols)))
        (dolist (map maps)
          (keymap-set map "h" #'ignore)
          (keymap-set map "z" #'ignore))
        (keymap-set jabber-common-keymap "C-c C-i" #'ignore)
        (keymap-set jabber-global-keymap "C-c" #'ignore)
        (keymap-set jabber-chat-mode-map "RET" #'ignore)
        (keymap-set jabber-chat-url-keymap "+" #'ignore)
        (define-key ctl-x-map (kbd "C-j") #'ignore)
        (with-temp-buffer
          (jabber-bookmarks-mode)
          (let ((owner (current-local-map)))
            (jabber-reload ,jabber-test-reload--root)
            (unless (eq owner (current-local-map))
              (error "Reload replaced the live buffer map"))))
        (cl-mapc
         (lambda (symbol map)
           (unless (and (eq (symbol-value symbol) map)
                        (eq (keymap-lookup map "h") 'ignore)
                        (eq (keymap-lookup map "z") 'ignore))
             (error "Reload lost map identity or bindings: %s" symbol)))
         symbols maps)
        (dolist (entry `((,jabber-common-keymap . "C-c C-i")
                         (,jabber-global-keymap . "C-c")
                         (,jabber-chat-mode-map . "RET")
                         (,jabber-chat-url-keymap . "+")
                         (,ctl-x-map . "C-j")))
          (unless (eq (keymap-lookup (car entry) (cdr entry)) 'ignore)
            (error "Reload overwrote action binding: %s" (cdr entry))))))))

(ert-deftest jabber-test-reload-popup-thread-title-preserves-bindings ()
  "Retain title-menu identity and custom keys through both reload paths."
  (jabber-test-reload--clean-emacs-eval
   `(progn
      (setq jabber-db-path nil)
      (require 'jabber-chat-commands)
      (load-file ,(expand-file-name "admin/jabber-reload.el"
                                   jabber-test-reload--root))
      (let ((map jabber-chat-operations-menu-map))
        (dolist (binding (list #'ignore nil "xy"
                               '(menu-item "Custom" forward-char)
                               (make-sparse-keymap)))
          (define-key map (kbd "L") binding)
          (keymap-set map "z" #'ignore)
          (keymap-set map "h" #'ignore)
          (dolist (stage '(source supported))
            (if (eq stage 'source)
                (load ,(expand-file-name "lisp/jabber-chat-commands.el"
                                         jabber-test-reload--root) nil t t)
              (jabber-reload ,jabber-test-reload--root))
            (unless (and (eq map jabber-chat-operations-menu-map)
                         (equal (cdr (assq ?L (cdr map))) binding)
                         (eq (keymap-lookup map "z") 'ignore)
                         (eq (keymap-lookup map "h") 'ignore))
              (error "Reload overwrote custom thread-menu keys: %S" stage))))))))

(ert-deftest jabber-test-reload-popup-in-place-rollback ()
  "Restore retained maps and nested prefixes after error or quit."
  (dolist (condition '(error quit))
    (let* ((jabber-test-reload-mode-map (make-sparse-keymap))
           (map jabber-test-reload-mode-map)
           (prefix (make-sparse-keymap)))
      (keymap-set map "x" prefix)
      (keymap-set prefix "a" #'ignore)
      (cl-letf (((symbol-function 'jabber-reload--plan)
                 (lambda (_root)
                   '(:files ("bad.el") :maps (jabber-test-reload-mode-map))))
                ((symbol-function 'load-file)
                 (lambda (_file)
                   (keymap-set map "z" #'ignore)
                   (keymap-set prefix "a" #'forward-char)
                   (signal condition '("Interrupted reload")))))
        (should (eq (condition-case caught
                        (jabber-reload jabber-test-reload--root)
                      ((error quit) (car caught)))
                    condition)))
      (should (eq jabber-test-reload-mode-map map))
      (should (eq (keymap-lookup map "x") prefix))
      (should (eq (keymap-lookup prefix "a") #'ignore))
      (should-not (keymap-lookup map "z")))))

(ert-deftest jabber-test-reload-popup-native-help ()
  "Open guarded help with native h and ? after repeated source loading."
  (dolist (case '((jabber-bookmarks jabber-bookmarks-mode)
                  (jabber-omemo-trust jabber-omemo-trust-mode)))
    (jabber-test-reload--clean-emacs-eval
     `(progn
        (setq jabber-db-path nil)
        (require ',(car case))
        (load ,(expand-file-name (format "lisp/%s.el" (car case))
                                 jabber-test-reload--root)
              nil t t)
        (save-window-excursion
          (with-temp-buffer
            (switch-to-buffer (current-buffer))
            (,(cadr case))
            ;; Inspect the displayed native window; older supported popup
            ;; releases have neither --popup-buffer nor a dismiss function.
            (let ((owner (current-buffer)))
              (execute-kbd-macro (kbd "h"))
              (unless (and (get-buffer-window "*keymap-popup*")
                           (eq (current-buffer) owner))
                (error "Help did not open in its source buffer"))
              (execute-kbd-macro (kbd "q"))
              (when (get-buffer-window "*keymap-popup*")
                (error "Help did not dismiss"))
              (execute-kbd-macro (kbd "?"))
              (unless (get-buffer-window "*keymap-popup*")
                (error "Help did not reopen"))
              (condition-case nil (execute-kbd-macro (kbd "C-g"))
                (quit nil))
              (when (or (get-buffer-window "*keymap-popup*")
                        overriding-terminal-local-map)
                (error "Help retained its window or input map")))))))))

(ert-deftest jabber-test-reload-popup-trust-caption ()
  "Keep peer and account captions through guarded help and both reload paths."
  (jabber-test-reload--clean-emacs-eval
   `(progn
      (setq jabber-db-path nil)
      (require 'jabber-omemo-trust)
      (load-file ,(expand-file-name "admin/jabber-reload.el"
                                   jabber-test-reload--root))
      (save-window-excursion
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (jabber-omemo-trust-mode)
          (setq-local jabber-omemo-trust--peer "p@x"
                      jabber-omemo-trust--account "a@x")
          (let ((owner (current-buffer))
                (map (current-local-map)))
            (dolist (stage '(initial source supported))
              (pcase stage
                ('source
                 (load ,(expand-file-name "lisp/jabber-omemo-trust.el"
                                          jabber-test-reload--root) nil t t))
                ('supported (jabber-reload ,jabber-test-reload--root)))
              (unless (eq map (current-local-map))
                (error "Reload replaced the trust map"))
              (execute-kbd-macro (kbd "h"))
              (let ((window (get-buffer-window "*keymap-popup*")))
                (unless (and window (eq owner (current-buffer)))
                  (error "Help did not open in its source buffer"))
                ;; Batch Emacs does not format mode lines.  Evaluate their
                ;; native :eval slots and inspect the renderer's payloads.
                (with-current-buffer (window-buffer window)
                  (let ((rendered
                         (format "%s %s" (buffer-string)
                                 (mapcar (lambda (item)
                                           (if (eq (car-safe item) :eval)
                                               (eval (cadr item) t)
                                             item))
                                         (append header-line-format
                                                 mode-line-format)))))
                    (unless (and (string-match-p "Peer:.*p@x" rendered)
                                 (string-match-p "Account:.*a@x" rendered))
                      (error "Help lost its peer/account caption: %s" stage)))))
              (execute-kbd-macro (kbd "q"))
              (when (or (get-buffer-window "*keymap-popup*")
                        overriding-terminal-local-map)
                (error "Help retained its window or input map")))))))))

(defun jabber-test-reload--captured-filter (prefix)
  "Return a menu filter retaining PREFIX."
  (lambda (_) prefix))

(ert-deftest jabber-test-reload-popup-captured-prefix-rollback ()
  "Restore source and compiled filter captures without evaluating filters."
  (dolist (condition '(error quit))
    (dolist (representation '(source byte factory
                              named-source alias-source named-byte alias-byte))
      (let* ((root (make-temp-file "jabber-reload-filter-" t))
             (jabber-test-reload-mode-map (make-sparse-keymap))
             (map jabber-test-reload-mode-map)
             (prefix (make-sparse-keymap))
             (parent (make-sparse-keymap))
             (calls (list 0))
             (source (eval '(lambda (_)
                             (setcar calls (1+ (car calls)))
                             prefix)
                           (list (cons 'prefix prefix) (cons 'calls calls))))
             (callable (pcase representation
                         ((or 'byte 'named-byte 'alias-byte) (byte-compile source))
                         ('factory (jabber-test-reload--captured-filter prefix))
                         (_ source)))
             (name (make-symbol "user-filter"))
             (alias (make-symbol "user-filter-alias"))
             (_ (fset name callable))
             (_ (fset alias name))
             (filter (pcase representation
                       ((or 'named-source 'named-byte) name)
                       ((or 'alias-source 'alias-byte) alias)
                       (_ callable)))
             (item (list 'menu-item "User prefix" nil :filter filter)))
        (unwind-protect
            (progn
              (make-directory (expand-file-name "lisp" root))
              (with-temp-file (expand-file-name "lisp/jabber-fixture.el" root)
                (insert ";;; -*- lexical-binding: t; -*-\n"
                        "(defvar jabber-test-reload-mode-map)\n")
                (prin1 '(define-key (lookup-key jabber-test-reload-mode-map "X")
                                    "a" #'forward-char)
                       (current-buffer))
                (prin1 '(define-key
                         (keymap-parent (lookup-key jabber-test-reload-mode-map "X"))
                         "p" #'forward-char)
                       (current-buffer))
                (prin1 `(signal ',condition '("Interrupted reload"))
                       (current-buffer)))
              (define-key prefix "a" #'ignore)
              (define-key parent "p" #'ignore)
              (set-keymap-parent prefix parent)
              (define-key map "X" item)
              (setq item (cdr (assq ?X (cdr map))))
              ;; Discovery must not invoke the filter, even once.
              (jabber-reload--snapshot-map-cells
               '(jabber-test-reload-mode-map))
              (should (zerop (car calls)))
              (with-temp-buffer
                (use-local-map map)
                (should (eq (key-binding "Xa") #'ignore))
                (should (eq condition
                            (condition-case caught (jabber-reload root)
                              ((error quit) (car caught)))))
                (should (eq (key-binding "Xa") #'ignore)))
              (should (eq jabber-test-reload-mode-map map))
              (should (eq (cdr (assq ?X (cdr map))) item))
              (should (eq (nth 4 item) filter))
              (should (eq (funcall filter nil) prefix))
              (should (eq (symbol-function name) callable))
              (should (eq (symbol-function alias) name))
              (should (eq (keymap-parent prefix) parent))
              (should (eq (lookup-key parent "p") #'ignore)))
          (delete-directory root t))))))

(ert-deftest jabber-test-reload-popup-filter-callable-boundary ()
  "Follow filter aliases without following arbitrary captured symbol cells."
  (let* ((jabber-test-reload-mode-map (make-sparse-keymap))
         (prefix (make-sparse-keymap))
         (unrelated (make-symbol "user-data"))
         (hidden-prefix (make-sparse-keymap))
         (name (make-symbol "user-filter"))
         (alias (make-symbol "user-filter-alias"))
         (filter (eval '(lambda (_) (ignore unrelated) prefix)
                       (list (cons 'unrelated unrelated) (cons 'prefix prefix)))))
    (fset unrelated (jabber-test-reload--captured-filter hidden-prefix))
    (fset name filter)
    (fset alias name)
    (define-key jabber-test-reload-mode-map "X"
                (list 'menu-item "User prefix" nil :filter alias))
    (let ((cells (jabber-reload--snapshot-map-cells
                  '(jabber-test-reload-mode-map))))
      (should (assq name cells))
      (should (assq alias cells))
      (should (assq prefix cells))
      (should-not (assq unrelated cells))
      (should-not (assq hidden-prefix cells))
      (fset name #'ignore)
      (fset alias #'ignore)
      (jabber-reload--restore-map-cells cells)
      (should (eq (symbol-function name) filter))
      (should (eq (symbol-function alias) name))
      (should (eq (lookup-key jabber-test-reload-mode-map "X") prefix)))))

(ert-deftest jabber-test-reload-popup-symbol-prefix-rollback ()
  "Restore a reachable external prefix function cell after error or quit."
  (dolist (condition '(error quit))
    (let* ((root (make-temp-file "jabber-reload-symbol-" t))
           (jabber-test-reload-mode-map (make-sparse-keymap))
           (prefix (make-sparse-keymap))
           (symbol (make-symbol "user-prefix"))
           (alias (make-symbol "user-prefix-alias")))
      (unwind-protect
          (progn
            (fset symbol prefix)
            (fset alias symbol)
            (define-key prefix "a" #'ignore)
            (define-key jabber-test-reload-mode-map "X" alias)
            (make-directory (expand-file-name "lisp" root))
            (with-temp-file (expand-file-name "lisp/jabber-fixture.el" root)
              (insert ";;; -*- lexical-binding: t; -*-\n"
                        "(defvar jabber-test-reload-mode-map)\n")
              (prin1 '(fset (cdr (assq ?X (cdr jabber-test-reload-mode-map)))
                            (make-sparse-keymap)) (current-buffer))
              (prin1 `(signal ',condition '("Interrupted reload"))
                     (current-buffer)))
            (should (eq condition
                        (condition-case caught (jabber-reload root)
                          ((error quit) (car caught)))))
            (should (eq (symbol-function alias) symbol))
            (should (eq (symbol-function symbol) prefix))
            (should (eq (lookup-key jabber-test-reload-mode-map "Xa") #'ignore)))
        (delete-directory root t)))))

(defun jabber-test-reload--fault (forms condition)
  "Load FORMS through the real planner, then signal CONDITION."
  (let* ((root (make-temp-file "jabber-reload-fault-" t))
         (directory (expand-file-name "lisp" root)))
    (unwind-protect
        (progn
          (make-directory directory)
          (with-temp-file (expand-file-name "jabber-fixture.el" directory)
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(defvar jabber-test-reload-mode-map)\n"
                    "(defvar jabber-test-reload--container)\n")
            (dolist (form forms)
              (prin1 form (current-buffer))
              (insert "\n"))
            (prin1 `(signal ',condition '("Rollback fixture"))
                   (current-buffer)))
          (should (equal (condition-case caught (jabber-reload root)
                           ((error quit) caught))
                         (list condition "Rollback fixture"))))
      (delete-directory root t))))

(defvar jabber-test-reload--container)

(ert-deftest jabber-test-reload-full-map-default-rollback ()
  "Restore a full map's default without installing stale explicit values."
  (dolist (condition '(error quit))
    (dolist (default '(nil ignore))
      (let* ((jabber-test-reload-mode-map (make-keymap))
             (map jabber-test-reload-mode-map)
             (table (cadr map)))
        (set-char-table-range table nil default)
        (jabber-test-reload--fault
         '((set-char-table-range (cadr jabber-test-reload-mode-map)
                                nil #'forward-char)
           (define-key jabber-test-reload-mode-map "z" #'forward-char))
         condition)
        (should (eq jabber-test-reload-mode-map map))
        (should (eq (cadr map) table))
        (should (eq (char-table-range table nil) default))
        (should (eq (lookup-key map "a") default))
        ;; A restored default must remain authoritative, not be shadowed by
        ;; effective values materialized as local character bindings.
        (set-char-table-range table nil #'backward-char)
        (should (eq (lookup-key map "a") #'backward-char))
        (should (eq (lookup-key map "z") #'backward-char))))))

(ert-deftest jabber-test-reload-full-map-parent-rollback ()
  "Restore parent links and parent-owned cells with exact shared identity."
  (dolist (condition '(error quit))
    (dolist (replacement '(nil new))
      (let* ((jabber-test-reload-mode-map (make-keymap))
             (map jabber-test-reload-mode-map)
             (table (cadr map))
             (parent (make-char-table 'keymap))
             (grandparent (make-char-table 'keymap)))
        (set-char-table-parent table parent)
        (set-char-table-parent parent grandparent)
        (set-char-table-range parent ?a #'ignore)
        (set-char-table-range grandparent ?b #'ignore)
        (set-char-table-range table '(?c . ?e) #'backward-char)
        (jabber-test-reload--fault
         `((let* ((table (cadr jabber-test-reload-mode-map))
                  (parent (char-table-parent table)))
             (set-char-table-range (char-table-parent parent) ?b #'forward-char)
             (set-char-table-range parent ?a #'forward-char)
             (set-char-table-parent parent nil)
             (set-char-table-parent table
                                    ,(and replacement '(make-char-table 'keymap)))
             (set-char-table-range table '(?c . ?e) nil)))
         condition)
        (should (eq jabber-test-reload-mode-map map))
        (should (eq (cadr map) table))
        (should (eq (char-table-parent table) parent))
        (should (eq (char-table-parent parent) grandparent))
        (should (eq (char-table-range parent ?a) #'ignore))
        (should (eq (char-table-range grandparent ?b) #'ignore))
        (should (eq (lookup-key map "d") #'backward-char))
        (set-char-table-range parent ?a #'forward-char)
        (set-char-table-range grandparent ?b #'forward-char)
        (should (eq (lookup-key map "a") #'forward-char))
        (should (eq (lookup-key map "b") #'forward-char))))))

(defvar jabber-test-reload-child-map)

(ert-deftest jabber-test-reload-full-map-reversed-parent-rollback ()
  "Restore a legally reversed parent edge without transient parent cycles."
  (dolist (condition '(error quit))
    ;; The planner lists the parent first; rollback therefore visits its
    ;; child first.  Retain both map objects and their actual native tables.
    (let* ((jabber-test-reload-mode-map (make-keymap))
           (jabber-test-reload-child-map (make-keymap))
           (parent (cadr jabber-test-reload-mode-map))
           (child (cadr jabber-test-reload-child-map)))
      (set-char-table-range parent ?a #'ignore)
      (set-char-table-parent child parent)
      (jabber-test-reload--fault
       '((defvar jabber-test-reload-child-map)
         (set-char-table-parent (cadr jabber-test-reload-child-map) nil)
         (set-char-table-parent (cadr jabber-test-reload-mode-map)
                                (cadr jabber-test-reload-child-map)))
       condition)
      (should (eq (cadr jabber-test-reload-mode-map) parent))
      (should (eq (cadr jabber-test-reload-child-map) child))
      (should (eq (char-table-parent child) parent))
      (should-not (char-table-parent parent))
      (set-char-table-range parent ?a #'forward-char)
      (should (eq (lookup-key jabber-test-reload-child-map "a") #'forward-char)))))

(ert-deftest jabber-test-reload-full-map-parent-sharing-after-unrelated-fault ()
  "Keep inherited bindings dynamic after an unrelated failed define-key."
  (dolist (condition '(error quit))
    (let* ((jabber-test-reload-mode-map (make-keymap))
           (map jabber-test-reload-mode-map)
           (parent (make-char-table 'keymap)))
      (set-char-table-parent (cadr map) parent)
      (set-char-table-range parent ?a #'ignore)
      (jabber-test-reload--fault
       '((define-key jabber-test-reload-mode-map "z" #'ignore)) condition)
      (should (eq (char-table-parent (cadr map)) parent))
      (should-not (lookup-key map "z"))
      (set-char-table-range parent ?a #'forward-char)
      (should (eq (lookup-key map "a") #'forward-char))
      (set-char-table-range parent nil #'backward-char)
      (should (eq (lookup-key map "b") #'backward-char)))))

(ert-deftest jabber-test-reload-captured-char-table-extra-slots-rollback ()
  "Restore captured extra slots and their retained mutable values in place."
  (dolist (condition '(error quit))
    (let* ((jabber-test-reload-mode-map (make-sparse-keymap))
           (purpose (make-symbol "jabber-test-table"))
           (_ (put purpose 'char-table-extra-slots 2))
           (jabber-test-reload--container (make-char-table purpose))
           (value (vector 'old))
           ;; Capture the table lexically, not through its special variable.
           (filter (eval '(lambda (_) (ignore table) nil)
                         (list (cons 'table jabber-test-reload--container)))))
      (set-char-table-extra-slot jabber-test-reload--container 0 value)
      (set-char-table-extra-slot jabber-test-reload--container 1 'old)
      (define-key jabber-test-reload-mode-map "X"
                  (list 'menu-item "Captured table" nil :filter filter))
      (jabber-test-reload--fault
       '((aset (char-table-extra-slot jabber-test-reload--container 0) 0 'new)
         (set-char-table-extra-slot jabber-test-reload--container 0 [new])
         (set-char-table-extra-slot jabber-test-reload--container 1 'new))
       condition)
      (should (eq (char-table-extra-slot jabber-test-reload--container 0) value))
      (should (eq (aref value 0) 'old))
      (should (eq (char-table-extra-slot jabber-test-reload--container 1) 'old)))))

(ert-deftest jabber-test-reload-hash-captured-prefix-rollback ()
  "Restore hash captures and entries without invoking source or byte filters."
  (dolist (condition '(error quit))
    (dolist (representation '(source byte))
      (dolist (test '(eq equal))
        (let* ((jabber-test-reload-mode-map (make-sparse-keymap))
               (map jabber-test-reload-mode-map)
               (prefix (make-sparse-keymap))
               (key-map (make-sparse-keymap))
               (jabber-test-reload--container (make-hash-table :test test))
               (calls (list 0))
               (source (eval '(lambda (_)
                               (setcar calls (1+ (car calls)))
                               (gethash 'prefix table))
                             (list (cons 'table jabber-test-reload--container)
                                   (cons 'calls calls))))
               (filter (if (eq representation 'byte) (byte-compile source) source)))
          (define-key prefix "a" #'ignore)
          (define-key key-map "k" #'ignore)
          (puthash 'prefix prefix jabber-test-reload--container)
          (puthash key-map 'key-owned jabber-test-reload--container)
          (puthash 'self jabber-test-reload--container jabber-test-reload--container)
          (puthash 'removed nil jabber-test-reload--container)
          (define-key map "X" (list 'menu-item "Hash prefix" nil :filter filter))
          (puthash map 'owner jabber-test-reload--container)
          (jabber-reload--snapshot-map-cells '(jabber-test-reload-mode-map))
          (should (zerop (car calls)))
          (should (eq (lookup-key map "Xa") #'ignore))
          (jabber-test-reload--fault
           '((define-key (lookup-key jabber-test-reload-mode-map "X")
                         "a" #'forward-char)
             (maphash (lambda (key _) (when (keymapp key)
                                       (define-key key "k" #'forward-char)))
                      jabber-test-reload--container)
             (puthash 'prefix (make-sparse-keymap) jabber-test-reload--container)
             (define-key jabber-test-reload-mode-map "z" #'ignore)
             (remhash 'removed jabber-test-reload--container)
             (puthash 'new t jabber-test-reload--container))
           condition)
          (should (eq jabber-test-reload-mode-map map))
          (should (eq (gethash 'prefix jabber-test-reload--container) prefix))
          (should (eq (lookup-key map "X") prefix))
          (should (eq (lookup-key map "Xa") #'ignore))
          (should (eq (lookup-key key-map "k") #'ignore))
          (should (= (hash-table-count jabber-test-reload--container) 5))
          (should (eq (gethash map jabber-test-reload--container) 'owner))
          (should (eq (gethash 'removed jabber-test-reload--container 'absent) nil))
          (should (eq (gethash 'self jabber-test-reload--container)
                      jabber-test-reload--container))
          (should-not (gethash 'new jabber-test-reload--container)))))))

(ert-deftest jabber-test-reload-guarded-map-plan ()
  "Keep first-initialization map declarations visible to the real planner."
  (let ((root (make-temp-file "jabber-reload-plan-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "lisp" root))
          (with-temp-file (expand-file-name "lisp/jabber-fixture.el" root)
            (prin1 '(unless (boundp 'jabber-test-reload-mode-map)
                      (defvar-keymap jabber-test-reload-mode-map))
                   (current-buffer)))
          (should (memq 'jabber-test-reload-mode-map
                        (plist-get (jabber-reload--plan root) :maps))))
      (delete-directory root t))))

(ert-deftest jabber-test-reload-popup-all-initializers-preserve-help ()
  "Retain every popup's help, descriptions and identity on both reload paths."
  (jabber-test-reload--clean-emacs-eval
   `(progn
      (setq jabber-db-path nil)
      (require 'jabber)
      (require 'jabber-omemo-trust)
      (require 'jabber-ahc)
      (require 'jabber-vcard)
      (load-file ,(expand-file-name "admin/jabber-reload.el"
                                   jabber-test-reload--root))
      (let* ((cases '((jabber-ahc-command-list-map . jabber-ahc)
                      (jabber-bookmarks-edit-map . jabber-bookmarks)
                      (jabber-chat-encryption-menu-map . jabber-chat-commands)
                      (jabber-chat-operations-menu-map . jabber-chat-commands)
                      (jabber-info-menu-map . jabber-disco-menu)
                      (jabber-service-menu-map . jabber-disco-menu)
                      (jabber-muc-menu-map . jabber-muc-menu)
                      (jabber-omemo-trust-mode-map . jabber-omemo-trust)
                      (jabber-roster-presence-map . jabber-roster-menu)
                      (jabber-roster-discovery-map . jabber-roster-menu)
                      (jabber-roster-contact-action-map . jabber-roster-menu)
                      (jabber-roster-popup-map . jabber-roster-menu)
                      (jabber-roster-account-action-map . jabber-roster-menu)
                      (jabber-vcard-edit-mode-map . jabber-vcard)
                      (jabber-common-keymap . jabber-keymap)
                      (jabber-global-keymap . jabber-keymap)
                      (jabber-bookmarks-mode-map . jabber-bookmarks)))
             (custom (lambda () (interactive) (insert "CUSTOM")))
             (maps (mapcar (lambda (case) (symbol-value (car case))) cases)))
        (dolist (map maps)
          (define-key map "h" custom)
          (define-key map "?" custom)
          (keymap-popup-annotate map ignore "Retained caption"))
        (let ((contents (mapcar (lambda (map) (copy-tree map t)) maps)))
          (dolist (stage '(source supported))
            (if (eq stage 'source)
                (dolist (feature (delete-dups (mapcar #'cdr cases)))
                  (load (expand-file-name (format "lisp/%s.el" feature)
                                          ,jabber-test-reload--root) nil t t))
              (jabber-reload ,jabber-test-reload--root))
            (cl-mapc
             (lambda (case map content)
               (unless (and (eq map (symbol-value (car case)))
                            (equal map content)
                            (eq (lookup-key map "h") custom)
                            (eq (lookup-key map "?") custom))
                 (error "Reload changed retained popup: %s/%s" (car case) stage))
               (save-window-excursion
                 (with-temp-buffer
                   (switch-to-buffer (current-buffer))
                   (use-local-map map)
                   (execute-kbd-macro "h?")
                   (unless (equal (buffer-string) "CUSTOMCUSTOM")
                     (error "Retained help failed native dispatch: %s" (car case))))))
             cases maps contents)))))))

(ert-deftest jabber-test-reload-popup-predecessor-feature-boundary ()
  "Adopt an initialized predecessor chat feature without a new sentinel."
  (jabber-test-reload--clean-emacs-eval
   `(progn
      (setq jabber-db-path nil)
      (require 'jabber-chat-commands)
      ;; The predecessor provided the feature but never defined this sentinel.
      (when (boundp 'jabber-chat-commands--initialized-map)
        (makunbound 'jabber-chat-commands--initialized-map))
      (let ((map jabber-chat-mode-map)
            (custom (lambda () (interactive) (insert "CUSTOM"))))
        (define-key map (kbd "RET") custom)
        (define-key map (kbd "C-c C-t") custom)
        (load ,(expand-file-name "lisp/jabber-chat-commands.el"
                                 jabber-test-reload--root) nil t t)
        (unless (and (eq map jabber-chat-mode-map)
                     (eq (lookup-key map (kbd "RET")) custom)
                     (eq (lookup-key map (kbd "C-c C-t")) custom))
          (error "First feature upgrade overwrote predecessor actions"))
        (save-window-excursion
          (with-temp-buffer
            (switch-to-buffer (current-buffer))
            (use-local-map map)
            (execute-kbd-macro (kbd "RET C-c C-t"))
            (unless (equal (buffer-string) "CUSTOMCUSTOM")
              (error "Predecessor actions failed native dispatch"))))))))

(ert-deftest jabber-test-reload-owned-macro-string-rollback ()
  "Restore owned macro contents and properties before real native dispatch."
  (dolist (condition '(error quit))
    (let* ((jabber-test-reload-mode-map (make-sparse-keymap))
           (macro (copy-sequence "xy"))
           (property (copy-sequence "label")))
      (put-text-property 0 1 'jabber-label property macro)
      (define-key jabber-test-reload-mode-map "a" macro)
      (jabber-test-reload--fault
       '((let ((macro (cdr (assq ?a (cdr jabber-test-reload-mode-map)))))
           (aset macro 0 ?z)
           (aset (get-text-property 0 'jabber-label macro) 0 ?X)
           (set-text-properties 0 2 '(face error) macro)))
       condition)
      (should (eq (lookup-key jabber-test-reload-mode-map "a") macro))
      (should (equal macro "xy"))
      (should (eq (get-text-property 0 'jabber-label macro) property))
      (should (equal property "label"))
      (should-not (get-text-property 1 'jabber-label macro))
      (should-not (get-text-property 0 'face macro))
      (save-window-excursion
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (use-local-map jabber-test-reload-mode-map)
          (execute-kbd-macro "a")
          (should (equal (buffer-string) "xy")))))))

(ert-deftest jabber-test-reload-owned-string-hash-key-rollback ()
  "Restore captured string keys in place before rebuilding equal hash buckets."
  (dolist (condition '(error quit))
    (dolist (representation '(source byte))
      (let* ((jabber-test-reload-mode-map (make-sparse-keymap))
             (map jabber-test-reload-mode-map)
             (jabber-test-reload--container (make-hash-table :test #'equal))
             (table jabber-test-reload--container)
             (key (copy-sequence "prefix"))
             (prefix (make-sparse-keymap))
             (calls (list 0))
             (source (eval '(lambda (_)
                             (setcar calls (1+ (car calls)))
                             (gethash "prefix" table))
                           (list (cons 'table table) (cons 'calls calls))))
             (filter (if (eq representation 'byte) (byte-compile source) source)))
        (put-text-property 1 3 'face 'bold key)
        (define-key prefix "a" #'ignore)
        (puthash key prefix table)
        (define-key map "X" (list 'menu-item "Captured" nil :filter filter))
        (jabber-reload--snapshot-map-cells '(jabber-test-reload-mode-map))
        (should (zerop (car calls)))
        (should (eq (lookup-key map "Xa") #'ignore))
        (jabber-test-reload--fault
         '((maphash (lambda (key _value)
                      (aset key 0 ?X)
                      (set-text-properties 0 (length key) nil key))
                    jabber-test-reload--container))
         condition)
        (should (eq jabber-test-reload-mode-map map))
        (should (eq jabber-test-reload--container table))
        (should (eq (gethash "prefix" table) prefix))
        (let (actual-key)
          (maphash (lambda (stored-key _) (setq actual-key stored-key)) table)
          (should (eq actual-key key)))
        (should (equal key "prefix"))
        (should (eq (get-text-property 1 'face key) 'bold))
        (should-not (get-text-property 0 'face key))
        (should (eq (lookup-key map "Xa") #'ignore))))))

(defvar jabber-test-reload--mutation-completed)

(defun jabber-test-reload--mixed-string-fault (kind condition representation)
  "Restore mixed strings after KIND mutation and CONDITION in REPRESENTATION."
  (let* ((root (make-temp-file "jabber-reload-mixed-" t))
         (jabber-test-reload-mode-map (make-sparse-keymap))
         (map jabber-test-reload-mode-map)
         (macro (copy-sequence "xyαβ"))
         (label (copy-sequence "laλμ"))
         (key (copy-sequence "aα-prefix"))
         (prefix (make-sparse-keymap))
         (command (lambda () (interactive) (insert "PREFIX")))
         (jabber-test-reload--container (make-hash-table :test #'equal))
         (table jabber-test-reload--container)
         (source (eval '(lambda (_) (gethash "aα-prefix" table))
                       (list (cons 'table table))))
         (filter (if (eq representation 'byte) (byte-compile source) source))
         (jabber-test-reload--mutation-completed nil)
         caught)
    (unwind-protect
        (progn
          (put-text-property 0 1 'jabber-label label macro)
          (put-text-property 2 4 'face 'bold macro)
          (put-text-property 1 3 'face 'italic key)
          (put-text-property 2 4 'face 'underline label)
          (define-key map "a" macro)
          (define-key prefix "p" command)
          (puthash key prefix table)
          (define-key map "X" (list 'menu-item "Captured" nil :filter filter))
          (let ((saved (mapcar #'copy-sequence (list macro label key))))
            (make-directory (expand-file-name "lisp" root))
            (with-temp-file (expand-file-name "lisp/jabber-fixture.el" root)
              (insert ";;; -*- lexical-binding: t; -*-\n"
                      "(defvar jabber-test-reload-mode-map)\n"
                      "(defvar jabber-test-reload--container)\n"
                      "(defvar jabber-test-reload--mutation-completed)\n")
              (prin1
               (pcase kind
                 ('macro
                  '(let ((value (cdr (assq ?a (cdr jabber-test-reload-mode-map)))))
                     (aset value 0 ?z)
                     (set-text-properties 0 (length value) '(face error) value)))
                 ('property-value
                  '(aset (get-text-property
                          0 'jabber-label
                          (cdr (assq ?a (cdr jabber-test-reload-mode-map)))) 0 ?X))
                 ('property-only
                  '(let ((value (cdr (assq ?a (cdr jabber-test-reload-mode-map)))))
                     (set-text-properties 0 (length value) '(face error) value)))
                 ('hash-key
                  '(progn
                     (maphash (lambda (value _)
                                (aset value 0 ?X)
                                (set-text-properties 0 (length value)
                                                     '(face error) value))
                              jabber-test-reload--container)
                     (puthash 'introduced t jabber-test-reload--container))))
               (current-buffer))
              (prin1 '(setq jabber-test-reload--mutation-completed t)
                     (current-buffer))
              (prin1 `(signal ',condition '("Mixed rollback fixture" 991))
                     (current-buffer)))
            (setq caught (condition-case data (jabber-reload root)
                           ((error quit) data)))
            ;; Non-ASCII writes can be forbidden by the runtime.  This fixture
            ;; changes only ASCII (or properties), and must reach its own fault.
            (should jabber-test-reload--mutation-completed)
            (should (equal caught (list condition "Mixed rollback fixture" 991)))
            (should (eq jabber-test-reload-mode-map map))
            (should (eq (lookup-key map "a") macro))
            (should (eq (get-text-property 0 'jabber-label macro) label))
            (should (cl-every #'equal-including-properties
                              saved (list macro label key)))
            (should (eq jabber-test-reload--container table))
            (should (= (hash-table-count table) 1))
            (should (eq (gethash "aα-prefix" table) prefix))
            (maphash (lambda (stored-key _) (should (eq stored-key key))) table)
            (should (eq (plist-get (nthcdr 3 (cdr (assq ?X (cdr map)))) :filter)
                        filter))
            (should (eq (lookup-key map "Xp") command))
            (save-window-excursion
              (with-temp-buffer
                (switch-to-buffer (current-buffer))
                (use-local-map map)
                (execute-kbd-macro "aXp")
                (should (equal (buffer-string) "xyαβPREFIX"))))))
      (delete-directory root t))))

(ert-deftest jabber-test-reload-mixed-macro-string-rollback ()
  "Restore mixed native macros after successful ASCII mutation and a fault."
  (dolist (condition '(error quit))
    (jabber-test-reload--mixed-string-fault 'macro condition 'source)))

(ert-deftest jabber-test-reload-mixed-property-value-rollback ()
  "Restore an owned mixed property string without replacing its identity."
  (dolist (condition '(error quit))
    (jabber-test-reload--mixed-string-fault 'property-value condition 'source)))

(ert-deftest jabber-test-reload-mixed-property-only-rollback ()
  "Restore mixed string properties when no character changed."
  (dolist (condition '(error quit))
    (jabber-test-reload--mixed-string-fault 'property-only condition 'source)))

(ert-deftest jabber-test-reload-mixed-string-hash-key-rollback ()
  "Restore mixed equal-hash keys before replaying captured table entries."
  (dolist (condition '(error quit))
    (dolist (representation '(source byte))
      (jabber-test-reload--mixed-string-fault 'hash-key condition representation))))

(provide 'jabber-test-reload)
;;; jabber-test-reload.el ends here
