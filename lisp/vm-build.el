;; Add the current dir to the load-path  -*- lexical-binding: t; -*-
(setq load-path (cons default-directory load-path))

(defun vm-build-minimum-emacs-version (&optional directory)
  "Return the oldest Emacs VM supports, as declared in vm.el.
Read out of the source rather than copied, so there is no third place to keep
in step with `vm-min-emacs-version' and the Package-Requires header.  Returns
nil if vm.el cannot be found or does not say."
  (let ((vm-el (expand-file-name "vm.el" (or directory default-directory))))
    (when (file-readable-p vm-el)
      (with-temp-buffer
	(insert-file-contents vm-el)
	(goto-char (point-min))
	(when (re-search-forward
	       "(defconst[ \t]+vm-min-emacs-version[ \t]+\"\\([0-9.]+\\)\""
	       nil t)
	  (match-string 1))))))

(defun vm-build-check-emacs-version (&optional directory)
  "Signal an error if this Emacs is too old to build VM.
Issue #526: an Emacs too old to run VM will still byte-compile it, mostly
without complaint, and the failure then turns up at run time -- which is how
#524 happened, with an old Emacs first on root's PATH.  vm.el checks the
version when VM starts; this checks it when VM is built, which is where the
wrong Emacs actually gets chosen."
  (let ((minimum (vm-build-minimum-emacs-version directory)))
    (when (and minimum (version< emacs-version minimum))
      (error "VM needs Emacs %s or newer to build; this is Emacs %s"
	     minimum emacs-version))
    minimum))

(vm-build-check-emacs-version)
(setq debug-ignored-errors nil)

(defun vm-fix-cygwin-path (path)
  "If PATH does not exist, try the DOS path instead.
    This handles EmacsW32 path problems when building on cygwin."
  (if (file-exists-p path)
      path
    (let ((dos-path (cond ((and (locate-library "cygwin-mount")
    				(require 'cygwin-mount))
    			   (cygwin-mount-activate)
    			   (cygwin-mount-convert-file-name path))
			  ((string-match "^/cygdrive/\\([a-z]\\)" path)
			   (replace-match (format "%s:" 
						  (match-string 1 path))
					  t t path)))))
      (if (and dos-path (file-exists-p dos-path))
	  dos-path
	path))))

;; Add additional dirs to the load-path
(condition-case err
    (when (getenv "OTHERDIRS")
      (let ((otherdirs (read (format "%s" (getenv "OTHERDIRS"))))
	    dir)
	(while otherdirs
	  (setq dir (car otherdirs))
	  (if (not (file-exists-p dir))
	      (error "Extra `load-path' directory %S does not exist!" dir))
	  (setq load-path (cons dir load-path)
		otherdirs (cdr otherdirs)))))

  ((end-of-file) nil)
  ((invalid-read-syntax)   
   (message "OTHERDIRS=%S rejected by `read': %s"
	    (getenv "OTHERDIRS")
	    err
	    )))
  
;; Declarations for optional/platform-specific functions
(declare-function cygwin-mount-activate "ext:cygwin-mount" ())
(declare-function cygwin-mount-convert-file-name "ext:cygwin-mount" (path))
(declare-function custom-make-dependencies "cus-dep" ())
(declare-function update-autoloads-from-directories "autoload" (&rest dirs))

;; Load byte compile
(require 'bytecomp)
;; Current public setting
;; Check for undefined functions, ignore save-excursion problems
(setq byte-compile-warnings '(not suspicious unresolved))
;; Old permissive setting

;; Preload these to get macros right 
(require 'sendmail)

;; now add VM source dirs to load-path and preload some
(setq load-path (append '("." "./lisp") load-path))
(require 'vm-macro)
(require 'vm-misc)
(require 'vm-message)
(require 'vm-vars)


(defun vm-custom-make-dependencies ()
  (defvar generated-custom-dependencies-file)
  (if (load-library "cus-dep")
      (let ((generated-custom-dependencies-file "vm-cus-load.el"))
	(custom-make-dependencies))
    (error "Failed to load 'cus-dep'")))

(defun vm-built-autoloads (&optional autoloads-file source-dir)
  (let ((autoloads-file (or autoloads-file
                            (vm-fix-cygwin-path (car command-line-args-left))))
	(source-dir (or source-dir
                        (vm-fix-cygwin-path (car (cdr command-line-args-left)))))
        (debug-on-error t)
        (enable-local-eval nil))
    (if (not (file-exists-p source-dir))
        (error "Built directory %S does not exist!" source-dir))
    (message "Building autoloads file %S\nin directory %S." autoloads-file source-dir)
    (load-library "autoload")
    (defvar generated-autoload-file)
    (set-buffer (find-file-noselect autoloads-file))
    (erase-buffer)
    (setq generated-autoload-file autoloads-file)
    (setq make-backup-files nil)
    (insert ";;; vm-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t; -*-\n")
    (insert ";;\n")
    (insert ";;; Code:\n")
    (update-directory-autoloads source-dir)))

(provide 'vm-build)
