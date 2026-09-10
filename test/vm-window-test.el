;;; vm-window-test.el --- Tests for vm-window.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM window and frame functions.
;; Uses mock frame infrastructure for testing frame operations in batch mode.

;;; Code:

(require 'vm-test-init)
(require 'vm-window)

;;; Mock frame infrastructure

(defvar vm-test-mock-frames nil
  "List of mock frames created during test.")

(defvar vm-test-mock-selected-frame nil
  "The currently selected mock frame.")

(defvar vm-test-mock-frame-counter 0
  "Counter for generating unique frame IDs.")

(defun vm-test-make-mock-frame (&optional params)
  "Create a mock frame with optional PARAMS."
  (let ((frame (cons 'mock-frame (cl-incf vm-test-mock-frame-counter))))
    (push (cons frame params) vm-test-mock-frames)
    (setq vm-test-mock-selected-frame frame)
    frame))

(defun vm-test-mock-frame-p (obj)
  "Return t if OBJ is a mock frame."
  (and (consp obj) (eq (car obj) 'mock-frame)))

(defun vm-test-mock-framep (obj)
  "Mock `framep' for mock frames."
  (if (vm-test-mock-frame-p obj)
      t
    (framep obj)))

(defun vm-test-mock-selected-frame ()
  "Mock `selected-frame' returning mock frame."
  vm-test-mock-selected-frame)

(defun vm-test-mock-select-frame (frame &optional _norecord)
  "Mock `select-frame' for mock frames."
  (when (vm-test-mock-frame-p frame)
    (setq vm-test-mock-selected-frame frame))
  frame)

(defun vm-test-mock-make-frame (&optional params)
  "Mock `make-frame' creating mock frames."
  (vm-test-make-mock-frame params))

(defun vm-test-mock-delete-frame (&optional frame _force)
  "Mock `delete-frame' for mock frames."
  (when (vm-test-mock-frame-p frame)
    (setq vm-test-mock-frames
          (cl-remove-if (lambda (f) (eq (car f) frame)) vm-test-mock-frames))
    (when (eq frame vm-test-mock-selected-frame)
      (setq vm-test-mock-selected-frame (caar vm-test-mock-frames)))))

(defun vm-test-mock-frame-list ()
  "Mock `frame-list' returning mock frames."
  (mapcar #'car vm-test-mock-frames))

(defun vm-test-mock-next-frame (&optional frame _miniframe)
  "Mock `next-frame' for mock frames."
  (let* ((frames (vm-test-mock-frame-list))
         (pos (cl-position (or frame vm-test-mock-selected-frame) frames)))
    (if pos
        (or (nth (1+ pos) frames) (car frames))
      (car frames))))

(defun vm-test-mock-frame-visible-p (_frame)
  "Mock `frame-visible-p' - always visible in tests."
  t)

(defun vm-test-mock-raise-frame (&optional _frame)
  "Mock `raise-frame' - no-op in tests."
  nil)

(defmacro vm-test-with-mock-frames (&rest body)
  "Execute BODY with mock frame infrastructure.
Creates initial frame and sets up all frame function mocks."
  (declare (indent 0) (debug t))
  `(let ((vm-test-mock-frames nil)
         (vm-test-mock-selected-frame nil)
         (vm-test-mock-frame-counter 0)
         (vm-frame-list nil))
     (cl-letf (((symbol-function 'framep)
                #'vm-test-mock-framep)
               ((symbol-function 'selected-frame)
                #'vm-test-mock-selected-frame)
               ((symbol-function 'vm-selected-frame)
                #'vm-test-mock-selected-frame)
               ((symbol-function 'select-frame)
                #'vm-test-mock-select-frame)
               ((symbol-function 'vm-select-frame)
                #'vm-test-mock-select-frame)
               ((symbol-function 'make-frame)
                #'vm-test-mock-make-frame)
               ((symbol-function 'delete-frame)
                #'vm-test-mock-delete-frame)
               ((symbol-function 'vm-delete-frame)
                #'vm-test-mock-delete-frame)
               ((symbol-function 'frame-list)
                #'vm-test-mock-frame-list)
               ((symbol-function 'next-frame)
                #'vm-test-mock-next-frame)
               ((symbol-function 'vm-next-frame)
                #'vm-test-mock-next-frame)
               ((symbol-function 'frame-visible-p)
                #'vm-test-mock-frame-visible-p)
               ((symbol-function 'vm-frame-visible-p)
                #'vm-test-mock-frame-visible-p)
               ((symbol-function 'raise-frame)
                #'vm-test-mock-raise-frame)
               ((symbol-function 'vm-raise-frame)
                #'vm-test-mock-raise-frame))
       ;; Create initial frame
       (vm-test-make-mock-frame '((name . "initial")))
       ,@body)))

;;; vm-register-frame tests

(ert-deftest vm-window-test-register-frame ()
  "Test vm-register-frame adds frame to list."
  (let ((vm-frame-list nil))
    (vm-test-with-mock-frames
      (let ((frame (vm-test-make-mock-frame)))
        (vm-register-frame frame)
        (should (memq frame vm-frame-list))))))

(ert-deftest vm-window-test-register-multiple-frames ()
  "Test vm-register-frame accumulates frames."
  (let ((vm-frame-list nil))
    (vm-test-with-mock-frames
      (let ((f1 (vm-test-make-mock-frame))
            (f2 (vm-test-make-mock-frame))
            (f3 (vm-test-make-mock-frame)))
        (vm-register-frame f1)
        (vm-register-frame f2)
        (vm-register-frame f3)
        (should (= (length vm-frame-list) 3))
        (should (memq f1 vm-frame-list))
        (should (memq f2 vm-frame-list))
        (should (memq f3 vm-frame-list))))))

;;; vm-created-this-frame-p tests

(ert-deftest vm-window-test-created-this-frame-p-registered ()
  "Test vm-created-this-frame-p returns t for registered frames."
  (let ((vm-frame-list nil))
    (vm-test-with-mock-frames
      (let ((frame (vm-test-make-mock-frame)))
        (vm-register-frame frame)
        (should (vm-created-this-frame-p frame))))))

(ert-deftest vm-window-test-created-this-frame-p-not-registered ()
  "Test vm-created-this-frame-p returns nil for unregistered frames."
  (let ((vm-frame-list nil))
    (vm-test-with-mock-frames
      (let ((frame (vm-test-make-mock-frame)))
        ;; Don't register it
        (should-not (vm-created-this-frame-p frame))))))

(ert-deftest vm-window-test-created-this-frame-p-uses-selected ()
  "Test vm-created-this-frame-p uses selected frame when none given."
  (let ((vm-frame-list nil))
    (vm-test-with-mock-frames
      (let ((frame (vm-test-make-mock-frame)))
        (vm-register-frame frame)
        ;; Frame is now selected (make-mock-frame selects it)
        (should (vm-created-this-frame-p))))))

;;; vm-goto-new-frame tests

(ert-deftest vm-window-test-goto-new-frame-creates-frame ()
  "Test vm-goto-new-frame creates a new frame."
  (let ((vm-frame-list nil)
        (vm-frame-parameter-alist '((composition ((width . 80) (height . 40)))))
        (vm-warp-mouse-to-new-frame nil))
    (vm-test-with-mock-frames
      (let ((initial-count (length (vm-test-mock-frame-list))))
        (vm-goto-new-frame 'composition)
        (should (= (length (vm-test-mock-frame-list)) (1+ initial-count)))))))

(ert-deftest vm-window-test-goto-new-frame-registers-frame ()
  "Test vm-goto-new-frame registers the new frame."
  (let ((vm-frame-list nil)
        (vm-frame-parameter-alist '((composition ((width . 80)))))
        (vm-warp-mouse-to-new-frame nil))
    (vm-test-with-mock-frames
      (vm-goto-new-frame 'composition)
      (should (= (length vm-frame-list) 1))
      (should (vm-test-mock-frame-p (car vm-frame-list))))))

(ert-deftest vm-window-test-goto-new-frame-uses-parameters ()
  "Test vm-goto-new-frame uses frame parameters from alist."
  (let ((vm-frame-list nil)
        ;; Format: ((SYMBOL PARAMLIST) ...) where PARAMLIST is a list
        (vm-frame-parameter-alist '((composition ((width . 100) (height . 50)))
                                    (summary ((width . 80) (height . 30)))))
        (vm-warp-mouse-to-new-frame nil)
        (created-params nil))
    (vm-test-with-mock-frames
      (cl-letf (((symbol-function 'make-frame)
                 (lambda (params)
                   (setq created-params params)
                   (vm-test-make-mock-frame params))))
        (vm-goto-new-frame 'composition)
        (should (equal created-params '((width . 100) (height . 50))))))))

(ert-deftest vm-window-test-goto-new-frame-tries-multiple-types ()
  "Test vm-goto-new-frame tries multiple frame types."
  (let ((vm-frame-list nil)
        ;; Format: ((SYMBOL PARAMLIST) ...)
        (vm-frame-parameter-alist '((summary ((width . 80)))))
        (vm-warp-mouse-to-new-frame nil)
        (created-params nil))
    (vm-test-with-mock-frames
      (cl-letf (((symbol-function 'make-frame)
                 (lambda (params)
                   (setq created-params params)
                   (vm-test-make-mock-frame params))))
        ;; First type 'composition not in alist, falls back to 'summary
        (vm-goto-new-frame 'composition 'summary)
        (should (equal created-params '((width . 80))))))))

;;; vm-multiple-frames-possible-p tests

(ert-deftest vm-window-test-multiple-frames-are-not-possible-in-batch ()
  "A batch Emacs cannot make a frame, whatever `make-frame' says.
`make-frame' is defined there and fails with \"Unknown terminal type\", so
VM used to answer yes and die at the point where it went to give a
composition a frame -- which is what `make test-send\\=' did
(emacs-vm/vm#617).  Interactively the answer is `make-frame' as before."
  (should noninteractive)                ; the suite runs in batch
  (should-not (vm-multiple-frames-possible-p))
  (let ((noninteractive nil))
    (if (fboundp 'make-frame)
        (should (vm-multiple-frames-possible-p))
      (should-not (vm-multiple-frames-possible-p)))))

;;; vm-set-hooks-for-frame-deletion tests

(ert-deftest vm-window-test-set-hooks-for-frame-deletion ()
  "Test vm-set-hooks-for-frame-deletion adds hooks."
  (with-temp-buffer
    (vm-set-hooks-for-frame-deletion)
    (should (local-variable-p 'vm-undisplay-buffer-hook))
    (should (memq 'vm-delete-buffer-frame vm-undisplay-buffer-hook))
    (should (memq 'vm-delete-buffer-frame kill-buffer-hook))))

;;; vm-frame-totally-visible-p tests

(ert-deftest vm-window-test-frame-totally-visible-p-visible ()
  "Test vm-frame-totally-visible-p returns t for visible frames."
  (vm-test-with-mock-frames
    (should (vm-frame-totally-visible-p (vm-test-mock-selected-frame)))))

(ert-deftest vm-window-test-frame-totally-visible-p-nil ()
  "Test vm-frame-totally-visible-p returns nil for hidden frames."
  (vm-test-with-mock-frames
    (cl-letf (((symbol-function 'frame-visible-p)
               (lambda (_f) nil)))
      (should-not (vm-frame-totally-visible-p (vm-test-mock-selected-frame))))))

(ert-deftest vm-window-test-frame-totally-visible-p-hidden ()
  "Test vm-frame-totally-visible-p returns nil for 'hidden frames."
  (vm-test-with-mock-frames
    (cl-letf (((symbol-function 'frame-visible-p)
               (lambda (_f) 'hidden)))
      (should-not (vm-frame-totally-visible-p (vm-test-mock-selected-frame))))))

;;; vm-frame-iconified-p tests

(ert-deftest vm-window-test-frame-iconified-p-icon ()
  "Test vm-frame-iconified-p returns t when frame is iconified."
  (vm-test-with-mock-frames
    (cl-letf (((symbol-function 'vm-frame-visible-p)
               (lambda (_f) 'icon)))
      (should (vm-frame-iconified-p (vm-test-mock-selected-frame))))))

(ert-deftest vm-window-test-frame-iconified-p-visible ()
  "Test vm-frame-iconified-p returns nil when frame is visible."
  (vm-test-with-mock-frames
    (cl-letf (((symbol-function 'vm-frame-visible-p)
               (lambda (_f) t)))
      (should-not (vm-frame-iconified-p (vm-test-mock-selected-frame))))))

;;; Frame parameter alist tests

(ert-deftest vm-window-test-frame-parameter-alist-lookup ()
  "Test looking up frame parameters from alist."
  ;; Format: ((SYMBOL PARAMLIST) ...) where PARAMLIST is a list of cons cells
  (let ((vm-frame-parameter-alist
         '((composition ((width . 80) (height . 40) (menu-bar-lines . 0)))
           (summary ((width . 100) (height . 30)))
           (folder ((width . 80) (height . 50))))))
    (should (assq 'composition vm-frame-parameter-alist))
    (should (assq 'summary vm-frame-parameter-alist))
    (should (assq 'folder vm-frame-parameter-alist))
    (should-not (assq 'nonexistent vm-frame-parameter-alist))
    ;; Check parameter values - cadr gets the PARAMLIST
    (let ((comp-params (cadr (assq 'composition vm-frame-parameter-alist))))
      (should (equal (assq 'width comp-params) '(width . 80)))
      (should (equal (assq 'height comp-params) '(height . 40))))))

;;; Window loop tests (no frame mocking needed)
;;; What the window functions do.  These four had a test each asserting the
;;; function was bound.

(defmacro vm-window-test-with-two-windows (spec &rest body)
  "Run BODY with two windows, showing the buffers SPEC names.
SPEC is (VAR-A VAR-B): each is bound to a fresh buffer shown in a window.
The configuration is restored afterwards, so a test cannot strand the run in
a window it made."
  (declare (indent 1) (debug t))
  (let ((a (nth 0 spec)) (b (nth 1 spec)))
    `(let ((,a (generate-new-buffer " *vm-window-test-a*"))
           (,b (generate-new-buffer " *vm-window-test-b*")))
       (unwind-protect
           (save-window-excursion
             (delete-other-windows)
             (switch-to-buffer ,a)
             (select-window (split-window))
             (switch-to-buffer ,b)
             ,@body)
         (kill-buffer ,a)
         (kill-buffer ,b)))))

(ert-deftest vm-window-test-window-loop-replace-changes-every-such-window ()
  "`vm-window-loop' replace puts the second buffer wherever the first was."
  (vm-window-test-with-two-windows (a b)
    (let ((vm-search-other-frames nil))
      (vm-window-loop 'replace a b)
      (should-not (memq a (mapcar #'window-buffer (window-list))))
      (should (memq b (mapcar #'window-buffer (window-list)))))))

(ert-deftest vm-window-test-window-loop-takes-a-buffer-name ()
  "A buffer name works where a buffer does: `vm-window-loop' looks it up."
  (vm-window-test-with-two-windows (a b)
    (let ((vm-search-other-frames nil))
      (vm-window-loop 'replace (buffer-name a) b)
      (should-not (memq a (mapcar #'window-buffer (window-list)))))))

(ert-deftest vm-window-test-window-loop-delete-removes-the-window ()
  "`vm-window-loop' delete deletes the window showing the buffer."
  (vm-window-test-with-two-windows (a b)
    (let ((vm-search-other-frames nil)
          (before (length (window-list))))
      (vm-window-loop 'delete a)
      (should (= (length (window-list)) (1- before)))
      (should-not (memq a (mapcar #'window-buffer (window-list)))))))

(ert-deftest vm-window-test-window-loop-keeps-the-last-window ()
  "Deleting the only window is refused, since a frame must have one.
The deferred deletion the function goes to the trouble of is what makes this
work: the window is deleted after point has moved off it."
  (let ((a (generate-new-buffer " *vm-window-test-a*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer a)
          (let ((vm-search-other-frames nil))
            (vm-window-loop 'delete a)
            (should (= (length (window-list)) 1))))
      (kill-buffer a))))

(ert-deftest vm-window-test-bury-buffer-buries-the-current-one-by-default ()
  "`vm-bury-buffer' with no argument buries the buffer you are in."
  (let ((a (generate-new-buffer " *vm-window-test-a*"))
        (b (generate-new-buffer " *vm-window-test-b*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer b)
          (switch-to-buffer a)
          (should (eq (car (buffer-list)) a))
          (vm-bury-buffer)
          (should-not (eq (car (buffer-list)) a))
          (should (memq a (buffer-list))))
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest vm-window-test-unbury-buffer-leaves-the-windows-as-they-were ()
  "`vm-unbury-buffer' raises a buffer without disturbing the display.
It is called where VM wants a buffer out of the way of `bury-buffer' but has
no intention of showing it."
  (let ((a (generate-new-buffer " *vm-window-test-a*"))
        (b (generate-new-buffer " *vm-window-test-b*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer b)
          (bury-buffer a)
          (should (eq (car (last (buffer-list))) a))
          (vm-unbury-buffer a)
          (should (eq (window-buffer (selected-window)) b))
          (should-not (eq (car (last (buffer-list))) a)))
      (kill-buffer a)
      (kill-buffer b))))


;;; vm-bury-buffer tests

;;; vm-display function tests

;;; Frame compatibility function tests

(ert-deftest vm-window-test-frame-compat-functions-exist ()
  "Test that frame compatibility functions are defined."
  (should (fboundp 'vm-selected-frame))
  (should (fboundp 'vm-select-frame))
  (should (fboundp 'vm-delete-frame))
  (should (fboundp 'vm-raise-frame))
  (should (fboundp 'vm-frame-visible-p))
  (should (fboundp 'vm-window-frame)))

(ert-deftest vm-window-test-frame-navigation-functions-exist ()
  "Test that frame navigation functions exist when available."
  ;; These may not be bound in all Emacs builds
  (when (fboundp 'next-frame)
    (should (fboundp 'vm-next-frame))
    (should (fboundp 'vm-frame-selected-window))))

;;; The frame wrappers are functions, defined here (issue #595)

;; `vm-delete-frame', `vm-raise-frame' and `vm-select-frame' used to be made
;; with `(fset 'X (symbol-function (cond ...)))', choosing between the Emacs
;; and XEmacs spellings at load time.  Two consequences, both fixed by writing
;; them as ordinary functions that dispatch when called:
;;
;; `fset' writes the function cell and nothing else, so `symbol-file' returned
;; nil and the reference appendix, which files a command by the file defining
;; it, left them out of the manual entirely.
;;
;; And copying the function object copied `delete-frame''s interactive spec
;; with it, so `vm-delete-frame' was a command -- offered by `M-x', asking to
;; be used -- when it is an internal wrapper that VM never meant to expose.

(defconst vm-window-test--frame-wrappers
  '((vm-selected-frame        . (0 . 0))
    (vm-delete-frame          . (0 . 2))
    (vm-raise-frame           . (0 . 1))
    (vm-select-frame          . (1 . 2))
    (vm-frame-visible-p       . (1 . 1))
    (vm-frame-iconified-p     . (0 . 1))
    (vm-window-frame          . (1 . 1))
    (vm-next-frame            . (0 . 2))
    (vm-frame-selected-window . (0 . 1)))
  "Wrapper, and the arity it takes from the Emacs function it stands for.")

(ert-deftest vm-window-test-frame-wrappers-are-plain-functions ()
  "Each wrapper is a function with a known file, and is not a command."
  (require 'vm-window)
  (dolist (entry vm-window-test--frame-wrappers)
    (let ((wrapper (car entry)))
      (should (fboundp wrapper))
      (should (symbol-file wrapper))
      (should-not (commandp wrapper))
      (should (equal (cdr entry) (func-arity wrapper))))))

(ert-deftest vm-window-test-frame-wrappers-reach-emacs ()
  "Each wrapper calls through to what Emacs provides.
Batch Emacs has one visible frame, so all of these can be asked for real.
`vm-delete-frame' is the exception -- deleting the only frame is not
something to do mid-suite -- and its dispatch is the same `cond' as the
rest, checked by arity above."
  (require 'vm-window)
  (let ((frame (selected-frame)))
    (should (eq frame (vm-selected-frame)))
    (should (eq frame (vm-window-frame (selected-window))))
    (should (eq frame (vm-select-frame frame)))
    (should (eq frame (vm-next-frame frame)))
    (should (eq (selected-window) (vm-frame-selected-window frame)))
    (should (eq t (vm-frame-visible-p frame)))
    (should-not (vm-frame-iconified-p frame))
    (should-not (vm-raise-frame frame))
    (should-not (vm-raise-frame))))

;;; Saved window configurations (emacs-vm/vm#632)
;;
;; VM can remember a window layout per command and restore it next time.  The
;; three commands that manage those had no test: what matters is that a
;; configuration is recorded under the name given, written to the file so it
;; outlives the session, applied without complaint, and forgotten on request.

(defmacro vm-window-test--with-configuration-file (spec &rest body)
  "Run BODY with an empty window-configuration file.
SPEC is (FILE-VAR).  `vm-window-configurations' starts empty and the file is
in a directory of its own, so nothing of the user's is read or written."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-window-config" t))))
     (unwind-protect
         (let* ((,(car spec) (expand-file-name "configurations" dir))
                (vm-window-configuration-file ,(car spec))
                (vm-window-configurations nil)
                (vm-mutable-window-configuration t))
           ,@body)
       (delete-directory dir t))))

(ert-deftest vm-window-test-saving-a-window-configuration ()
  "`vm-save-window-configuration' records the layout under the name given and
writes it to `vm-window-configuration-file', which is what makes it outlive
the session."
  (vm-window-test--with-configuration-file (file)
    (should (null vm-window-configurations))
    (vm-save-window-configuration 'startup)
    (should (equal (mapcar #'car vm-window-configurations) '(startup)))
    (should (file-exists-p file))
    (should (string-match-p "startup"
                            (with-temp-buffer (insert-file-contents file)
                                              (buffer-string))))))

(ert-deftest vm-window-test-applying-and-deleting-a-configuration ()
  "A saved configuration can be applied by name and then forgotten.
Deleting it leaves nothing behind: the action has no configuration afterwards,
which is the point of the command."
  (vm-window-test--with-configuration-file (_file)
    (vm-save-window-configuration 'startup)
    (vm-apply-window-configuration 'startup)
    (should (equal (mapcar #'car vm-window-configurations) '(startup)))
    (vm-delete-window-configuration 'startup)
    (should (null vm-window-configurations))))

(ert-deftest vm-window-test-two-configurations-are-kept-apart ()
  "Configurations are per action, so saving a second leaves the first alone
and deleting one leaves the other."
  (vm-window-test--with-configuration-file (_file)
    (vm-save-window-configuration 'startup)
    (vm-save-window-configuration 'reading-message)
    (should (equal (sort (mapcar #'car vm-window-configurations)
                         (lambda (a b) (string< (symbol-name a)
                                                (symbol-name b))))
                   '(reading-message startup)))
    (vm-delete-window-configuration 'startup)
    (should (equal (mapcar #'car vm-window-configurations)
                   '(reading-message)))))

(ert-deftest vm-window-test-configurations-need-a-file-to-be-enabled ()
  "With no `vm-window-configuration-file' the commands say the feature is off
rather than quietly doing nothing -- there would be nowhere to keep what they
were asked to save."
  (let ((vm-window-configuration-file nil)
        (vm-window-configurations nil)
        (text-quoting-style 'grave))
    (dolist (command '(vm-save-window-configuration
                       vm-delete-window-configuration))
      (should (equal (cadr (should-error (funcall command 'startup)))
                     (concat "Configurable windows not enabled.  "
                             "Set vm-window-configuration-file to enable."))))))

(provide 'vm-window-test)

;;; vm-window-test.el ends here
