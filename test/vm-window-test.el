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

(ert-deftest vm-window-test-multiple-frames-possible-p ()
  "Test vm-multiple-frames-possible-p checks for make-frame."
  ;; In batch mode with GUI Emacs, make-frame is usually fboundp
  (if (fboundp 'make-frame)
      (should (vm-multiple-frames-possible-p))
    (should-not (vm-multiple-frames-possible-p))))

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

(ert-deftest vm-window-test-window-loop-exists ()
  "Test that vm-window-loop function exists."
  (should (fboundp 'vm-window-loop)))

;;; vm-bury-buffer tests

(ert-deftest vm-window-test-bury-buffer-exists ()
  "Test that vm-bury-buffer function exists."
  (should (fboundp 'vm-bury-buffer)))

(ert-deftest vm-window-test-unbury-buffer-exists ()
  "Test that vm-unbury-buffer function exists."
  (should (fboundp 'vm-unbury-buffer)))

;;; vm-display function tests

(ert-deftest vm-window-test-display-function-exists ()
  "Test that vm-display function exists."
  (should (fboundp 'vm-display)))

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

(provide 'vm-window-test)

;;; vm-window-test.el ends here
