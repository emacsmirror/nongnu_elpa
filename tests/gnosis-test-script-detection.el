;;; gnosis-test-script-detection.el --- Script detection tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for script detection and input method mapping.
;; `gnosis-utils-detect-script' is pure -- no database needed.

;;; Code:

(require 'ert)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name default-directory))))))
  (add-to-list 'load-path parent-dir))

(require 'gnosis-utils)
(require 'gnosis)

;; ---- Group 1: gnosis-utils-detect-script ----

(ert-deftest gnosis-test-detect-script-greek ()
  "Greek text returns greek script symbol."
  (should (eq 'greek (gnosis-utils-detect-script "ελληνικά"))))

(ert-deftest gnosis-test-detect-script-cyrillic ()
  "Cyrillic text returns cyrillic script symbol."
  (should (eq 'cyrillic (gnosis-utils-detect-script "русский"))))

(ert-deftest gnosis-test-detect-script-latin-only ()
  "Pure Latin text returns nil."
  (should (null (gnosis-utils-detect-script "hello world"))))

(ert-deftest gnosis-test-detect-script-empty-string ()
  "Empty string returns nil."
  (should (null (gnosis-utils-detect-script ""))))

(ert-deftest gnosis-test-detect-script-ascii-only ()
  "ASCII with punctuation and digits returns nil."
  (should (null (gnosis-utils-detect-script "test 123!@#"))))

(ert-deftest gnosis-test-detect-script-mixed-greek-latin ()
  "Mixed text returns the dominant non-Latin script."
  (should (eq 'greek (gnosis-utils-detect-script "the word λόγος means reason"))))

(ert-deftest gnosis-test-detect-script-single-char ()
  "Single non-Latin character is detected."
  (should (eq 'greek (gnosis-utils-detect-script "α"))))

(ert-deftest gnosis-test-detect-script-cjk ()
  "CJK ideographs are detected."
  (let ((script (gnosis-utils-detect-script "漢字")))
    (should script)
    (should (not (eq script 'latin)))))

(ert-deftest gnosis-test-detect-script-spaces-ignored ()
  "Whitespace does not affect detection."
  (should (eq 'greek (gnosis-utils-detect-script "  αβγ  "))))

(ert-deftest gnosis-test-detect-script-numbers-ignored ()
  "Digits (common script) do not affect detection."
  (should (eq 'greek (gnosis-utils-detect-script "42 αβγ"))))

;; ---- Group 2: gnosis-script-input-method-alist lookup ----

(ert-deftest gnosis-test-script-alist-default-has-greek ()
  "Default alist maps greek to the \"greek\" input method."
  (should (equal "greek"
                 (alist-get 'greek gnosis-script-input-method-alist))))

(ert-deftest gnosis-test-script-alist-missing-script ()
  "Unmapped script returns nil from alist."
  (let ((gnosis-script-input-method-alist '((greek . "greek"))))
    (should (null (alist-get 'cyrillic gnosis-script-input-method-alist)))))

(ert-deftest gnosis-test-script-alist-custom-mapping ()
  "Custom alist entries are respected."
  (let ((gnosis-script-input-method-alist
         '((greek . "greek") (cyrillic . "cyrillic-translit"))))
    (should (equal "cyrillic-translit"
                   (alist-get 'cyrillic gnosis-script-input-method-alist)))))

;; ---- Group 3: gnosis--read-string-with-input-method ----
;;
;; The function calls `activate-input-method' in the current buffer,
;; then `read-string' with INHERIT-INPUT-METHOD=t so the minibuffer
;; inherits it, then `deactivate-input-method' to clean up.
;;
;; We stub `activate-input-method' (to set `current-input-method'),
;; `deactivate-input-method' (to clear it), and `read-string' (to
;; capture the state at call time).

(ert-deftest gnosis-test-read-string-greek-input-method-active ()
  "Greek answer: activate-input-method called, inherit=t, deactivate called."
  (let ((captured-method nil)
        (captured-inherit nil)
        (deactivated nil)
        (gnosis-script-input-method-alist '((greek . "greek"))))
    (cl-letf (((symbol-function 'activate-input-method)
               (lambda (method) (setq current-input-method method)))
              ((symbol-function 'deactivate-input-method)
               (lambda () (setq current-input-method nil deactivated t)))
              ((symbol-function 'read-string)
               (lambda (_prompt &optional _init _hist _default inherit)
                 (setq captured-method current-input-method
                       captured-inherit inherit)
                 "dummy")))
      (gnosis--read-string-with-input-method "Answer: " "ελληνικά")
      (should (equal "greek" captured-method))
      (should (eq t captured-inherit))
      (should deactivated))))

(ert-deftest gnosis-test-read-string-cyrillic-input-method-active ()
  "Cyrillic answer with mapping: input method activated and inherited."
  (let ((captured-method nil)
        (captured-inherit nil)
        (gnosis-script-input-method-alist '((greek . "greek")
                                            (cyrillic . "cyrillic-translit"))))
    (cl-letf (((symbol-function 'activate-input-method)
               (lambda (method) (setq current-input-method method)))
              ((symbol-function 'deactivate-input-method)
               (lambda () (setq current-input-method nil)))
              ((symbol-function 'read-string)
               (lambda (_prompt &optional _init _hist _default inherit)
                 (setq captured-method current-input-method
                       captured-inherit inherit)
                 "dummy")))
      (gnosis--read-string-with-input-method "Answer: " "самолет")
      (should (equal "cyrillic-translit" captured-method))
      (should (eq t captured-inherit)))))

(ert-deftest gnosis-test-read-string-latin-no-activation ()
  "Latin answer: no activate/deactivate calls, plain read-string."
  (let ((activated nil)
        (gnosis-script-input-method-alist '((greek . "greek"))))
    (cl-letf (((symbol-function 'activate-input-method)
               (lambda (method) (setq activated method)))
              ((symbol-function 'deactivate-input-method) #'ignore)
              ((symbol-function 'read-string)
               (lambda (_prompt &rest _args) "dummy")))
      (gnosis--read-string-with-input-method "Answer: " "hello")
      (should (null activated)))))

(ert-deftest gnosis-test-read-string-unmapped-no-activation ()
  "Unmapped script: no activate/deactivate calls."
  (let ((activated nil)
        (gnosis-script-input-method-alist '((greek . "greek"))))
    (cl-letf (((symbol-function 'activate-input-method)
               (lambda (method) (setq activated method)))
              ((symbol-function 'deactivate-input-method) #'ignore)
              ((symbol-function 'read-string)
               (lambda (_prompt &rest _args) "dummy")))
      (gnosis--read-string-with-input-method "Answer: " "самолет")
      (should (null activated)))))

(ert-deftest gnosis-test-read-string-returns-user-input ()
  "Return value is the string from read-string."
  (let ((gnosis-script-input-method-alist '((greek . "greek"))))
    (cl-letf (((symbol-function 'activate-input-method)
               (lambda (method) (setq current-input-method method)))
              ((symbol-function 'deactivate-input-method)
               (lambda () (setq current-input-method nil)))
              ((symbol-function 'read-string)
               (lambda (_prompt &rest _args) "user-typed")))
      (should (equal "user-typed"
                     (gnosis--read-string-with-input-method "Answer: " "αβγ"))))))

(ert-deftest gnosis-test-read-string-deactivates-on-error ()
  "Input method is deactivated even when read-string signals an error."
  (let ((deactivated nil)
        (gnosis-script-input-method-alist '((greek . "greek"))))
    (cl-letf (((symbol-function 'activate-input-method)
               (lambda (method) (setq current-input-method method)))
              ((symbol-function 'deactivate-input-method)
               (lambda () (setq current-input-method nil deactivated t)))
              ((symbol-function 'read-string)
               (lambda (_prompt &rest _args) (error "Simulated quit"))))
      (ignore-errors
        (gnosis--read-string-with-input-method "Answer: " "αβγ"))
      (should deactivated))))

(ert-run-tests-batch-and-exit)
