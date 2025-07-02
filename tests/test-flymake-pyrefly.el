;;; test-flymake-pyrefly.el --- Test flymake-pyrefly package. -*- lexical-binding: t; -*-
;; Copyright (C) 2025  Boris Shminke

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Test suite for flymake-pyrefly.

;;; Code:
(require 'flymake-pyrefly)
(require 'flymake)
(ert-deftest flymake-pyrefly-test-no-pyrefly ()
  "Test error message when Pyrefly is not found."
  (find-file "tests/example.py")
  (should-error (flymake-pyrefly 'identity)))
(ert-deftest flymake-pyrefly-test-normal-use-case ()
  "Test a normal Pyrefly use-case."
  (defun mock-report-fn (args)
    (setq saved-args args))
  (push (getenv "PYREFLY_BIN_DIR") exec-path)
  (find-file "tests/example.py")
  (setq saved-args nil)
  (flymake-pyrefly 'mock-report-fn)
  (flymake-pyrefly 'mock-report-fn)
  (sleep-for 1)
  (pop exec-path)
  (should
   (equal (aref (car saved-args) 2) (cons 2 5))))
(ert-deftest flymake-pyrefly-test-setup ()
  (pyrefly-setup-flymake-backend)
  (should (member 'flymake-pyrefly flymake-diagnostic-functions)))
(provide 'test-flymake-pyrefly)
;;; test-flymake-pyrefly.el ends here
