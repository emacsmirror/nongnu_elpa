;;; test-flymake-pyrefly.el --- Test flymake-pyrefly package. -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Boris Shminke
;; Author: Boris Shminke <boris@shminke.com>

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
(when (require 'undercover nil t)
  (undercover "flymake-pyrefly.el"
              (:report-format 'lcov)
              (:send-report nil)))
(require 'flymake-pyrefly)
(require 'flymake)
(require 'f)
(ert-deftest flymake-pyrefly-test-no-pyrefly ()
  "Test error message when Pyrefly is not found."
  (find-file (f-join "test" "example.py"))
  (should-error (pyrefly-flymake-backend 'identity)
                :type 'no-pyrefly-error))
(ert-deftest flymake-pyrefly-test-normal-use-case ()
  "Test a normal Pyrefly use-case."
  (defun mock-report-fn (args)
    (setq saved-args args))
  (push (getenv "PYREFLY_BIN_DIR") exec-path)
  (find-file (f-join "test" "example.py"))
  (setq saved-args nil)
  (pyrefly-flymake-backend 'mock-report-fn)
  (pyrefly-flymake-backend 'mock-report-fn)
  (sleep-for 1)
  (pop exec-path)
  (should
   (equal (aref (car saved-args) 2) (cons 2 5))))
(ert-deftest flymake-pyrefly-test-setup ()
  (pyrefly-setup-flymake-backend)
  (should (member 'pyrefly-flymake-backend flymake-diagnostic-functions)))
(provide 'test-flymake-pyrefly)
;;; test-flymake-pyrefly.el ends here
