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
  (should-error (flymake-pyrefly 'identity)))
(ert-deftest flymake-pyrefly-test-normal-use-case ()
  "Test a normal Pyrefly use-case."
  (let ((saved-args nil)
        (mock-report-fn (lambda (args) (setq saved-args args))))
  (find-file "example.py")
  (flymake-pyrefly mock-report-fn)
  (flymake-pyrefly mock-report-fn)
  (sleep-for 1)
  (should
   (equal
    (aref (car saved-args) 5)
    "`Literal[0]` is not assignable to variable `x` with type `str` [bad-assignment]"))))
(ert-deftest flymake-pyrefly-test-setup ()
  (pyrefly-setup-flymake-backend)
  (should (member 'flymake-pyrefly flymake-diagnostic-functions)))
(provide 'test-flymake-pyrefly)
;;; test-flymake-pyrefly.el ends here
