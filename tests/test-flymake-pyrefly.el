;;; test-flymake-pyrefly.el --- Test flymake-pyrefly package.

;;; Code:
(require 'flymake-pyrefly)
(ert-deftest flymake-pyrefly-test-no-pyrefly ()
  "Test error message when Pyrefly is not found."
  (should-error (flymake-pyrefly 'message)))
(provide 'test-flymake-pyrefly)
;;; test-flymake-pyrefly.el ends here
