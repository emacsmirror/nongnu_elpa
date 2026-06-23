;;; hermes-browsers-tests.el --- browsers tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-rollback-rows-from-list ()
  "Rollback rows abbreviate the hash and map timestamp/message."
  (let ((rows (hermes-rollback--rows
               '((checkpoints . (((hash . "abcdef1234567890")
                                  (timestamp . "2026-01-01") (message . "edit foo"))))))))
    (should (equal (caar rows) "abcdef1234567890"))
    (should (equal (aref (cadr (car rows)) 0) "abcdef12"))
    (should (equal (aref (cadr (car rows)) 1) "2026-01-01"))
    (should (equal (aref (cadr (car rows)) 2) "edit foo"))))

(ert-deftest hermes-rollback-list-fetches-and-renders ()
  "Listing fetches rollback.list and renders the checkpoints."
  (let (stopped)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-rollback-list)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((checkpoints . (((hash . "h1") (message . "m1")))))))))
      (unwind-protect
          (progn
            (hermes-list-rollbacks)
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Rollbacks*"
              (should (derived-mode-p 'hermes-rollback-mode))
              (should (equal (caar tabulated-list-entries) "h1"))))
        (when (get-buffer "*Hermes Rollbacks*") (kill-buffer "*Hermes Rollbacks*"))))))

(ert-deftest hermes-rollback-display-diff-fontifies ()
  "The diff view renders the unified diff through diff-mode."
  (unwind-protect
      (progn
        (hermes-rollback--display-diff
         "abc1234567"
         '((diff . "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n")))
        (with-current-buffer "*Hermes Rollback Diff*"
          (should (derived-mode-p 'diff-mode))
          (should (string-match-p "\\+new" (buffer-string)))))
    (when (get-buffer "*Hermes Rollback Diff*")
      (kill-buffer "*Hermes Rollback Diff*"))))

(ert-deftest hermes-subagents-rows-indents-by-depth ()
  "Subagent rows indent the goal by spawn depth."
  (let ((rows (hermes-subagents--rows
               '((active . (((subagent_id . "s0") (depth . 0) (goal . "root")
                             (status . "running") (model . "m") (tool_count . 2))
                            ((subagent_id . "s1") (depth . 2) (goal . "child")
                             (status . "running") (model . "m") (tool_count . 0))))))))
    (should (equal (caar rows) "s0"))
    (should (equal (aref (cadr (car rows)) 0) "root"))
    (should (equal (aref (cadr (nth 1 rows)) 0) "    child"))
    (should (equal (aref (cadr (car rows)) 3) "2"))))

(ert-deftest hermes-subagents-list-fetches-and-renders ()
  "Listing fetches delegation.status and renders active subagents."
  (let (stopped)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-delegation-status)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((active . (((subagent_id . "s0") (depth . 0)
                                        (goal . "root")))))))))
      (unwind-protect
          (progn
            (hermes-list-subagents)
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Subagents*"
              (should (derived-mode-p 'hermes-subagents-mode))
              (should (equal (caar tabulated-list-entries) "s0"))))
        (when (get-buffer "*Hermes Subagents*")
          (kill-buffer "*Hermes Subagents*"))))))

(ert-deftest hermes-browser-list-browser-macro-defines-working-browser ()
  "`hermes-define-list-browser' defines a mode, keymap, render, and command."
  (hermes-define-list-browser browsertest
    :title "Hermes Browser Test"
    :buffer "*Hermes Browser Test*"
    :columns [("Name" 20 t)]
    :fetch (lambda (_client) (hermes--promise-resolved '("a" "b")))
    :rows (lambda (result)
            (mapcar (lambda (name) (list name (vector name))) result))
    :keys ("g" #'ignore))
  (unwind-protect
      (progn
        (should (fboundp 'hermes-browsertest-mode))
        (should (fboundp 'hermes-list-browsertest))
        (should (eq (keymap-lookup hermes-browsertest-mode-map "g") #'ignore))
        (hermes-browsertest--render '("x" "y"))
        (with-current-buffer "*Hermes Browser Test*"
          (should (derived-mode-p 'hermes-browsertest-mode))
          (should (equal tabulated-list-format [("Name" 20 t)]))
          (should (equal (mapcar #'car tabulated-list-entries) '("x" "y")))))
    (when (get-buffer "*Hermes Browser Test*")
      (kill-buffer "*Hermes Browser Test*"))))

(provide 'hermes-browsers-tests)
;;; hermes-browsers-tests.el ends here
