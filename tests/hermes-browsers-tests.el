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

(ert-deftest hermes-browser-list-browser-revert-refreshes-without-display ()
  "Revert refreshes rows in place; only the command displays the buffer."
  (hermes-define-list-browser browserrevert
    :title "Hermes Browser Revert"
    :buffer "*Hermes Browser Revert*"
    :columns [("Name" 20 t)]
    :fetch (lambda (_client) (hermes--promise-resolved '("a" "b")))
    :rows (lambda (result)
            (mapcar (lambda (name) (list name (vector name))) result)))
  (let (displayed)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'pop-to-buffer)
               (lambda (&rest _) (setq displayed t))))
      (unwind-protect
          (progn
            (hermes-browserrevert--revert)
            (should-not displayed)
            (with-current-buffer "*Hermes Browser Revert*"
              (should (equal (mapcar #'car tabulated-list-entries) '("a" "b"))))
            (hermes-list-browserrevert)
            (should displayed))
        (when (get-buffer "*Hermes Browser Revert*")
          (kill-buffer "*Hermes Browser Revert*"))))))

;;; Group: dynamic column widths

(ert-deftest hermes-browser-dynamic-format-fits-and-flexes ()
  "Dynamic column format fits the width and grows weighted columns."
  (let ((specs '(("A" 6 0 t) ("B" 8 0 t) ("C" 10 3 nil))))
    (dolist (width '(30 40 80 120))
      (let ((format (hermes-browser--dynamic-format width specs)))
        (should (= (hermes-test--tabulated-list-format-total-width format)
                   width))))
    (let ((narrow (hermes-browser--dynamic-format 40 specs))
          (wide (hermes-browser--dynamic-format 120 specs)))
      (should (> (cadr (aref wide 2)) (cadr (aref narrow 2))))
      (should (= (cadr (aref wide 0)) 6)))))

(ert-deftest hermes-browser-dynamic-format-preserves-sort-and-name ()
  "Dynamic format keeps each spec's header and sort predicate."
  (let ((format (hermes-browser--dynamic-format
                 80 '(("A" 6 0 t) ("B" 10 5 nil)))))
    (should (equal (car (aref format 0)) "A"))
    (should (eq (caddr (aref format 0)) t))
    (should (eq (caddr (aref format 1)) nil))))

(ert-deftest hermes-browser-dynamic-format-honors-max-cap ()
  "A column MAX caps its computed width even on a wide window."
  (let ((format (hermes-browser--dynamic-format
                 200 '(("A" 6 0 t) ("Wide" 10 5 t 20)))))
    (should (= (cadr (aref format 1)) 20))))

(ert-deftest hermes-browser-shrink-widths-fits-narrow-target ()
  "Shrinking trims the widest column until the total fits the target."
  (let ((widths (hermes-browser--shrink-widths '(10 20 30) 30)))
    (should (<= (apply #'+ widths) 30))
    (should (seq-every-p (lambda (w) (> w 0)) widths))))

(ert-deftest hermes-cron-columns-scale-with-width ()
  "Cron dynamic columns fit the display width and keep index-sensitive order."
  (dolist (width '(40 80 120))
    (let ((format (hermes-cron--format width)))
      (should (= (hermes-test--tabulated-list-format-total-width format) width))
      (should (equal (car (aref format 2)) "State"))
      (should (equal (car (aref format 3)) "Profile")))))

(provide 'hermes-browsers-tests)
;;; hermes-browsers-tests.el ends here
