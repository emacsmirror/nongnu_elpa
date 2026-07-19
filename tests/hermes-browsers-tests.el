;;; hermes-browsers-tests.el --- browsers tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-browser-semantic-faces-are-customizable ()
  "Every semantic browser role has its own customizable face."
  (dolist (face '(hermes-browser-name hermes-browser-title
                  hermes-browser-description hermes-browser-identifier
                  hermes-browser-profile hermes-browser-count
                  hermes-browser-message-count hermes-browser-tool-count
                  hermes-browser-total hermes-browser-priority
                  hermes-browser-assignee hermes-browser-model
                  hermes-browser-provider hermes-browser-type
                  hermes-browser-timestamp hermes-browser-schedule
                  hermes-browser-delivery hermes-browser-prompt
                  hermes-browser-command hermes-browser-category
                  hermes-browser-version hermes-browser-source
                  hermes-browser-message hermes-browser-default
                  hermes-browser-reasoning hermes-browser-diagnostic
                  hermes-browser-uptime hermes-browser-goal
                  hermes-browser-enabled hermes-browser-state
                  hermes-browser-status hermes-browser-severity
                  hermes-browser-active hermes-browser-success
                  hermes-browser-pending hermes-browser-error
                  hermes-browser-muted))
    (should (facep face))))

(ert-deftest hermes-browser-semantic-face-cell-preserves-visible-text ()
  "Semantic cells add only the requested face to their visible text."
  (let ((cell (hermes-browser--face-cell 42 'hermes-browser-count)))
    (should (equal cell "42"))
    (should (eq (get-text-property 0 'face cell) 'hermes-browser-count)))
  (should (equal (hermes-browser--face-cell "" 'hermes-browser-count) ""))
  (should-not (get-text-property
               0 'face (hermes-browser--face-cell "plain" nil))))

(ert-deftest hermes-browser-semantic-status-face-classifies-known-states ()
  "Known backend states map to the shared semantic face vocabulary."
  (should (eq (hermes-browser--status-face "running")
              'hermes-browser-active))
  (should (eq (hermes-browser--status-face "ready")
              'hermes-browser-success))
  (should (eq (hermes-browser--status-face "on")
              'hermes-browser-success))
  (should (eq (hermes-browser--status-face "triage")
              'hermes-browser-pending))
  (should (eq (hermes-browser--status-face "connecting")
              'hermes-browser-pending))
  (should (eq (hermes-browser--status-face "blocked")
              'hermes-browser-error))
  (should (eq (hermes-browser--status-face "archived")
              'hermes-browser-muted))
  (should (eq (hermes-browser--status-face "backend-specific")
              'hermes-browser-status)))

(ert-deftest hermes-browser-semantic-status-cell-faces-unknown-states ()
  "Status cells give known and unknown states explicit faces."
  (let ((known (hermes-browser--status-cell "RUNNING"))
        (unknown (hermes-browser--status-cell "custom"))
        (column-unknown
         (hermes-browser--status-cell "custom" 'hermes-browser-status)))
    (should (equal known "RUNNING"))
    (should (eq (get-text-property 0 'face known) 'hermes-browser-active))
    (should (equal unknown "custom"))
    (should (eq (get-text-property 0 'face unknown)
                'hermes-browser-status))
    (should (eq (get-text-property 0 'face column-unknown)
                'hermes-browser-status))))

(ert-deftest hermes-rollback-rows-from-list ()
  "Rollback rows abbreviate the hash and map timestamp/message."
  (let ((rows (hermes-rollback--rows
               '((checkpoints . (((hash . "abcdef1234567890")
                                  (timestamp . "2026-01-01") (message . "edit foo"))))))))
    (should (equal (caar rows) "abcdef1234567890"))
    (should (equal (aref (cadr (car rows)) 0) "abcdef12"))
    (should (equal (aref (cadr (car rows)) 1) "2026-01-01"))
    (should (equal (aref (cadr (car rows)) 2) "edit foo"))))

(ert-deftest hermes-rollback-rows-face-every-column ()
  "Rollback rows give every column its own face."
  (let* ((row (car (hermes-rollback--rows
                    '((checkpoints . (((hash . "abcdef1234567890")
                                       (timestamp . "2026-01-01")
                                       (message . "edit foo"))))))))
         (entry (cadr row)))
    (should (eq (get-text-property 0 'face (aref entry 0))
                'hermes-browser-identifier))
    (should (eq (get-text-property 0 'face (aref entry 1))
                'hermes-browser-timestamp))
    (should (eq (get-text-property 0 'face (aref entry 2))
                'hermes-browser-message))))

(ert-deftest hermes-rollback-list-fetches-and-renders ()
  "Listing fetches rollback.list with the live session id and renders it."
  (let (stopped seen-session)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "sid-live"))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-rollback-list)
               (lambda (_client &rest args)
                 (setq seen-session (plist-get args :session-id))
                 (funcall (plist-get args :resolve)
                          '((checkpoints . (((hash . "h1") (message . "m1")))))))))
      (unwind-protect
          (progn
            (hermes-list-rollbacks)
            (should (eq stopped 'fake-client))
            (should (equal seen-session "sid-live"))
            (with-current-buffer "*Hermes Rollbacks*"
              (should (derived-mode-p 'hermes-rollback-mode))
              (should (equal (caar tabulated-list-entries) "h1"))))
        (when (get-buffer "*Hermes Rollbacks*") (kill-buffer "*Hermes Rollbacks*"))))))

(ert-deftest hermes-rollback-list-without-live-session-rejects ()
  "Without a live chat session the fetch rejects and the client is released."
  (let (stopped reported)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-rollback--live-session-id) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-rollback-list)
               (lambda (&rest _) (error "Must not reach the RPC")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) reported))))
      (hermes-list-rollbacks)
      (should (eq stopped 'fake-client))
      (should (cl-some (lambda (m) (string-match-p "live chat session" m))
                       reported)))))

(ert-deftest hermes-rollback-diff-passes-session-id ()
  "The diff command threads the live session id into rollback.diff."
  (let (seen-session seen-hash)
    (cl-letf (((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "sid-live"))
              ((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-rollback-diff)
               (lambda (_client hash &rest args)
                 (setq seen-hash hash
                       seen-session (plist-get args :session-id))
                 (funcall (plist-get args :resolve) '((diff . "")))))
              ((symbol-function 'hermes-rollback--display-diff)
               (lambda (&rest _))))
      (with-temp-buffer
        (hermes-rollback-mode)
        (setq tabulated-list-entries '(("hash-1" ["hash-1" "" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-rollback-show-diff))
      (should (equal seen-hash "hash-1"))
      (should (equal seen-session "sid-live")))))

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

(ert-deftest hermes-subagents-rows-face-every-column ()
  "Subagent rows give every column its own face."
  (let* ((row (car (hermes-subagents--rows
                    '((active . (((subagent_id . "s0") (goal . "root")
                                  (status . "running") (model . "m")
                                  (tool_count . 2))))))))
         (entry (cadr row)))
    (should (eq (get-text-property 0 'face (aref entry 0))
                'hermes-browser-goal))
    (should (equal (get-text-property 0 'face (aref entry 1))
                   '(hermes-browser-active hermes-browser-status)))
    (should (eq (get-text-property 0 'face (aref entry 2))
                'hermes-browser-model))
    (should (eq (get-text-property 0 'face (aref entry 3))
                'hermes-browser-tool-count))))

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

;;; Group: desktop notifications

(ert-deftest hermes-browser-notify-uses-notifications-when-available ()
  "When D-Bus notifications exist, the helper forwards title and body."
  (let (got)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'notifications-notify)
               (lambda (&rest args) (setq got args) 1)))
      (should (hermes-browser--notify "T" "B"))
      (should (equal (plist-get got :title) "T"))
      (should (equal (plist-get got :body) "B")))))

(ert-deftest hermes-browser-notify-falls-back-to-message ()
  "Without notifications the helper degrades to a `message' and returns nil."
  (let (msg)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (not (eq feature 'notifications))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (should-not (hermes-browser--notify "T" "B"))
      (should (string-match-p "T: B" msg)))))

(ert-deftest hermes-profiles-rows-map-fields ()
  "Profile rows carry name, default marker, model, provider, and description."
  (let ((rows (hermes-profiles--rows
               '((profiles . (((name . "default") (is_default . t)
                               (model . "gpt-5.5") (provider . "openai")
                               (description . "main"))
                              ((name . "planner") (is_default . :false))))))))
    (should (equal (caar rows) "default"))
    (should (equal (aref (cadr (car rows)) 1) "*"))
    (should (equal (aref (cadr (car rows)) 2) "gpt-5.5"))
    (should (equal (aref (cadr (car rows)) 3) "openai"))
    (should (equal (aref (cadr (car rows)) 5) "main"))
    (should (equal (aref (cadr (cadr rows)) 1) ""))))

(ert-deftest hermes-profiles-rows-face-every-column ()
  "Profile rows give every column its own face."
  (let* ((row (car (hermes-profiles--rows
                    '((profiles . (((name . "default") (is_default . t)
                                    (model . "gpt-5.5")
                                    (provider . "openai")
                                    (description . "main"))))))))
         (entry (cadr row)))
    (should (eq (get-text-property 0 'face (aref entry 0))
                'hermes-browser-profile))
    (should (eq (get-text-property 0 'face (aref entry 1))
                'hermes-browser-default))
    (should (eq (get-text-property 0 'face (aref entry 2))
                'hermes-browser-model))
    (should (eq (get-text-property 0 'face (aref entry 3))
                'hermes-browser-provider))
    (should (eq (get-text-property 0 'face (aref entry 4))
                'hermes-browser-reasoning))
    (should (eq (get-text-property 0 'face (aref entry 5))
                'hermes-browser-description))))

(ert-deftest hermes-profiles-set-model-puts-provider-and-model ()
  "Setting a profile model PUTs provider+model to the profile route."
  (let (seen-method seen-path seen-body reverted)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((providers . (((slug . "openai") (name . "openai")
                                           (authenticated . t)
                                           (models . ("gpt-5.5")))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (car collection)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (setq seen-method method
                       seen-path path
                       seen-body (plist-get args :body))
                 (hermes--promise-resolved
                  '((ok . t) (model . "gpt-5.5") (provider . "openai")))))
              ((symbol-function 'hermes-profiles--revert)
               (lambda (&rest _) (setq reverted t))))
      (with-temp-buffer
        (hermes-profiles-mode)
        (setq tabulated-list-entries
              '(("planner" ["planner" "" "" "" "\u2014" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-profiles-set-model))
      (should (equal seen-method "PUT"))
      (should (equal seen-path "/api/profiles/planner/model"))
      (should (equal (cdr (assq 'provider seen-body)) "openai"))
      (should (equal (cdr (assq 'model seen-body)) "gpt-5.5"))
      (should reverted))))

(provide 'hermes-browsers-tests)
;;; hermes-browsers-tests.el ends here
