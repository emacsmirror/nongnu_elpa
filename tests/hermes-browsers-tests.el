;;; hermes-browsers-tests.el --- browsers tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(defvar hermes-browser-test--fetch-function nil)

(hermes-define-list-browser browseridentity
  :title "Hermes Browser Identity"
  :buffer "*Hermes Browser Identity*"
  :columns [("Name" 20 t)]
  :fetch (lambda (_client) (funcall hermes-browser-test--fetch-function))
  :rows (lambda (result)
          (mapcar (lambda (name) (list name (vector name))) result)))

(ert-deftest hermes-browser-command-pins-resolved-instance ()
  "A browser command resolves once and uses that instance for its client."
  (let ((instance '("remote" . "https://hermes.example.test"))
        (hermes-instances '(("local" . "http://127.0.0.1:9119")
                            ("remote" . "https://hermes.example.test")))
        (hermes-browser-test--fetch-function
         (lambda () (hermes--promise-resolved '("remote item"))))
        started-url)
    (cl-letf (((symbol-function 'hermes-instance-resolve)
               (lambda () instance))
              ((symbol-function 'hermes-browser--existing-client)
               (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _)
                 (setq started-url hermes-dashboard-transport-url)
                 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore))
      (unwind-protect
          (progn
            (hermes-list-browseridentity)
            (with-current-buffer "*Hermes Browser Identity*"
              (should (equal hermes-instance instance))
              (should (string-match-p
                       "remote" (hermes-browser--instance-header-line)))
              (should (equal started-url (hermes-instance-url instance)))))
        (when (get-buffer "*Hermes Browser Identity*")
          (kill-buffer "*Hermes Browser Identity*"))))))

(ert-deftest hermes-browser-existing-client-matches-buffer-instance ()
  "Passive client reuse is limited to the current buffer's instance."
  (let ((local '("local" . "http://127.0.0.1:9119"))
        (remote '("remote" . "https://hermes.example.test"))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test")))
        (local-client (hermes-test--dashboard-client))
        (remote-client (hermes-test--dashboard-client))
        buffers)
    (unwind-protect
        (progn
          (dolist (pair (list (cons local local-client)
                              (cons remote remote-client)))
            (let ((buffer (generate-new-buffer (hermes-test--chat-buffer-name))))
              (push buffer buffers)
              (with-current-buffer buffer
                (hermes-chat-mode)
                (setq hermes-instance (car pair)
                      hermes-chat--dashboard-client (cdr pair)))))
          (with-temp-buffer
            (setq hermes-instance remote)
            (should (eq (hermes-browser--existing-client) remote-client))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            buffers))))

(ert-deftest hermes-browser-existing-client-does-not-prompt-without-context ()
  "Passive client lookup returns nil when several instances are ambiguous."
  (let ((hermes-instances '(("local" . "http://127.0.0.1:9119")
                            ("remote" . "https://hermes.example.test")))
        (hermes-instance nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "Unexpected instance prompt"))))
      (should-not (hermes-browser--existing-client)))))

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

(ert-deftest hermes-subagents-interrupt-reports-finished-result ()
  "An interrupt result with `found' false does not report success."
  (let ((promise (hermes--promise-make)) messages refreshed)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'fake-client)
                                       on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-subagents--revert)
               (lambda (&rest _) (setq refreshed (current-buffer))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (hermes-subagents-mode)
        (setq tabulated-list-entries '(("s1" ["goal" "running" "m" "0"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-subagents-interrupt)
        (hermes--promise-resolve promise '((found . :false)))
        (should (eq refreshed (current-buffer))))
      (should-not (cl-some (lambda (text) (string-match-p "interrupted" text))
                           messages))
      (should (cl-some (lambda (text)
                         (string-match-p "already finished\\|not found" text))
                       messages)))))

(ert-deftest hermes-browser-revert-does-not-resurrect-killed-buffer ()
  "A late revert result does not recreate its killed browser buffer."
  (let ((promise (hermes--promise-make))
        (hermes-browser-test--fetch-function nil))
    (setq hermes-browser-test--fetch-function (lambda () promise))
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore))))
      (hermes-browseridentity--render '("initial"))
      (with-current-buffer "*Hermes Browser Identity*"
        (hermes-browseridentity--revert))
      (kill-buffer "*Hermes Browser Identity*")
      (hermes--promise-resolve promise '("late"))
      (should-not (get-buffer "*Hermes Browser Identity*")))))

(ert-deftest hermes-browser-revert-keeps-newest-result ()
  "An older refresh cannot overwrite rows from a newer refresh."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (requests 0)
        (hermes-browser-test--fetch-function nil))
    (setq hermes-browser-test--fetch-function
          (lambda ()
            (setq requests (1+ requests))
            (if (= requests 1) first second)))
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore))))
      (unwind-protect
          (progn
            (hermes-browseridentity--render '("initial"))
            (with-current-buffer "*Hermes Browser Identity*"
              (hermes-browseridentity--revert)
              (hermes-browseridentity--revert))
            (hermes--promise-resolve second '("new"))
            (hermes--promise-resolve first '("old"))
            (with-current-buffer "*Hermes Browser Identity*"
              (should (equal (mapcar #'car tabulated-list-entries) '("new")))))
        (when (get-buffer "*Hermes Browser Identity*")
          (kill-buffer "*Hermes Browser Identity*"))))))

(ert-deftest hermes-browser-late-rejections-respect-request-ownership ()
  "Replaced and killed list-browser requests cannot report late failures."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (requests 0)
        messages
        (hermes-browser-test--fetch-function nil))
    (setq hermes-browser-test--fetch-function
          (lambda ()
            (setq requests (1+ requests))
            (if (= requests 1) first second)))
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-browseridentity--render '("initial"))
            (with-current-buffer "*Hermes Browser Identity*"
              (hermes-browseridentity--revert)
              (hermes-browseridentity--revert))
            (hermes--promise-reject first "superseded failure")
            (kill-buffer "*Hermes Browser Identity*")
            (hermes--promise-reject second "orphaned failure")
            (should-not messages))
        (when (get-buffer "*Hermes Browser Identity*")
          (kill-buffer "*Hermes Browser Identity*"))))))

(ert-deftest hermes-browser-request-token-survives-mode-reset ()
  "A request token cannot become current again after changing modes twice."
  (with-temp-buffer
    (hermes-browseridentity-mode)
    (let ((old (hermes-browser--next-request-generation)))
      (fundamental-mode)
      (hermes-browseridentity-mode)
      (hermes-browser--next-request-generation)
      (should-not (hermes-browser--request-current-p (current-buffer) old)))))

(ert-deftest hermes-browser-retarget-invalidates-pending-request ()
  "Changing browser instance ownership invalidates pending requests."
  (let ((local '("local" . "http://127.0.0.1:9119"))
        (remote '("remote" . "https://hermes.example.test")))
    (with-temp-buffer
      (setq-local hermes-instance local)
      (let ((generation (hermes-browser--next-request-generation)))
        (hermes-browser--own-instance local)
        (should (hermes-browser--request-current-p
                 (current-buffer) generation))
        (hermes-browser--own-instance remote)
        (should-not (hermes-browser--request-current-p
                     (current-buffer) generation))))))

(ert-deftest hermes-browser-run-on-client-cleans-signalling-setup ()
  "A synchronous fetch setup error releases its transient client once."
  (let ((stops 0) reported)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq stops (1+ stops))))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq reported (apply #'format fmt args)))))
      (hermes-browser--run-on-client (lambda (_client) (error "setup failed")))
      (should (= stops 1))
      (should (equal reported "Hermes: setup failed")))))

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
            (with-current-buffer (get-buffer-create "*Hermes Browser Revert*")
              (hermes-browserrevert-mode)
              (hermes-browserrevert--revert))
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

(ert-deftest hermes-profiles-set-model-refreshes-origin-after-newer-read ()
  "A completed model update starts a fresh read in its originating profile buffer."
  (let ((put (hermes--promise-make)) refreshed)
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
               (lambda (&rest _) put))
              ((symbol-function 'hermes-profiles--revert)
               (lambda (&rest _) (setq refreshed (current-buffer))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-profiles-mode)
        (setq tabulated-list-entries
              '(("planner" ["planner" "" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (let ((origin (current-buffer)))
          (hermes-profiles-set-model)
          (hermes-browser--next-request-generation)
          (with-temp-buffer
            (hermes--promise-resolve
             put '((ok . t) (model . "gpt-5.5") (provider . "openai"))))
          (should (eq refreshed origin)))))))

(ert-deftest hermes-profiles-stale-model-catalog-cannot-prompt-or-put ()
  "A model catalog from instance A cannot act after retargeting to B."
  (let ((catalog (hermes--promise-make)) prompted put)
    (cl-letf (((symbol-function 'hermes-instance-resolve)
               (lambda () (or hermes-instance '("default" . "http://default"))))
              ((symbol-function 'hermes-browser--existing-client)
               (lambda () 'client-a))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) catalog))
              ((symbol-function 'hermes-profiles--read-model-candidate)
               (lambda (&rest _) (setq prompted t) '("p" . "m")))
              ((symbol-function 'hermes-profiles--put-model)
               (lambda (&rest _) (setq put t) (hermes--promise-resolved nil))))
      (with-temp-buffer
        (hermes-profiles-mode)
        (hermes-browser--own-instance '("a" . "http://a"))
        (setq tabulated-list-entries
              '(("planner" ["planner" "" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-profiles-set-model)
        (hermes-browser--own-instance '("b" . "http://b"))
        (hermes--promise-resolve catalog '((providers . nil)))))
    (should-not prompted)
    (should-not put)))

(ert-deftest hermes-profiles-lifecycle-uses-exact-rest-and-refreshes ()
  "Profile lifecycle commands use exact REST contracts and refresh on success."
  (let (requests (refreshes 0))
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (push (list method path (plist-get args :body)) requests)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'hermes-profiles--revert)
               (lambda (&rest _) (setq refreshes (1+ refreshes))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-profiles-mode)
        (hermes-profiles-create " worker ")
        (setq tabulated-list-entries
              '(("old/name" ["old/name" "" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-profiles-rename " new ")
        (hermes-profiles-delete)))
    (should (member '("POST" "/api/profiles" ((name . "worker"))) requests))
    (should (member '("PATCH" "/api/profiles/old%2Fname"
                      ((new_name . "new")))
                    requests))
    (should (member '("DELETE" "/api/profiles/old%2Fname" nil) requests))
    (should (= refreshes 3))))

(ert-deftest hermes-profiles-lifecycle-refuses-default-profile ()
  "Profile lifecycle commands refuse to mutate the built-in default profile."
  (let (requested)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (setq requested t)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (hermes-profiles-mode)
        (should-error (hermes-profiles-create " Default ") :type 'user-error)
        (setq tabulated-list-entries
              '(("default" ["default" "*" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (should-error (hermes-profiles-rename "renamed") :type 'user-error)
        (should-error (hermes-profiles-delete) :type 'user-error)
        (setq tabulated-list-entries
              '(("worker" ["worker" "" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (should-error (hermes-profiles-rename " DEFAULT ") :type 'user-error)))
    (should-not requested)))

(ert-deftest hermes-profiles-create-can-clone-existing-profile ()
  "Profile creation sends the backend's optional clone_from field."
  (let (seen-body)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (_method _path &rest args)
                 (setq seen-body (plist-get args :body))
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'hermes-profiles--revert) #'ignore)
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-profiles-mode)
        (hermes-profiles-create "worker" "planner")))
    (should (equal seen-body
                   '((name . "worker") (clone_from . "planner"))))))

(ert-deftest hermes-profiles-soul-get-and-put-use-exact-profile-route ()
  "SOUL editing loads and saves the selected non-default profile."
  (let (requests soul-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--existing-client)
                   (lambda () 'fake-client))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (method path &rest args)
                     (push (list method path (plist-get args :body)) requests)
                     (hermes--promise-resolved
                      (if (equal method "GET")
                          '((content . "You are precise.\n") (exists . t))
                        '((ok . t))))))
                  ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer))
                  ((symbol-function 'message) #'ignore))
          (with-temp-buffer
            (hermes-profiles-mode)
            (setq tabulated-list-entries
                  '(("planner" ["planner" "" "" "" "—" ""])))
            (tabulated-list-print)
            (goto-char (point-min))
            (hermes-profiles-edit-soul))
          (setq soul-buffer (get-buffer "*Hermes Profile SOUL: planner*"))
          (should (buffer-live-p soul-buffer))
          (with-current-buffer soul-buffer
            (should (equal (buffer-string) "You are precise.\n"))
            (goto-char (point-max))
            (insert "Stay brief.\n")
            (hermes-profiles-soul-save))
          (should (member '("GET" "/api/profiles/planner/soul" nil) requests))
          (should (member
                   '("PUT" "/api/profiles/planner/soul"
                     ((content . "You are precise.\nStay brief.\n")))
                   requests)))
      (when (buffer-live-p soul-buffer) (kill-buffer soul-buffer)))))

(ert-deftest hermes-profiles-soul-ignores-stale-and-killed-buffer-results ()
  "Late SOUL reads cannot overwrite a repurposed or killed editor buffer."
  (let ((read (hermes--promise-make)) target)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) read))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (with-temp-buffer
        (hermes-profiles-mode)
        (setq tabulated-list-entries
              '(("planner" ["planner" "" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-profiles-edit-soul))
      (setq target (get-buffer "*Hermes Profile SOUL: planner*"))
      (with-current-buffer target
        (setq hermes-profiles-soul-profile "other")
        (insert "new owner"))
      (hermes--promise-resolve read '((content . "stale") (exists . t)))
      (with-current-buffer target
        (should (equal (buffer-string) "new owner")))
      (kill-buffer target)
      (should-not (buffer-live-p target)))))

(ert-deftest hermes-profiles-soul-read-preserves-input-typed-while-loading ()
  "A late SOUL read does not overwrite user input typed after dispatch."
  (let ((read (hermes--promise-make)) target)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--existing-client)
                   (lambda () 'fake-client))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (&rest _) read))
                  ((symbol-function 'pop-to-buffer) #'ignore))
          (with-temp-buffer
            (hermes-profiles-mode)
            (setq tabulated-list-entries
                  '(("planner" ["planner" "" "" "" "—" ""])))
            (tabulated-list-print)
            (goto-char (point-min))
            (hermes-profiles-edit-soul))
          (setq target (get-buffer "*Hermes Profile SOUL: planner*"))
          (with-current-buffer target (insert "typed while loading"))
          (hermes--promise-resolve read '((content . "stale")))
          (with-current-buffer target
            (should (equal (buffer-string) "typed while loading"))))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest hermes-profiles-soul-buffers-are-instance-specific ()
  "The same profile on two instances uses two independently owned editors."
  (let* ((local '("local" . "http://127.0.0.1:9119"))
         (remote '("remote" . "https://hermes.example.test"))
         (hermes-instances (list local remote))
         buffers saved-instance saved-content)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--existing-client)
                   (lambda () 'fake-client))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (method _path &rest args)
                     (when (equal method "PUT")
                       (setq saved-instance hermes-instance
                             saved-content
                             (alist-get 'content (plist-get args :body))))
                     (hermes--promise-resolved '((content . "SOUL\n")))))
                  ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer)))
          (dolist (instance (list local remote))
            (with-temp-buffer
              (hermes-profiles-mode)
              (setq hermes-instance instance
                    tabulated-list-entries
                    '(("planner" ["planner" "" "" "" "—" ""])))
              (tabulated-list-print)
              (goto-char (point-min))
              (hermes-profiles-edit-soul)))
          (setq buffers
                (list (get-buffer "*Hermes Profile SOUL@local: planner*")
                      (get-buffer "*Hermes Profile SOUL@remote: planner*")))
          (should (cl-every #'buffer-live-p buffers))
          (should (equal (mapcar (lambda (buffer)
                                   (buffer-local-value 'hermes-instance buffer))
                                 buffers)
                         (list local remote)))
          (with-current-buffer (car buffers)
            (goto-char (point-max))
            (insert "local draft\n")
            (should (buffer-modified-p))
            (hermes-profiles-soul-save))
          (should (equal saved-instance local))
          (should (equal saved-content "SOUL\nlocal draft\n"))
          (let ((header (with-current-buffer (cadr buffers)
                          (hermes-profiles--soul-header-line))))
            (should (string-match-p "Hermes instance: remote" header))
            (should (string-match-p "Profile: planner" header))
            (should (string-match-p "C-c C-c save" header))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            buffers))))

(ert-deftest hermes-profiles-soul-refuses-default-profile ()
  "The built-in default profile has no editable SOUL surface."
  (let (requested)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (setq requested t)
                 (hermes--promise-resolved nil))))
      (with-temp-buffer
        (hermes-profiles-mode)
        (setq tabulated-list-entries
              '(("default" ["default" "*" "" "" "—" ""])))
        (tabulated-list-print)
        (goto-char (point-min))
        (should-error (hermes-profiles-edit-soul) :type 'user-error)))
    (should-not requested)))

(ert-deftest hermes-dashboard-keymap-reaches-profiles-browser ()
  "The main dashboard exposes profile management directly."
  (should (eq (lookup-key hermes-dashboard-mode-map (kbd "F"))
              #'hermes-list-profiles)))

(ert-deftest hermes-rollback-diff-ignores-stale-result ()
  "An older rollback diff cannot replace the result of a newer request."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (calls 0)
        displayed)
    (cl-letf (((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "session"))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (if (= calls 1) first second)))
              ((symbol-function 'hermes-rollback--display-diff)
               (lambda (_hash result)
                 (push (hermes-transport--get result 'diff) displayed))))
      (with-temp-buffer
        (hermes-rollback-mode)
        (setq tabulated-list-entries '(("abc" ["abc" "now" "message"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-rollback-show-diff)
        (hermes-rollback-show-diff)
        (hermes--promise-resolve second '((diff . "new")))
        (hermes--promise-resolve first '((diff . "old"))))
      (should (equal displayed '("new"))))))

(ert-deftest hermes-rollback-diff-ignores-killed-origin ()
  "A rollback diff response is ignored after its list buffer dies."
  (let ((promise (hermes--promise-make)) displayed)
    (cl-letf (((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "session"))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-rollback--display-diff)
               (lambda (&rest _) (setq displayed t))))
      (let ((origin (generate-new-buffer " *Hermes rollback origin*")))
        (with-current-buffer origin
          (hermes-rollback-mode)
          (setq tabulated-list-entries '(("abc" ["abc" "now" "message"])))
          (tabulated-list-print)
          (goto-char (point-min))
          (hermes-rollback-show-diff))
        (kill-buffer origin)
        (hermes--promise-resolve promise '((diff . "late")))
        (should-not displayed)))))

(ert-deftest hermes-rollback-restore-rejects-false-success ()
  "A rollback response declaring failure does not report success or refresh."
  (let (messages refreshed)
    (cl-letf (((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "session"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-catch
                  (hermes--promise-then (funcall make-promise 'client) on-success)
                  (lambda (reason) (push reason messages)))))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) (hermes--promise-resolved
                                   '((success . :false) (error . "denied")))))
              ((symbol-function 'hermes-rollback--revert)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (hermes-rollback-mode)
        (setq tabulated-list-entries '(("abc" ["abc" "now" "message"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-rollback-restore))
      (should-not refreshed)
      (should (cl-some (lambda (text) (string-match-p "denied" text)) messages))
      (should-not (cl-some (lambda (text) (string-match-p "restored" text)) messages)))))

(ert-deftest hermes-rollback-restore-refreshes-origin-on-success ()
  "A successful rollback starts a fresh read in its originating buffer."
  (let (refreshed)
    (cl-letf (((symbol-function 'hermes-rollback--live-session-id)
               (lambda () "session"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) (hermes--promise-resolved '((success . t)))))
              ((symbol-function 'hermes-rollback--revert)
               (lambda (&rest _) (setq refreshed (current-buffer))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-rollback-mode)
        (setq tabulated-list-entries '(("abc" ["abc" "now" "message"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (let ((origin (current-buffer)))
          (hermes-rollback-restore)
          (should (eq refreshed origin)))))))

(ert-deftest hermes-subagents-interrupt-refreshes-after-newer-read ()
  "A completed interrupt starts a fresh read despite an intervening refresh."
  (let ((promise (hermes--promise-make)) refreshed)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-dashboard-transport-call-fn)
               (lambda (&rest _) promise))
              ((symbol-function 'hermes-subagents--revert)
               (lambda (&rest _) (setq refreshed (current-buffer))))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-subagents-mode)
        (setq tabulated-list-entries '(("s1" ["goal" "running" "m" "0"])))
        (tabulated-list-print)
        (goto-char (point-min))
        (let ((origin (current-buffer)))
          (hermes-subagents-interrupt)
          (hermes-browser--next-request-generation)
          (hermes--promise-resolve promise '((found . t)))
          (should (eq refreshed origin)))))))

(provide 'hermes-browsers-tests)
;;; hermes-browsers-tests.el ends here
