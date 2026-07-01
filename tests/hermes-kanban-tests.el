;;; hermes-kanban-tests.el --- kanban tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-kanban-status-display-uses-shared-icons ()
  "Status display helpers share icons, labels, and raw status properties."
  (should (equal hermes-kanban--current-board-marker "📍"))
  (should (equal hermes-kanban--board-count-statuses
                 '("todo" "ready" "running" "blocked" "done" "archived")))
  (dolist (spec '(("todo" "📝")
                  ("ready" "✅")
                  ("running" "⚙️")
                  ("blocked" "⛔")
                  ("done" "🏁")
                  ("archived" "🗄️")))
    (pcase-let ((`(,status ,icon) spec))
      (should (equal (hermes-kanban--status-icon status) icon))
      (let ((formatted (hermes-kanban--format-status status)))
        (should (equal (substring-no-properties formatted)
                       (format "%s %s" icon status)))
        (should (equal (get-text-property 0 'hermes-kanban-status formatted)
                       status)))
      (let ((indicator (hermes-kanban--format-status-indicator status)))
        (should (equal (substring-no-properties indicator) icon))
        (should (equal (get-text-property 0 'hermes-kanban-status indicator)
                       status)))))
  (let ((running (hermes-kanban--format-status "running")))
    (should (equal (hermes-kanban--entry-status
                    (vector running "2" "elisp-dev" "Do thing"))
                   "running"))
    (should (equal (hermes-kanban--entry-status
                    (vector "⚙️ running" "2" "elisp-dev" "Do thing"))
                   "running")))
  (should (equal (hermes-kanban--format-status-count
                  '((ready . 2)) "ready")
                 "2"))
  (let* ((raw (copy-sequence "done"))
         (formatted (hermes-kanban--format-status raw)))
    (should (equal (substring-no-properties formatted) "🏁 done"))
    (should (equal (get-text-property 0 'hermes-kanban-status formatted)
                   "done"))
    (should-not (text-properties-at 0 raw))))

(ert-deftest hermes-kanban-tabulated-list-formats-scale-with-width ()
  "Kanban tabulated-list formats fit and flex by display width."
  (dolist (width '(30 40 50 80 120))
    (let ((boards (hermes-kanban--boards-tabulated-list-format width))
          (tasks (hermes-kanban--tasks-tabulated-list-format width)))
      (should (= (hermes-test--tabulated-list-format-total-width boards)
                 width))
      (should (<= (hermes-test--tabulated-list-format-total-width tasks)
                  width))
      (should (equal (car (aref boards 0)) ""))
      (should (equal (car (aref boards 1)) "📋"))
      (should (equal (car (aref boards 3)) "📝"))
      (should (>= (cadr (aref boards 0)) 1))
      (should (>= (cadr (aref boards 1)) 1))
      (should (>= (cadr (aref tasks 0)) 1))
      (should (>= (cadr (aref tasks 3)) 1))
      (should (<= (cadr (aref tasks 3))
                  hermes-kanban--task-title-column-max-width))
      (when (>= width 50)
        (should (>= (cadr (aref boards 1)) 12))
        (should (>= (cadr (aref boards 3)) 4))
        (should (>= (cadr (aref tasks 0)) 6))
        (should (>= (cadr (aref tasks 2)) 10))
        (should (>= (cadr (aref tasks 3)) 20)))))
  (let ((narrow-boards (hermes-kanban--boards-tabulated-list-format 50))
        (wide-boards (hermes-kanban--boards-tabulated-list-format 120))
        (narrow-tasks (hermes-kanban--tasks-tabulated-list-format 50))
        (wide-tasks (hermes-kanban--tasks-tabulated-list-format 120)))
    (should (< (cadr (aref narrow-boards 1))
               (cadr (aref wide-boards 1))))
    (should (< (cadr (aref narrow-boards 3))
               (cadr (aref wide-boards 3))))
    (should (< (cadr (aref narrow-tasks 2))
               (cadr (aref wide-tasks 2))))
    (should (< (cadr (aref narrow-tasks 3))
               (cadr (aref wide-tasks 3))))
    (should (= (cadr (aref (hermes-kanban--tasks-tabulated-list-format 200) 3))
               hermes-kanban--task-title-column-max-width))))

(ert-deftest hermes-kanban-window-size-change-recomputes-format ()
  "Kanban tabulated-list modes recompute widths when their window resizes."
  (dolist (mode '(hermes-kanban-boards-mode hermes-kanban-mode))
    (with-temp-buffer
      (funcall mode)
      (let (printed)
        (cl-letf (((symbol-function 'window-body-width)
                   (lambda (_window &optional _pixelwise) 120))
                  ((symbol-function 'tabulated-list-print)
                   (lambda (&rest _) (setq printed t))))
          (setq tabulated-list-format
                (if (derived-mode-p 'hermes-kanban-boards-mode)
                    (hermes-kanban--boards-tabulated-list-format 50)
                  (hermes-kanban--tasks-tabulated-list-format 50)))
          (hermes-kanban--window-size-change 'fake-window)
          (should printed)
          (let ((total (hermes-test--tabulated-list-format-total-width
                        tabulated-list-format)))
            (if (derived-mode-p 'hermes-kanban-boards-mode)
                (should (= total 120))
              (should (<= total 120))
              (should (<= (cadr (aref tabulated-list-format 3))
                          hermes-kanban--task-title-column-max-width)))))))))

(ert-deftest hermes-kanban-board-rows-from-boards ()
  "Board rows map name/total/per-status counts and mark the current board."
  (cl-labels ((column-for (status)
                (+ 3 (cl-position status hermes-kanban--board-count-statuses
                                  :test #'equal))))
    (let* ((rows (hermes-kanban--board-rows
                  '(((slug . "emacs-lisp") (name . "Emacs Lisp")
                     (is_current . t) (total . 4)
                     (counts . ((todo . 1) (running . 2)
                                (archived . 1)))))))
           (entry (cadr (car rows))))
      (should (equal (caar rows) (cons "emacs-lisp" "Emacs Lisp")))
      (should (equal (aref entry 0) "📍"))
      (should (equal (aref entry 1) "Emacs Lisp"))
      (should (equal (aref entry 2) "4"))
      (should (equal (aref entry (column-for "todo")) "1"))
      (should (equal (aref entry (column-for "running")) "2"))
      (should (equal (aref entry (column-for "archived")) "1")))))

(ert-deftest hermes-kanban-task-rows-from-columns ()
  "Task rows flatten dashboard status columns into status/pri/assignee/title."
  (let* ((title "Do thing with a long title that tabulated-list truncates by column")
         (rows (hermes-kanban--task-rows
                `(((name . "todo")
                   (tasks . (((id . "t1") (status . "todo") (priority . 2)
                              (assignee . "elisp-dev") (title . ,title)
                              (created_at . 1000)))))
                  ((name . "running") (tasks . nil))))))
    (should (equal (caar rows) "t1"))
    (should (= (length rows) 1))
    (let ((status (aref (cadr (car rows)) 0)))
      (should (equal (substring-no-properties status) "📝"))
      (should (equal (get-text-property 0 'hermes-kanban-status status)
                     "todo")))
    (should (equal (aref (cadr (car rows)) 1) "2"))
    (should (equal (aref (cadr (car rows)) 2) "elisp-dev"))
    (should (equal (aref (cadr (car rows)) 3) title))))

(ert-deftest hermes-kanban-task-rows-sort-newest-first ()
  "Task rows are sorted by `created_at' descending across all status columns."
  (let* ((columns
          `(((name . "done")
             (tasks . (((id . "old") (status . "done") (priority . 3)
                        (title . "Oldest") (created_at . 1000))
                       ((id . "mid") (status . "done") (priority . 5)
                        (title . "Middle") (created_at . 2000)))))
            ((name . "todo")
             (tasks . (((id . "new") (status . "todo") (priority . 1)
                        (title . "Newest") (created_at . 3000))
                       ((id . "newer2") (status . "todo") (priority . 2)
                        (title . "Second") (created_at . 2500)))))))
         (ids (mapcar #'car (hermes-kanban--task-rows columns))))
    ;; Newest created_at first, regardless of the backend's status column order
    ;; (backend returns "done" before "todo" here).
    (should (equal ids '("new" "newer2" "mid" "old")))))

(ert-deftest hermes-kanban-task-rows-missing-created-at-sorts-oldest ()
  "Tasks with missing or non-numeric `created_at' sort after dated ones."
  (let* ((columns
          `(((name . "todo")
             (tasks . (((id . "dated") (status . "todo") (priority . 1)
                        (title . "Dated") (created_at . 1000))
                       ((id . "missing") (status . "todo") (priority . 2)
                         (title . "No timestamp"))
                       ((id . "string-ts") (status . "todo") (priority . 3)
                        (title . "Bad timestamp") (created_at . "oops")))))))
         (rows (hermes-kanban--task-rows columns))
         (ids (mapcar #'car rows)))
    ;; Dated first, then the two timestamp-less tasks in input order (stable).
    (should (equal ids '("dated" "missing" "string-ts")))))

(ert-deftest hermes-kanban-render-boards-lists-boards ()
  "The boards overview fetches /boards and renders one row per board."
  (cl-letf (((symbol-function 'hermes-kanban--api)
             (lambda (method path &optional _body _query)
               (should (equal method "GET"))
               (should (equal path "/boards"))
               (hermes--promise-resolved '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
						       (is_current . t) (total . 1)
						       (counts . ((ready . 1)))))))))))
    (unwind-protect
        (progn
          (hermes-list-kanban)
          (with-current-buffer "*Hermes Kanban Boards*"
            (should (derived-mode-p 'hermes-kanban-boards-mode))
            (should (equal (car (aref tabulated-list-format 1)) "📋"))
            (should (equal (caar tabulated-list-entries)
                           (cons "emacs-lisp" "Emacs Lisp")))))
      (when (get-buffer "*Hermes Kanban Boards*")
        (kill-buffer "*Hermes Kanban Boards*")))))

(ert-deftest hermes-kanban-boards-revert-refreshes-without-display ()
  "Reverting the boards overview refreshes in place; the command displays."
  (let (displayed)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
                                (is_current . t) (total . 1)
                                (counts . ((ready . 1))))))))))
              ((symbol-function 'pop-to-buffer)
               (lambda (&rest _) (setq displayed t))))
      (unwind-protect
          (progn
            (hermes-kanban--boards-revert)
            (should-not displayed)
            (with-current-buffer "*Hermes Kanban Boards*"
              (should (equal (caar tabulated-list-entries)
                             (cons "emacs-lisp" "Emacs Lisp"))))
            (hermes-list-kanban)
            (should displayed))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-board-actions-dispatch-rest-calls ()
  "Board overview actions use REST endpoints, safe archive, and refresh."
  (let (calls prompts)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved (pcase path
					     ("/boards"
					      '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
							    (is_current . t) (total . 1)
							    (counts . ((ready . 1))))))))
					     ("/boards/emacs-lisp/switch" '((current . "emacs-lisp")))
					     ("/boards/emacs-lisp" '((board . ((slug . "emacs-lisp")
									       (name . "Renamed")))))
					     (_ (error "unexpected path: %s" path))))))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (push prompt prompts)
                 t))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (hermes-kanban-switch-board)
              (goto-char (point-min))
              (hermes-kanban-rename-board " Renamed ")
              (goto-char (point-min))
              (hermes-kanban-archive-board))
            (should (member '("POST" "/boards/emacs-lisp/switch" nil nil)
                            calls))
            (should (member '("PATCH" "/boards/emacs-lisp"
                              ((name . "Renamed")) nil)
                            calls))
            (should (member '("DELETE" "/boards/emacs-lisp" nil nil)
                            calls))
            (should (= (cl-count-if (lambda (call)
                                      (equal (cadr call) "/boards"))
                                    calls)
                       4))
            (should (= (length prompts) 2))
            (should (cl-some (lambda (prompt)
                               (string-match-p "current board" prompt))
                             prompts))
            (should (cl-some (lambda (prompt)
                               (string-match-p "hard delete" prompt))
                             prompts)))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-rename-board-rejects-blank-name ()
  "Whitespace-only board renames signal before PATCH or refresh."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 (hermes--promise-resolved '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
							 (is_current . t) (total . 1))))))))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (should-error (hermes-kanban-rename-board "   ")
                            :type 'user-error))
            (should-not calls))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-archive-current-board-cancel-stops-before-delete ()
  "Declining the current-board archive prompt skips DELETE and refresh."
  (let (calls prompts)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 (hermes--promise-resolved '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
							 (is_current . t) (total . 1))))))))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (push prompt prompts)
                 nil))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (should-error (hermes-kanban-archive-board)
                            :type 'user-error))
            (should-not calls)
            (should (= (length prompts) 1))
            (should (string-match-p "current board" (car prompts))))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-archive-default-board-stops-before-prompt ()
  "The protected default board is rejected before prompts or DELETE."
  (let (calls prompted)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 (hermes--promise-resolved '((boards . (((slug . "default") (name . "Default")
							 (is_current . t) (total . 1))))))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (setq prompted t)
                 t))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (let ((err (should-error (hermes-kanban-archive-board)
                                       :type 'user-error)))
                (should (string-match-p "protected.*cannot be archived"
                                        (error-message-string err)))))
            (should-not calls)
            (should-not prompted))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-archive-board-cancel-skips-delete ()
  "Declining the normal archive prompt skips DELETE and refresh."
  (let (calls prompts)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 (hermes--promise-resolved '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
							 (total . 1))))))))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (push prompt prompts)
                 nil))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (should-not (hermes-kanban-archive-board)))
            (should-not calls)
            (should (= (length prompts) 1))
            (should (string-match-p "hard delete" (car prompts))))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-open-board-renders-tasks ()
  "Opening a board fetches /board with its slug and flattens the columns."
  (cl-letf (((symbol-function 'window-body-width)
             (lambda (&optional _window _pixelwise) 80))
            ((symbol-function 'hermes-kanban--api)
             (lambda (method path &optional _body query)
               (should (equal method "GET"))
               (should (equal path "/board"))
               (should (equal (cdr (assq 'board query)) "emacs-lisp"))
               (hermes--promise-resolved '((columns . (((name . "todo")
							(tasks . (((id . "t1") (status . "todo")
								   (title . "Do thing")))))))
					   (assignees . ("elisp-dev")))))))
    (unwind-protect
        (progn
          (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
          (with-current-buffer "*Hermes Kanban*"
            (should (derived-mode-p 'hermes-kanban-mode))
            (should (equal hermes-kanban--slug "emacs-lisp"))
            (should (equal hermes-kanban--assignees '("elisp-dev")))
            (should (= (hermes-test--tabulated-list-format-total-width
                        tabulated-list-format)
                       80))
            (should (>= (cadr (aref tabulated-list-format 0)) 6))
            (should (>= (cadr (aref tabulated-list-format 3)) 20))
            (should (equal (caar tabulated-list-entries) "t1"))))
      (when (get-buffer "*Hermes Kanban*") (kill-buffer "*Hermes Kanban*")))))

(ert-deftest hermes-kanban-show-fetches-task-at-point ()
  "Showing fetches the task on the current row and renders its body."
  (let (show-path)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (_method path &optional _body _query)
                 (hermes--promise-resolved (cond
					    ((equal path "/board")
					     '((columns . (((name . "todo")
							    (tasks . (((id . "t1") (status . "todo")
								       (title . "Do thing")))))))
					       (assignees)))
					    (t (setq show-path path)
					       '((task . ((id . "t1") (title . "Do thing") (status . "todo")
							  (body . "details here"))))))))))
      (unwind-protect
          (progn
            (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
            (with-current-buffer "*Hermes Kanban*"
              (goto-char (point-min))
              (hermes-kanban-show))
            (should (equal show-path "/tasks/t1"))
            (with-current-buffer "*Hermes Kanban Task*"
              (should (derived-mode-p 'hermes-kanban-task-mode))
              (when (require 'markdown-mode nil t)
                (should (derived-mode-p 'markdown-mode)))
              (should buffer-read-only)
              (should (equal hermes-kanban-task--task-id "t1"))
              (should (string-match-p "## Description" (buffer-string)))
              (should (string-match-p "details here" (buffer-string)))))
        (dolist (b '("*Hermes Kanban*" "*Hermes Kanban Task*"))
          (when (get-buffer b) (kill-buffer b)))))))

(ert-deftest hermes-kanban-format-task-detail-renders-markdown-sections ()
  "Task detail formatting includes Markdown task sections and rows."
  (let* ((payload
          '((task . ((id . "t1") (title . "Do thing") (status . "running")
                     (priority . 5) (assignee . "elisp-dev")
                     (created_at . 1700000000)
                     (body . "details here")
                     (diagnostics . (((kind . "stale_running")
                                      (severity . "error")
                                      (title . "Stale worker")
                                      (detail . "No heartbeat")
                                      (count . 2)
                                      (run_id . 7)
                                      (actions . (((kind . "reclaim")
                                                   (label . "Reclaim")
                                                   (suggested . t)))))))))
            (comments . (((id . 1) (author . "thanos")
                          (body . "needs eyes") (created_at . 1700000010))))
            (events . (((id . 2) (kind . "claimed")
                        (created_at . 1700000020)
                        (payload . ((run_id . 7))))))
            (attachments . (((id . 5) (filename . "report.txt")
                             (content_type . "text/plain") (size . 42)
                             (uploaded_by . "dashboard")
                             (stored_path . "/tmp/report.txt"))))
            (runs . (((id . 7) (profile . "elisp-dev")
                      (status . "finished") (outcome . "blocked")
                      (worker_pid . 1234)
                      (started_at . 1700000000) (ended_at . 1700000060)
                      (summary . "needs review")
                      (metadata . ((tests . 3)))
                      (error . "review-required"))))))
         (text (hermes-kanban--format-task-detail payload)))
    (should (string-match-p (regexp-quote "# Do thing") text))
    (should (string-match-p (regexp-quote "- Status: `⚙️ running`") text))
    (should (string-match-p "## Description" text))
    (should (string-match-p "## Run history (1)" text))
    (should (string-match-p (regexp-quote "### Run #7 — blocked @elisp-dev") text))
    (should (string-match-p "needs review" text))
    (should (string-match-p "tests" text))
    (should (string-match-p "## Diagnostics (1)" text))
    (should (string-match-p (regexp-quote "### [error] stale_running: Stale worker") text))
    (should (string-match-p "Reclaim" text))
    (should (string-match-p "## Attachments (1)" text))
    (should (string-match-p (regexp-quote "### report.txt (#5) (42 B)") text))
    (should (string-match-p "/tmp/report.txt" text))
    (should (string-match-p "## Comments (1)" text))
    (should (string-match-p (regexp-quote "— thanos") text))
    (should (string-match-p "needs eyes" text))
    (should (string-match-p "## Events (1)" text))
    (should (string-match-p (regexp-quote "— claimed") text))
    (should (string-match-p "Payload:" text))))

(ert-deftest hermes-kanban-format-task-detail-renders-empty-states ()
  "Task detail formatting names empty Markdown sections instead of omitting them."
  (let ((text (hermes-kanban--format-task-detail
               '((task . ((id . "t-empty") (title . "Empty task")
                          (status . "todo") (body . "")))
                 (comments) (events) (attachments) (runs)))))
    (should (string-match-p "## Diagnostics (0)" text))
    (should (string-match-p "— no diagnostics —" text))
    (should (string-match-p "## Attachments (0)" text))
    (should (string-match-p "— no attachments —" text))
    (should (string-match-p "## Comments (0)" text))
    (should (string-match-p "— no comments —" text))
    (should (string-match-p "## Events (0)" text))
    (should (string-match-p "— no events —" text))
    (should (string-match-p "## Run history (0)" text))
    (should (string-match-p "— no runs —" text))))

(ert-deftest hermes-kanban-format-task-shows-failure-fields ()
  "A distressed task surfaces branch, run, failure count, and last error."
  (let ((text (hermes-kanban--format-task
               '((id . "t9") (title . "Flaky") (status . "running")
                 (priority . 3) (assignee . "elisp-dev")
                 (created_at . 1700000000)
                 (branch_name . "feat/flaky")
                 (current_run_id . 42)
                 (consecutive_failures . 2)
                 (last_failure_error . "worker crashed")))))
    (should (string-match-p (regexp-quote "- Branch: `feat/flaky`") text))
    (should (string-match-p (regexp-quote "- Run: `#42`") text))
    (should (string-match-p (regexp-quote "- Failures: 2") text))
    (should (string-match-p (regexp-quote "- Last error: worker crashed") text))))

(ert-deftest hermes-kanban-format-task-hides-healthy-failure-fields ()
  "A healthy task adds no branch, run, failure, or error lines."
  (let ((text (hermes-kanban--format-task
               '((id . "t1") (title . "Fine") (status . "todo")
                 (priority . 5) (created_at . 1700000000)
                 (consecutive_failures . 0) (last_failure_error . nil)))))
    (should-not (string-match-p "- Branch:" text))
    (should-not (string-match-p "- Run:" text))
    (should-not (string-match-p "- Failures:" text))
    (should-not (string-match-p "- Last error:" text))))

(ert-deftest hermes-kanban-format-failure-fields-renders-present-only ()
  "Only present fields render; a lone branch yields just the branch line."
  (should (equal "" (hermes-kanban--format-failure-fields
                     '((consecutive_failures . 0)))))
  (should (equal "- Branch: `main`\n"
                 (hermes-kanban--format-failure-fields
                  '((branch_name . "main") (consecutive_failures . 0))))))

;;; Group N: recovery actions

(ert-deftest hermes-kanban-run-id-for-task-reads-current-run ()
  "The run id comes off the task's current_run_id; absent ids yield nil."
  (should (equal 7 (hermes-kanban--run-id-for-task '((current_run_id . 7)))))
  (should-not (hermes-kanban--run-id-for-task '((current_run_id))))
  (should-not (hermes-kanban--run-id-for-task '((id . "t1")))))

(ert-deftest hermes-kanban-reason-body-omits-empty-reason ()
  "A nil reason drops the body; a reason becomes a one-key alist."
  (should-not (hermes-kanban--reason-body nil))
  (should (equal '((reason . "stuck")) (hermes-kanban--reason-body "stuck"))))

(ert-deftest hermes-kanban-read-reason-trims-and-nils-blank ()
  "A blank reason reads as nil; surrounding whitespace is trimmed."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
    (should-not (hermes-kanban--read-reason "Reason: ")))
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  boom ")))
    (should (equal "boom" (hermes-kanban--read-reason "Reason: ")))))

(ert-deftest hermes-kanban-terminate-run-without-run-reports-and-skips ()
  "A task with no active run is reported and never hits the terminate endpoint."
  (let (calls msgs)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved nil)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
      (hermes-kanban--terminate-run-for-task '((id . "t1")) "t1" nil #'ignore)
      (should-not calls)
      (should (cl-some (lambda (m) (string-match-p "no active run" m)) msgs)))))

(ert-deftest hermes-kanban-terminate-run-posts-to-run-endpoint ()
  "Confirming terminates the resolved run id, omitting an empty reason."
  (let (calls refreshed)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (hermes-kanban--terminate-run-for-task
       '((id . "t1") (current_run_id . 42)) "t1" '((board . "emacs-lisp"))
       (lambda () (setq refreshed t)))
      (should (member '("POST" "/runs/42/terminate" nil ((board . "emacs-lisp")))
                      calls))
      (should refreshed))))

(ert-deftest hermes-kanban-comment-posts-from-task-detail-buffer ()
  "Commenting from the task detail view posts to the task and refreshes."
  (let (calls refreshed)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'read-string-from-buffer)
               (lambda (prompt initial)
                 (should (equal prompt "Comment: "))
                 (should (equal initial ""))
                 "looks good"))
              ((symbol-function 'revert-buffer) (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (with-temp-buffer
        (hermes-kanban-task-mode)
        (setq hermes-kanban-task--task-id "t1"
              hermes-kanban-task--board-slug "emacs-lisp")
        (hermes-kanban-comment)
        (should (member '("POST" "/tasks/t1/comments" ((body . "looks good"))
                          ((board . "emacs-lisp")))
                        calls))
        (should refreshed)))))

(ert-deftest hermes-kanban-reclaim-posts-to-reclaim-endpoint ()
  "Reclaiming the task at point POSTs reclaim with the board query and reason."
  (let (calls)
    (cl-letf (((symbol-function 'window-body-width)
               (lambda (&optional _w _p) 80))
              ((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved
                  (if (equal path "/board")
                      '((columns . (((name . "running")
                                     (tasks . (((id . "t1") (status . "running")
                                                (title . "Do thing")))))))
                        (assignees))
                    '((ok . t))))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'read-string) (lambda (&rest _) "stuck"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
            (with-current-buffer "*Hermes Kanban*"
              (goto-char (point-min))
              (hermes-kanban-reclaim))
            (should (member '("POST" "/tasks/t1/reclaim"
                              ((reason . "stuck")) ((board . "emacs-lisp")))
                            calls)))
        (when (get-buffer "*Hermes Kanban*") (kill-buffer "*Hermes Kanban*"))))))

;;; Group N: diagnostics overview

(ert-deftest hermes-kanban-diagnostic-summary-counts-extra ()
  "A single diagnostic shows its title; extras add a (+N more) suffix."
  (should (equal "No heartbeat"
                 (hermes-kanban--diagnostic-summary '((title . "No heartbeat")) 1)))
  (should (equal "No heartbeat (+2 more)"
                 (hermes-kanban--diagnostic-summary '((title . "No heartbeat")) 3))))

(ert-deftest hermes-kanban-diagnostic-row-uses-top-and-falls-back ()
  "A row carries the task id, top severity, title, assignee, and summary."
  (let ((row (hermes-kanban--diagnostic-row
              '((task_id . "t1") (task_title . "Stuck task")
                (task_assignee . "elisp-dev")
                (diagnostics . [((severity . "critical") (title . "No heartbeat"))
                                ((severity . "warning") (title . "Retried"))])))))
    (should (equal "t1" (car row)))
    (should (equal ["critical" "Stuck task" "elisp-dev" "No heartbeat (+1 more)"]
                   (cadr row))))
  (let ((row (hermes-kanban--diagnostic-row
              '((task_id . "t2") (task_title . "")
                (diagnostics . [((severity . "warning") (title . "Slow"))])))))
    (should (equal ["warning" "t2" "-" "Slow"] (cadr row)))))

(ert-deftest hermes-kanban-diagnostic-rows-tolerates-missing-optionals ()
  "Rows build from groups whose diagnostics omit run_id and data."
  (let ((rows (hermes-kanban--diagnostic-rows
               [((task_id . "t1") (task_title . "A")
                 (diagnostics . [((severity . "error") (title . "X"))]))
                ((task_id . "t2") (task_title . "B")
                 (diagnostics . [((severity . "warning") (title . "Y"))]))])))
    (should (equal '("t1" "t2") (mapcar #'car rows)))))

(ert-deftest hermes-kanban-render-diagnostics-lists-tasks ()
  "Rendering fetches /diagnostics with the board query and lists distressed tasks."
  (let (query)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional _body q)
                 (should (equal method "GET"))
                 (should (equal path "/diagnostics"))
                 (setq query q)
                 (hermes--promise-resolved
                  '((diagnostics . [((task_id . "t1") (task_title . "Stuck")
                                     (task_assignee . "elisp-dev")
                                     (diagnostics . [((severity . "critical")
                                                      (title . "No heartbeat"))]))])
                    (count . 1)))))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-diagnostics "emacs-lisp" "Emacs Lisp")
            (should (equal (cdr (assq 'board query)) "emacs-lisp"))
            (with-current-buffer "*Hermes Kanban Diagnostics*"
              (should (derived-mode-p 'hermes-kanban-diagnostics-mode))
              (should (equal hermes-kanban--slug "emacs-lisp"))
              (should (equal (caar tabulated-list-entries) "t1"))))
        (when (get-buffer "*Hermes Kanban Diagnostics*")
          (kill-buffer "*Hermes Kanban Diagnostics*"))))))

(ert-deftest hermes-kanban-render-diagnostics-reports-empty-board ()
  "An empty board renders no rows and reports the empty state."
  (let (msgs)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (_m _p &optional _b _q)
                 (hermes--promise-resolved '((diagnostics . []) (count . 0)))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
      (unwind-protect
          (progn
            (hermes-kanban--render-diagnostics "emacs-lisp" "Emacs Lisp")
            (with-current-buffer "*Hermes Kanban Diagnostics*"
              (should-not tabulated-list-entries))
            (should (cl-some (lambda (m) (string-match-p "No active diagnostics" m))
                             msgs)))
        (when (get-buffer "*Hermes Kanban Diagnostics*")
          (kill-buffer "*Hermes Kanban Diagnostics*"))))))

(defun hermes-kanban-test--face-match-p (face expected)
  "Return non-nil when FACE contains EXPECTED."
  (cond
   ((eq face expected) t)
   ((listp face) (memq expected face))))

(defun hermes-kanban-test--line-has-face-p (text line expected)
  "Return non-nil when LINE in TEXT has EXPECTED face on any character."
  (when-let* ((start (string-match (regexp-quote line) text)))
    (cl-loop for i from start below (+ start (length line))
             thereis (hermes-kanban-test--face-match-p
                      (get-text-property i 'face text)
                      expected))))

(ert-deftest hermes-kanban-show-log-fetches-selected-task-log ()
  "Log viewing goes through the dashboard REST endpoint for the selected task."
  (let (log-path log-query)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional _body query)
                 (should (equal method "GET"))
                 (hermes--promise-resolved (cond
					    ((equal path "/board")
					     '((columns . (((name . "running")
							    (tasks . (((id . "t1") (status . "running")
								       (title . "Do thing")))))))
					       (assignees . ("elisp-dev"))))
					    ((equal path "/tasks/t1/log")
					     (setq log-path path
						   log-query query)
					     '((task_id . "t1") (path . "/logs/t1.log")
					       (exists . t) (size_bytes . 12)
					       (content . "hello from worker\n")
					       (truncated . :json-false)))
					    (t (error "unexpected path: %s" path)))))))
      (unwind-protect
          (progn
            (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
            (with-current-buffer "*Hermes Kanban*"
              (goto-char (point-min))
              (hermes-kanban-show-log))
            (should (equal log-path "/tasks/t1/log"))
            (should (equal (cdr (assq 'board log-query)) "emacs-lisp"))
            (should (equal (cdr (assq 'tail log-query)) 100000))
            (with-current-buffer "*Hermes Kanban Log*"
              (should (derived-mode-p 'hermes-kanban-log-mode))
              (should (equal hermes-kanban-log--task-id "t1"))
              (should (equal hermes-kanban-log--board-slug "emacs-lisp"))
              (let ((text (buffer-string)))
                (should (string-match-p "Worker log for t1" text))
                (should (string-match-p "/logs/t1.log" text))
                (should (string-match-p "hello from worker" text)))))
        (dolist (b '("*Hermes Kanban*" "*Hermes Kanban Log*"))
          (when (get-buffer b) (kill-buffer b)))))))

(ert-deftest hermes-kanban-format-log-renders-empty-and-error-states ()
  "Worker log formatting is explicit when the backend reports no log or an error."
  (should (string-match-p "no worker log"
                          (hermes-kanban--format-log
                           '((task_id . "t1") (exists . :json-false)
                             (content . "")))))
  (should (string-match-p "failed to load worker log: boom"
                          (hermes-kanban--format-log
                           '((task_id . "t1") (error . "boom"))))))

(ert-deftest hermes-kanban-format-log-sanitizes-control-output ()
  "Worker log formatting renders CR and ANSI control output readably."
  (let* ((text (hermes-kanban--format-log
                `((task_id . "t1") (exists . t)
                  (content . ,(concat "start\rprogress\r\ndone\n"
                                      "\33[31merror\33[0m\n")))))
         (plain (substring-no-properties text)))
    (should-not (string-match-p "\r" plain))
    (should-not (string-match-p (regexp-quote "\33[") plain))
    (should (string-match-p "start\nprogress\ndone" plain))
    (should (string-match-p "error" plain))))

(ert-deftest hermes-kanban-format-log-fontifies-embedded-diff ()
  "Worker log formatting applies diff faces to embedded unified diffs."
  (let* ((content (concat "before diff\n"
                          "a//lisp/foo.el → b//lisp/foo.el\n"
                          "@@ -17,2 +17,2 @@\n"
                          " context\n"
                          "-old\n"
                          "+new\n"
                          "middle diff\n"
                          "diff --git a/lisp/bar.el b/lisp/bar.el\n"
                          "--- a/lisp/bar.el\n"
                          "+++ b/lisp/bar.el\n"
                          "@@ -1 +1 @@\n"
                          "-before\n"
                          "+after\n"
                          "after diff\n"))
         (text (hermes-kanban--format-log
                `((task_id . "t1") (exists . t) (content . ,content))))
         (plain (substring-no-properties text)))
    (should (string-match-p "before diff" plain))
    (should (string-match-p (regexp-quote "@@ -17,2 +17,2 @@") plain))
    (should (string-match-p "-old" plain))
    (should (string-match-p "\\+new" plain))
    (should (string-match-p (regexp-quote "@@ -1 +1 @@") plain))
    (should (string-match-p "-before" plain))
    (should (string-match-p "\\+after" plain))
    (should (string-match-p "after diff" plain))
    (should (hermes-kanban-test--line-has-face-p
             text "@@ -17,2 +17,2 @@" 'diff-hunk-header))
    (should (hermes-kanban-test--line-has-face-p
             text "-old" 'diff-indicator-removed))
    (should (hermes-kanban-test--line-has-face-p
             text "-old" 'diff-removed))
    (should (hermes-kanban-test--line-has-face-p
             text "+new" 'diff-indicator-added))
    (should (hermes-kanban-test--line-has-face-p
             text "+new" 'diff-added))
    (should (hermes-kanban-test--line-has-face-p
             text "@@ -1 +1 @@" 'diff-hunk-header))
    (should (hermes-kanban-test--line-has-face-p
             text "-before" 'diff-removed))
    (should (hermes-kanban-test--line-has-face-p
             text "+after" 'diff-added))))

(ert-deftest hermes-kanban-format-log-does-not-fontify-ordinary-plus-minus-lines ()
  "Worker log formatting ignores ordinary plus/minus lines without hunks."
  (let* ((content (concat "worker said\n"
                          "+not a diff addition\n"
                          "-not a diff removal\n"
                          "@@ -1 +1 @@\n"
                          "-incomplete hunk\n"))
         (text (hermes-kanban--format-log
                `((task_id . "t1") (exists . t) (content . ,content))))
         (plain (substring-no-properties text)))
    (should (string-match-p "\\+not a diff addition" plain))
    (should (string-match-p "-not a diff removal" plain))
    (should (string-match-p (regexp-quote "@@ -1 +1 @@") plain))
    (should (string-match-p "-incomplete hunk" plain))
    (dolist (face '(diff-added diff-indicator-added diff-removed
                    diff-indicator-removed diff-hunk-header))
      (should-not (hermes-kanban-test--line-has-face-p
                   text "+not a diff addition" face))
      (should-not (hermes-kanban-test--line-has-face-p
                   text "-not a diff removal" face))
      (should-not (hermes-kanban-test--line-has-face-p
                   text "@@ -1 +1 @@" face))
      (should-not (hermes-kanban-test--line-has-face-p
                   text "-incomplete hunk" face)))))

(ert-deftest hermes-kanban-log-mode-navigates-embedded-diff-hunks ()
  "Log-mode n/p commands move across embedded unified diff hunks.
Incomplete header-shaped blocks that the fontifier rejects are skipped."
  (with-temp-buffer
    (hermes-kanban-log-mode)
    (let ((inhibit-read-only t))
      (insert (hermes-kanban--render-log-content
               (concat "worker said\n"
                       ;; Incomplete hunk-shaped block: a header that
                       ;; announces one old and one new line, but the
                       ;; following lines are not +/- body lines, so
                       ;; `hermes-kanban--consume-diff-hunk' rejects it
                       ;; and the fontifier does not fontify it.
                       "@@ -1 +1 @@\n"
                       "this is just prose, not a diff body\n"
                       "a//lisp/foo.el → b//lisp/foo.el\n"
                       "@@ -1 +1 @@\n"
                       "-old\n"
                       "+new\n"
                       "between\n"
                       "@@ -5 +5 @@\n"
                       "-alpha\n"
                       "+beta\n"))))
    (should (eq (lookup-key hermes-kanban-log-mode-map (kbd "n"))
                'hermes-kanban-log-next-hunk))
    (should (eq (lookup-key hermes-kanban-log-mode-map (kbd "p"))
                'hermes-kanban-log-previous-hunk))
    (goto-char (point-min))
    ;; `n' must skip the incomplete header block at the top and land on
    ;; the first VALID hunk inside the fontified diff.
    (hermes-kanban-log-next-hunk)
    (should (looking-at (regexp-quote "@@ -1 +1 @@")))
    (let ((first-hunk (point)))
      (hermes-kanban-log-next-hunk)
      (should (looking-at (regexp-quote "@@ -5 +5 @@")))
      ;; `p' must also skip the incomplete block and land back on the
      ;; first valid hunk, not on the bogus header above it.
      (hermes-kanban-log-previous-hunk)
      (should (= (point) first-hunk)))))

;;; Group N: live events tail

(ert-deftest hermes-kanban-events-handle-frame-advances-cursor-and-schedules ()
  "A `{events,cursor}' frame advances the cursor and debounces one refresh."
  (let ((tail (hermes-kanban--events-tail-create
               :buffer (current-buffer) :cursor 1))
        scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (setq scheduled t) 'timer)))
      (hermes-kanban--events-handle-frame
       tail "{\"events\":[{\"id\":5}],\"cursor\":5}")
      (should (= 5 (hermes-kanban--events-tail-cursor tail)))
      (should scheduled))))

(ert-deftest hermes-kanban-events-handle-frame-ignores-bad-json ()
  "A malformed frame leaves the cursor untouched and schedules nothing."
  (let ((tail (hermes-kanban--events-tail-create
               :buffer (current-buffer) :cursor 3))
        scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (setq scheduled t) 'timer)))
      (hermes-kanban--events-handle-frame tail "not json")
      (should (= 3 (hermes-kanban--events-tail-cursor tail)))
      (should-not scheduled))))

(ert-deftest hermes-kanban-live-indicator-reflects-tail-state ()
  "The indicator is shadow when off and success when a tail is live."
  (with-temp-buffer
    (should (eq 'shadow (get-text-property 0 'face (hermes-kanban--live-indicator))))
    (setq-local hermes-kanban--events-tail (hermes-kanban--events-tail-create))
    (let ((ind (hermes-kanban--live-indicator)))
      (should (string-match-p "live" ind))
      (should (eq 'success (get-text-property 1 'face ind))))))

(ert-deftest hermes-kanban-events-reconnect-backs-off-and-stops-when-dead ()
  "Reconnect doubles the backoff, never double-schedules, and stops if dead."
  (let (scheduled
        (tail (hermes-kanban--events-tail-create
               :buffer (current-buffer) :backoff 2)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay &rest _) (push delay scheduled) 'timer)))
      (hermes-kanban--events-reconnect tail)
      (should (equal scheduled '(2)))
      (should (= 4 (hermes-kanban--events-tail-backoff tail)))
      (hermes-kanban--events-reconnect tail)
      (should (equal scheduled '(2)))))
  (let ((dead (generate-new-buffer "k")) (count 0))
    (kill-buffer dead)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (cl-incf count) 'timer)))
      (hermes-kanban--events-reconnect
       (hermes-kanban--events-tail-create :buffer dead))
      (should (= 0 count)))))

(ert-deftest hermes-kanban-toggle-live-requires-board-mode ()
  "Toggling live updates outside a board buffer signals a `user-error'."
  (with-temp-buffer
    (should-error (hermes-kanban-toggle-live) :type 'user-error)))

(ert-deftest hermes-kanban-toggle-live-on-seeds-cursor-then-off ()
  "Toggling on seeds the cursor from the last render and installs teardown."
  (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80))
            ((symbol-function 'hermes-kanban--events-connect) #'ignore))
    (with-temp-buffer
      (hermes-kanban-mode)
      (setq hermes-kanban--slug "emacs-lisp"
            hermes-kanban--latest-event-id 7)
      (hermes-kanban-toggle-live)
      (should hermes-kanban--events-tail)
      (should (= 7 (hermes-kanban--events-tail-cursor
                    hermes-kanban--events-tail)))
      (should (memq #'hermes-kanban--events-teardown kill-buffer-hook))
      (hermes-kanban-toggle-live)
      (should-not hermes-kanban--events-tail))))

(ert-deftest hermes-kanban-render-board-seeds-latest-event-id ()
  "Rendering a board records its latest_event_id for live seeding."
  (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80))
            ((symbol-function 'hermes-kanban--api)
             (lambda (_m _p &optional _b _q)
               (hermes--promise-resolved
                '((columns . (((name . "todo") (tasks . []))))
                  (assignees) (latest_event_id . 42))))))
    (unwind-protect
        (progn
          (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
          (with-current-buffer "*Hermes Kanban*"
            (should (= 42 hermes-kanban--latest-event-id))))
      (when (get-buffer "*Hermes Kanban*") (kill-buffer "*Hermes Kanban*")))))

(ert-deftest hermes-kanban-profile-candidates-merge-cache-and-board-assignees ()
  "Candidates merge the warmed profile cache with board-known assignees."
  (let ((hermes-dashboard-transport--profile-cache nil))
    (hermes-dashboard-transport--store-profile-cache
     '((profiles . (((name . "default") (is_default . t))
                    ((name . "elisp-dev"))
                    ((name . "reviewer"))))))
    (with-temp-buffer
      (hermes-kanban-mode)
      (setq hermes-kanban--assignees '("elisp-dev" "spike"))
      (should (equal (hermes-kanban--profile-candidates)
                     '("default" "elisp-dev" "reviewer" "spike"))))))

(ert-deftest hermes-kanban-profile-candidates-empty-when-no-source ()
  "With no cache and no board assignees, candidates is empty and never errors."
  (let ((hermes-dashboard-transport--profile-cache nil))
    (with-temp-buffer
      (hermes-kanban-mode)
      (setq hermes-kanban--assignees nil)
      (should (equal (hermes-kanban--profile-candidates) nil)))))

(ert-deftest hermes-kanban-profile-candidates-use-task-detail-assignees ()
  "Task detail completions include assignees captured from the board."
  (let ((hermes-dashboard-transport--profile-cache nil))
    (with-temp-buffer
      (hermes-kanban-task-mode)
      (setq hermes-kanban-task--assignees '("elisp-dev" "reviewer"))
      (should (equal (hermes-kanban--profile-candidates)
                     '("elisp-dev" "reviewer"))))))

(ert-deftest hermes-kanban-change-assignee-patches-from-task-detail ()
  "Changing assignee from the task detail PATCHes /tasks/:id and reverts."
  (let (calls reverted)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "elisp-dev"))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (with-temp-buffer
        (hermes-kanban-task-mode)
        (setq hermes-kanban-task--task-id "t1"
              hermes-kanban-task--board-slug "emacs-lisp"
              hermes-kanban-task--status "ready")
        (hermes-kanban-change-assignee)
        (should (member '("PATCH" "/tasks/t1" ((assignee . "elisp-dev"))
                          ((board . "emacs-lisp")))
                        calls))
        (should reverted)))))

(ert-deftest hermes-kanban-change-assignee-reassigns-running-task-detail ()
  "Changing assignee for a running task detail uses reclaiming reassign."
  (let (calls reverted)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "elisp-dev"))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (with-temp-buffer
        (hermes-kanban-task-mode)
        (setq hermes-kanban-task--task-id "t1"
              hermes-kanban-task--board-slug "emacs-lisp"
              hermes-kanban-task--status "running")
        (hermes-kanban-change-assignee)
        (should (member '("POST" "/tasks/t1/reassign"
                          ((profile . "elisp-dev") (reclaim_first . t))
                          ((board . "emacs-lisp")))
                        calls))
        (should reverted)))))

(provide 'hermes-kanban-tests)
;;; hermes-kanban-tests.el ends here
