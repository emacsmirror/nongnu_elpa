;;; hermes-tracker-tests.el --- Tracker tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)
(require 'hermes-tracker)

(ert-deftest hermes-tracker-normalize-base-url-requires-secure-remote-origin ()
  "Tracker URLs require HTTPS except for loopback development."
  (should (equal (hermes-tracker--normalize-base-url " https://tracker.test/ ")
                 "https://tracker.test"))
  (should (equal (hermes-tracker--normalize-base-url "http://127.0.0.1:8000/")
                 "http://127.0.0.1:8000"))
  (should-error (hermes-tracker--normalize-base-url "http://tracker.test")
                :type 'user-error)
  (should-error (hermes-tracker--normalize-base-url "https://u:p@tracker.test")
                :type 'user-error)
  (should-error (hermes-tracker--normalize-base-url "file:///tmp/tracker")
                :type 'user-error))

(ert-deftest hermes-tracker-build-request-redacts-auth-and-adds-mutation-headers ()
  "Tracker requests carry bearer, actor, idempotency, and redaction metadata."
  (let ((hermes-tracker-url "https://tracker.test")
        (hermes-tracker-actor "emacs-user"))
    (cl-letf (((symbol-function 'hermes-tracker--token) (lambda (_) "top-secret")))
      (let* ((request (hermes-tracker--request
                       "POST" "/api/v1/repos/proj/todos"
                       '((title . "Do it")) "stable-invocation"))
             (headers (plist-get request :headers)))
        (should (equal (plist-get request :url)
                       "https://tracker.test/api/v1/repos/proj/todos"))
        (should (equal (cdr (assoc "Authorization" headers))
                       "Bearer top-secret"))
        (should (equal (cdr (assoc "X-Actor" headers)) "emacs-user"))
        (should (string-prefix-p "emacs-hermes-"
                                 (cdr (assoc "Idempotency-Key" headers))))
        (should (equal (plist-get request :secrets) '("top-secret")))
        (should-not (string-match-p "top-secret"
                                    (hermes-tracker--safe-request-description request)))))))

(ert-deftest hermes-tracker-unconfigured-signals-without-auth-lookup ()
  "An absent Tracker URL stops before credential lookup."
  (let ((hermes-tracker-url nil)
        looked-up)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) (setq looked-up t))))
      (should-error (hermes-tracker--base-url) :type 'user-error)
      (should-not looked-up))))

(ert-deftest hermes-tracker-repository-and-todo-rows-normalize-api-values ()
  "Repository and TODO payloads become stable tabulated-list entries."
  (let ((repo-row (car (hermes-tracker--repository-rows
                        [((slug . "proj") (name . "Project")
                          (description . "Work"))])))
        (todo-row (car (hermes-tracker--todo-rows
                        '((todos . [((number . 3) (status . "open")
                                    (priority . 7) (assignee . "elisp-dev")
                                    (title . "Build it"))]))))))
    (should (equal (car repo-row) "proj"))
    (should (equal (cadr repo-row) ["Project" "proj" "Work"]))
    (should (equal (car todo-row) 3))
    (should (equal (cadr todo-row) ["3" "open" "7" "elisp-dev" "Build it"]))))

(ert-deftest hermes-tracker-rows-face-every-column ()
  "Tracker repository and TODO rows give every column its own face."
  (let* ((repo-row (car (hermes-tracker--repository-rows
                         '(((slug . "proj") (name . "Project")
                            (description . "Work"))))))
         (row (car (hermes-tracker--todo-rows
                    '((todos . (((number . 3) (status . "open")
                                 (priority . 7) (assignee . "elisp-dev")
                                 (title . "Build it"))))))))
         (entry (cadr row)))
    (should (eq (get-text-property 0 'face (aref (cadr repo-row) 0))
                'hermes-browser-name))
    (should (eq (get-text-property 0 'face (aref (cadr repo-row) 1))
                'hermes-browser-identifier))
    (should (eq (get-text-property 0 'face (aref (cadr repo-row) 2))
                'hermes-browser-description))
    (should (eq (get-text-property 0 'face (aref entry 0))
                'hermes-browser-identifier))
    (should (equal (get-text-property 0 'face (aref entry 1))
                   '(hermes-browser-active hermes-browser-status)))
    (should (eq (get-text-property 0 'face (aref entry 2))
                'hermes-browser-priority))
    (should (eq (get-text-property 0 'face (aref entry 3))
                'hermes-browser-assignee))
    (should (eq (get-text-property 0 'face (aref entry 4))
                'hermes-browser-title))))

(ert-deftest hermes-tracker-format-detail-renders-durable-fields ()
  "TODO detail includes criteria, evidence, links, comments, and closure data."
  (let ((text (hermes-tracker--format-todo-detail
               '((repo_slug . "proj") (number . 3) (title . "Build it")
                 (status . "done") (priority . 7) (assignee . "elisp-dev")
                 (description . "Description")
                 (acceptance_criteria . ["Works"])
                 (verification_commands . ["make test"])
                 (verification_output . "42 tests pass")
                 (closing_commit . "abc123") (closed_by . "reviewer")
                 (closed_at . "2026-07-12T12:00:00Z")
                 (links . [((link_type . "depends-on")
                            (target_repo_slug . "core") (target_number . 2)
                            (target_title . "Foundation"))])
                 (comments . [((author . "elisp-dev") (body . "Ready")
                               (created_at . "2026-07-12T11:00:00Z"))])))))
    (dolist (needle '("# Build it" "Description" "Works" "make test"
                      "42 tests pass" "abc123" "reviewer"
                      "depends-on core#2" "Ready"))
      (should (string-match-p (regexp-quote needle) text)))))

(ert-deftest hermes-tracker-parse-tracker-reference-is-strict ()
  "Only one exact canonical tracker-ref block opts a Kanban card in."
  (should (equal (hermes-tracker-parse-reference
                  "before\n```tracker-ref\n{\"number\":3,\"repo_slug\":\"proj\"}\n```\nafter")
                 '(:repo-slug "proj" :number 3)))
  (dolist (body '(nil
                  "```tracker-ref\nnot-json\n```"
                  "```tracker-ref\n{\"repo_slug\":\"proj\",\"number\":0}\n```"
                  "```tracker-ref\n{\"repo_slug\":\"proj\",\"number\":3,\"extra\":true}\n```"
                  "```tracker-ref   \n{\"number\":3,\"repo_slug\":\"proj\"}\n```"
                  "````tracker-ref\n{\"number\":3,\"repo_slug\":\"proj\"}\n````"
                  "```tracker-ref\n{\"number\":3,\"repo_slug\":\"proj\"}\n```\n```tracker-ref"
                  "```tracker-ref\n{ \"number\": 3, \"repo_slug\": \"proj\" }\n```"
                  "```tracker-ref\n{\"repo_slug\":\"proj\",\"number\":3}\n```"
                  "```tracker-ref\n{\"number\":1,\"repo_slug\":\"a\"}\n```\n```tracker-ref\n{\"number\":2,\"repo_slug\":\"b\"}\n```"))
    (should-not (hermes-tracker-parse-reference body))))

(ert-deftest hermes-tracker-render-and-append-reference-is-canonical ()
  "Canonical tracker-ref rendering is compact and append refuses conflicts."
  (should (equal (hermes-tracker-render-reference "proj" 3)
                 "```tracker-ref\n{\"number\":3,\"repo_slug\":\"proj\"}\n```"))
  (should (equal (hermes-tracker--body-with-reference "Body" "proj" 3)
                 "Body\n\n```tracker-ref\n{\"number\":3,\"repo_slug\":\"proj\"}\n```"))
  (should-error
   (hermes-tracker--body-with-reference
    "```tracker-ref\n{\"number\":2,\"repo_slug\":\"other\"}\n```" "proj" 3)
   :type 'user-error))

(ert-deftest hermes-tracker-kanban-references-and-drift-are-normalized ()
  "Canonical references resolve to linked-card records with drift state."
  (let* ((todo '((repo_slug . "proj") (number . 3)
                 (external_references
                  . [((system . "other") (external_id . "x"))
                     ((system . "hermes-kanban") (external_id . "t_1234abcd")
                      (metadata . ((board . "default") (profile . "elisp-dev"))))])
                 (superseded_kanban_executions
                  . [((task_id . "t_1234abcd")
                      (replacement_kanban_id . "t_99999999"))])))
         (refs (hermes-tracker--kanban-references todo))
         (linked (hermes-tracker--linked-card
                  todo (car refs)
                  '((task . ((id . "t_1234abcd") (title . "Card")
                             (status . "done") (assignee . "elisp-dev")
                             (body . "```tracker-ref\n{\"number\":3,\"repo_slug\":\"proj\"}\n```")))))))
    (should (= (length refs) 1))
    (should (equal (plist-get linked :drift) 'none))
    (should (equal (plist-get linked :execution-state) 'superseded))
    (should (equal (plist-get linked :replacement-id) "t_99999999")))
  (let ((missing (hermes-tracker--linked-card
                  '((repo_slug . "proj") (number . 3))
                  '((external_id . "t_deadbeef") (metadata . ((board . "default"))))
                  nil)))
    (should (equal (plist-get missing :drift) 'missing-card))))

(ert-deftest hermes-tracker-linked-card-detects-one-sided-and-mismatched-references ()
  "Loaded cards report missing or mismatched card-side backlinks."
  (let ((todo '((repo_slug . "proj") (number . 3)))
        (ref '((external_id . "t_1234abcd") (metadata . ((board . "default"))))))
    (should (equal
             (plist-get (hermes-tracker--linked-card
                         todo ref '((task . ((id . "t_1234abcd") (body . "plain")))))
                        :drift)
             'missing-card-backlink))
    (should (equal
             (plist-get (hermes-tracker--linked-card
                         todo ref
                         '((task . ((id . "t_1234abcd")
                                    (body . "```tracker-ref\n{\"number\":1,\"repo_slug\":\"other\"}\n```")))))
                        :drift)
             'mismatched-card-backlink))))

(ert-deftest hermes-tracker-dashboard-entry-is-discoverable-and-lazy ()
  "Dashboard exposes Tracker without requiring configuration or doing I/O."
  (let ((hermes-tracker-url nil))
    (should (eq (keymap-lookup hermes-dashboard-mode-map "T")
                'hermes-list-tracker-repositories))
    (should (featurep 'hermes-tracker))))

(ert-deftest hermes-tracker-todo-detail-uses-markdown-mode ()
  "Tracker TODO details retain actions while rendering as Markdown."
  (cl-letf (((symbol-function 'pop-to-buffer) #'switch-to-buffer))
    (unwind-protect
        (progn
          (hermes-tracker--display-todo
           (cons '((repo_slug . "proj") (number . 3) (title . "Build it")) nil))
          (with-current-buffer "*Hermes Tracker TODO*"
            (should (derived-mode-p 'hermes-tracker-todo-mode))
            (when (require 'markdown-mode nil t)
              (should (derived-mode-p 'markdown-mode)))
            (should buffer-read-only)
            (goto-char (point-min))
            (should (re-search-forward outline-regexp nil t))
            (should (= (funcall outline-level) 1))
            (setq-local revert-buffer-function (lambda (&rest _)))
            (revert-buffer t)
            (should (eq (keymap-lookup (current-local-map) "u")
                        'hermes-tracker-update-todo))
            (should (eq (keymap-lookup (current-local-map) "C")
                        'hermes-tracker-close-todo))))
      (when (get-buffer "*Hermes Tracker TODO*")
        (kill-buffer "*Hermes Tracker TODO*")))))

(ert-deftest hermes-tracker-repositories-discard-late-response ()
  "A late repositories response cannot replace the latest request."
  (let ((old (hermes--promise-make)) (new (hermes--promise-make)) (calls 0))
    (cl-letf (((symbol-function 'hermes-tracker--request-async)
               (lambda (&rest _)
                 (cl-incf calls)
                 (if (= calls 1) old new)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (unwind-protect
          (progn
            (hermes-list-tracker-repositories)
            (hermes-list-tracker-repositories)
            (hermes--promise-resolve
             new '(((slug . "new") (name . "New"))))
            (hermes--promise-resolve
             old '(((slug . "old") (name . "Old"))))
            (with-current-buffer "*Hermes Tracker Repositories*"
              (should (equal (caar tabulated-list-entries) "new"))))
        (when (get-buffer "*Hermes Tracker Repositories*")
          (kill-buffer "*Hermes Tracker Repositories*"))))))

(ert-deftest hermes-tracker-todo-list-discards-late-response ()
  "A late repository A response cannot replace repository B TODOs."
  (let ((a (hermes--promise-make)) (b (hermes--promise-make)))
    (cl-letf (((symbol-function 'hermes-tracker--request-async)
               (lambda (_method path &rest _)
                 (if (string-match-p "/repos/a/" path) a b)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (unwind-protect
          (progn
            (hermes-tracker--render-todos "a" "Repo A")
            (hermes-tracker--render-todos "b" "Repo B")
            (hermes--promise-resolve
             b '((todos . (((number . 2) (status . "open")
                            (title . "New"))))))
            (hermes--promise-resolve
             a '((todos . (((number . 1) (status . "open")
                            (title . "Old"))))))
            (with-current-buffer "*Hermes Tracker TODOs*"
              (should (equal hermes-tracker--repo-slug "b"))
              (should (= (caar tabulated-list-entries) 2))))
        (when (get-buffer "*Hermes Tracker TODOs*")
          (kill-buffer "*Hermes Tracker TODOs*"))))))

(ert-deftest hermes-tracker-todo-detail-discards-late-response ()
  "A late TODO A response cannot replace the newer TODO B detail."
  (let ((a (hermes--promise-make)) (b (hermes--promise-make)))
    (cl-letf (((symbol-function 'hermes-tracker--fetch-todo-with-cards)
               (lambda (repo _number) (if (equal repo "a") a b)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (unwind-protect
          (progn
            (hermes-tracker-open-todo "a" 1)
            (hermes-tracker-open-todo "b" 2)
            (hermes--promise-resolve
             b '(((repo_slug . "b") (number . 2) (title . "New"))))
            (hermes--promise-resolve
             a '(((repo_slug . "a") (number . 1) (title . "Old"))))
            (with-current-buffer "*Hermes Tracker TODO*"
              (should (equal hermes-tracker--repo-slug "b"))
              (should (= hermes-tracker--todo-number 2))
              (should (string-match-p "# New" (buffer-string)))
              (should-not (string-match-p "# Old" (buffer-string)))))
        (when (get-buffer "*Hermes Tracker TODO*")
          (kill-buffer "*Hermes Tracker TODO*"))))))

(ert-deftest hermes-tracker-linked-card-finds-stale-board-metadata ()
  "A linked task moved to another board is reported as stale-board, not missing."
  (let* ((todo '((repo_slug . "proj") (number . 3)))
         (reference '((external_id . "t_1234abcd")
                      (metadata . ((board . "old")))))
         card calls)
    (cl-letf (((symbol-function
                'hermes-dashboard-transport-api-request-async)
               (lambda (_method path &rest args)
                 (let ((board (cdr (assq 'board (plist-get args :query)))))
                   (push (list path board) calls)
                   (cond
                    ((equal path "/api/plugins/kanban/boards")
                     (hermes--promise-resolved
                      '((boards . (((slug . "new")))))))
                    ((equal board "new")
                     (hermes--promise-resolved
                      '((task . ((id . "t_1234abcd") (title . "Moved")
                                  (status . "todo")
                                  (body . "```tracker-ref\n{\"number\":3,\"repo_slug\":\"proj\"}\n```"))))))
                    (t
                     (hermes--promise-rejected
                      "Hermes dashboard request failed (HTTP 404)")))))))
      (hermes--promise-then
       (hermes-tracker--fetch-linked-card todo reference)
       (lambda (value) (setq card value)))
      (should (equal (plist-get card :board) "new"))
      (should (eq (plist-get card :drift) 'stale-board))
      (should (member '("/api/plugins/kanban/boards" nil) calls)))))

(ert-deftest hermes-tracker-linked-card-propagates-primary-fetch-failure ()
  "A non-404 primary task fetch failure is not reported as a missing card."
  (let (rejected)
    (cl-letf (((symbol-function
                'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (hermes--promise-rejected
                  "Hermes dashboard request failed (HTTP 500)"))))
      (hermes--promise-catch
       (hermes-tracker--fetch-linked-card
        '((repo_slug . "proj") (number . 3))
        '((external_id . "t_1234abcd")
          (metadata . ((board . "default")))))
       (lambda (reason) (setq rejected reason)))
      (should (string-match-p "HTTP 500" rejected)))))

(ert-deftest hermes-tracker-linked-card-propagates-partial-board-failure ()
  "An incomplete fallback scan cannot turn an unknown card into a missing card."
  (let (rejected)
    (cl-letf (((symbol-function
                'hermes-dashboard-transport-api-request-async)
               (lambda (_method path &rest args)
                 (let ((board (cdr (assq 'board (plist-get args :query)))))
                   (cond
                    ((equal path "/api/plugins/kanban/boards")
                     (hermes--promise-resolved
                      '((boards . (((slug . "a")) ((slug . "b")))))))
                    ((member board '("default" "a"))
                     (hermes--promise-rejected
                      "Hermes dashboard request failed (HTTP 404)"))
                    (t
                     (hermes--promise-rejected
                      "Hermes dashboard request failed (HTTP 500)")))))))
      (hermes--promise-catch
       (hermes-tracker--fetch-linked-card
        '((repo_slug . "proj") (number . 3))
        '((external_id . "t_1234abcd")
          (metadata . ((board . "default")))))
       (lambda (reason) (setq rejected reason)))
      (should (string-match-p "HTTP 500" rejected)))))

(ert-deftest hermes-tracker-linked-card-prefers-match-over-partial-failure ()
  "A fallback match remains authoritative when a different board fetch fails."
  (let (card)
    (cl-letf (((symbol-function
                'hermes-dashboard-transport-api-request-async)
               (lambda (_method path &rest args)
                 (let ((board (cdr (assq 'board (plist-get args :query)))))
                   (cond
                    ((equal path "/api/plugins/kanban/boards")
                     (hermes--promise-resolved
                      '((boards . (((slug . "found")) ((slug . "broken")))))))
                    ((equal board "default")
                     (hermes--promise-rejected
                      "Hermes dashboard request failed (HTTP 404)"))
                    ((equal board "found")
                     (hermes--promise-resolved
                      '((task . ((id . "t_1234abcd") (title . "Moved")
                                  (body . "```tracker-ref\n{\"number\":3,\"repo_slug\":\"proj\"}\n```"))))))
                    (t
                     (hermes--promise-rejected
                      "Hermes dashboard request failed (HTTP 500)")))))))
      (hermes--promise-then
       (hermes-tracker--fetch-linked-card
        '((repo_slug . "proj") (number . 3))
        '((external_id . "t_1234abcd")
          (metadata . ((board . "default")))))
       (lambda (value) (setq card value)))
      (should (equal (plist-get card :board) "found"))
      (should (eq (plist-get card :drift) 'stale-board)))))

(ert-deftest hermes-tracker-create-todo-reads-multiline-description ()
  "TODO creation reads its description through the multiline editor."
  (let (request refreshed)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Title"))
              ((symbol-function 'read-string-from-buffer)
               (lambda (&rest _) "line one\nline two"))
              ((symbol-function 'read-number) (lambda (&rest _) 4))
              ((symbol-function 'hermes-tracker--request-async)
               (lambda (method path &optional body invocation)
                 (setq request (list method path body invocation))
                 (hermes--promise-resolved '((number . 1)))))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq refreshed t))))
      (with-temp-buffer
        (setq-local hermes-tracker--repo-slug "proj")
        (hermes-tracker-create-todo))
      (should refreshed)
      (should (equal (cdr (assq 'description (nth 2 request)))
                     "line one\nline two")))))

(ert-deftest hermes-tracker-comment-reads-multiline-body ()
  "Tracker comments preserve multiline text from the editor."
  (let (request refreshed)
    (cl-letf (((symbol-function 'read-string-from-buffer)
               (lambda (&rest _) "first\nsecond"))
              ((symbol-function 'hermes-tracker--request-async)
               (lambda (method path &optional body invocation)
                 (setq request (list method path body invocation))
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq refreshed t))))
      (with-temp-buffer
        (setq-local major-mode 'hermes-tracker-todo-mode
                    hermes-tracker--repo-slug "proj"
                    hermes-tracker--todo-number 3)
        (hermes-tracker-comment))
      (should refreshed)
      (should (equal (cdr (assq 'body (nth 2 request))) "first\nsecond")))))

(ert-deftest hermes-tracker-close-requires-verification-evidence ()
  "Closing stops before the API when multiline evidence is blank."
  (let (called)
    (cl-letf (((symbol-function 'read-string-from-buffer)
               (lambda (&rest _) " \n "))
              ((symbol-function 'hermes-tracker--request-async)
               (lambda (&rest _) (setq called t))))
      (with-temp-buffer
        (setq-local major-mode 'hermes-tracker-todo-mode
                    hermes-tracker--repo-slug "proj"
                    hermes-tracker--todo-number 3)
        (should-error (hermes-tracker-close-todo) :type 'user-error))
      (should-not called))))

(ert-deftest hermes-tracker-close-posts-evidence-and-optional-commit ()
  "Closing posts durable evidence and refreshes only after success."
  (let (request refreshed)
    (cl-letf (((symbol-function 'read-string-from-buffer)
               (lambda (&rest _) "make test\n42 passed"))
              ((symbol-function 'read-string) (lambda (&rest _) "abc123"))
              ((symbol-function 'hermes-tracker--request-async)
               (lambda (method path &optional body invocation)
                 (setq request (list method path body invocation))
                 (hermes--promise-resolved '((status . "done")))))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq refreshed t))))
      (with-temp-buffer
        (setq-local major-mode 'hermes-tracker-todo-mode
                    hermes-tracker--repo-slug "proj"
                    hermes-tracker--todo-number 3)
        (hermes-tracker-close-todo))
      (should refreshed)
      (should (equal (seq-take request 3)
                     '("POST" "/api/v1/repos/proj/todos/3/close"
                       ((verification_output . "make test\n42 passed")
                        (closing_commit . "abc123"))))))))

(ert-deftest hermes-tracker-close-rejection-does-not-refresh ()
  "A dependency conflict is reported and leaves the detail unchanged."
  (let (refreshed messages)
    (cl-letf (((symbol-function 'read-string-from-buffer)
               (lambda (&rest _) "verified"))
              ((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'hermes-tracker--request-async)
               (lambda (&rest _)
                 (hermes--promise-rejected "blocked by dependency")))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (setq-local major-mode 'hermes-tracker-todo-mode
                    hermes-tracker--repo-slug "proj"
                    hermes-tracker--todo-number 3)
        (hermes-tracker-close-todo))
      (should-not refreshed)
      (should (seq-some (lambda (text)
                          (string-match-p "blocked by dependency" text))
                        messages)))))

(ert-deftest hermes-tracker-link-card-uses-atomic-reference-route ()
  "Card linking appends the canonical reference through the dedicated route."
  (let (request)
    (cl-letf (((symbol-function 'hermes-tracker--ensure-external-reference)
               (lambda (&rest _) (hermes--promise-resolved 'ok)))
              ((symbol-function
                'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (setq request (list method path args))
                 (hermes--promise-resolved '((task . ((id . "t_1234abcd"))))))))
      (hermes-tracker--link-card
       "proj" 3 "default" "t_1234abcd"
       '((task . ((id . "t_1234abcd") (assignee . "dev")
                   (body . "newer body")))))
      (should (equal (car request) "POST"))
      (should (equal (cadr request)
                     "/api/plugins/kanban/tasks/t_1234abcd/tracker-reference"))
      (should (equal (plist-get (caddr request) :body)
                     '((repo_slug . "proj") (number . 3)))))))

(ert-deftest hermes-tracker-link-card-stops-when-backlink-fails ()
  "A Tracker backlink failure prevents the card-side append."
  (let (called rejected)
    (cl-letf (((symbol-function 'hermes-tracker--ensure-external-reference)
               (lambda (&rest _) (hermes--promise-rejected "tracker failed")))
              ((symbol-function
                'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) (setq called t))))
      (hermes--promise-catch
       (hermes-tracker--link-card
        "proj" 3 "default" "t_1234abcd"
        '((task . ((id . "t_1234abcd") (body . "body")))))
       (lambda (reason) (setq rejected reason)))
      (should (equal rejected "tracker failed"))
      (should-not called))))

(ert-deftest hermes-tracker-link-card-surfaces-card-append-failure ()
  "A card-side append failure reports the repairable one-sided link."
  (let (rejected)
    (cl-letf (((symbol-function 'hermes-tracker--ensure-external-reference)
               (lambda (&rest _) (hermes--promise-resolved 'ok)))
              ((symbol-function
                'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) (hermes--promise-rejected "append failed"))))
      (hermes--promise-catch
       (hermes-tracker--link-card
        "proj" 3 "default" "t_1234abcd"
        '((task . ((id . "t_1234abcd") (body . "body")))))
       (lambda (reason) (setq rejected reason)))
      (should (string-match-p "Tracker backlink was written" rejected))
      (should (string-match-p "retry" rejected))
      (should (string-match-p "append failed" rejected)))))

(ert-deftest hermes-tracker-link-command-refreshes-after-complete-success ()
  "The link command reports and refreshes only after both sides succeed."
  (let ((link (hermes--promise-make)) messages refreshed)
    (cl-letf (((symbol-function 'read-string)
               (let ((answers '("default" "t_1234abcd")))
                 (lambda (&rest _) (pop answers))))
              ((symbol-function
                'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '((task . ((id . "t_1234abcd") (body . "body")))))))
              ((symbol-function 'hermes-tracker--link-card)
               (lambda (&rest _) link))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (setq-local hermes-tracker--repo-slug "proj"
                    hermes-tracker--todo-number 3)
        (hermes-tracker-link-kanban)
        (should-not refreshed)
        (should-not (seq-some (lambda (text) (string-match-p "linked" text))
                              messages))
        (hermes--promise-resolve link 'ok)
        (should refreshed)
        (should (seq-some (lambda (text) (string-match-p "linked" text))
                          messages))))))

(ert-deftest hermes-tracker-link-command-reports-final-chain-failure ()
  "A final link failure reports once and never refreshes the TODO."
  (let (messages refreshed)
    (cl-letf (((symbol-function 'read-string)
               (let ((answers '("default" "t_1234abcd")))
                 (lambda (&rest _) (pop answers))))
              ((symbol-function
                'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '((task . ((id . "t_1234abcd") (body . "body")))))))
              ((symbol-function 'hermes-tracker--link-card)
               (lambda (&rest _) (hermes--promise-rejected "append failed")))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (with-temp-buffer
        (setq-local hermes-tracker--repo-slug "proj"
                    hermes-tracker--todo-number 3)
        (hermes-tracker-link-kanban))
      (should-not refreshed)
      (should (= (length messages) 1))
      (should (string-match-p "append failed" (car messages))))))

(provide 'hermes-tracker-tests)
;;; hermes-tracker-tests.el ends here
