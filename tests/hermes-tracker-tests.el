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
                  "before\n```tracker-ref\n{\"repo_slug\":\"proj\",\"number\":3}\n```\nafter")
                 '(:repo-slug "proj" :number 3)))
  (dolist (body '(nil
                  "```tracker-ref\nnot-json\n```"
                  "```tracker-ref\n{\"repo_slug\":\"proj\",\"number\":0}\n```"
                  "```tracker-ref\n{\"repo_slug\":\"proj\",\"number\":3,\"extra\":true}\n```"
                  "```tracker-ref\n{\"repo_slug\":\"a\",\"number\":1}\n```\n```tracker-ref\n{\"repo_slug\":\"b\",\"number\":2}\n```"))
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
                             (body . "```tracker-ref\n{\"repo_slug\":\"proj\",\"number\":3}\n```")))))))
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
                                    (body . "```tracker-ref\n{\"repo_slug\":\"other\",\"number\":1}\n```")))))
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
                        'hermes-tracker-update-todo))))
      (when (get-buffer "*Hermes Tracker TODO*")
        (kill-buffer "*Hermes Tracker TODO*")))))

(provide 'hermes-tracker-tests)
;;; hermes-tracker-tests.el ends here
