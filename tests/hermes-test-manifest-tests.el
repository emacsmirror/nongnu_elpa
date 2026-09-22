;;; hermes-test-manifest-tests.el --- Manifest validator tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise real file membership and both mapping directions in disposable
;; trees.  No project libraries, external packages or live services are loaded.

;;; Code:

(require 'ert)
(require 'hermes-test-manifest)

(defmacro hermes-test-manifest-tests--tree (paths &rest body)
  "Create a disposable manifest tree containing PATHS, then run BODY.
Bind `root' to its directory and remove it even when an assertion fails."
  (declare (indent 1) (debug t))
  `(let ((root (make-temp-file "hermes-manifest-" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "lisp" root))
           (make-directory (expand-file-name "tests" root))
           (dolist (path ,paths)
             (with-temp-file (expand-file-name path root)
               (insert ";;; Disposable manifest fixture\n")))
           ,@body)
       (delete-directory root t))))

(defun hermes-test-manifest-tests--assert-invalid (message root sources tests support edges)
  "Assert MESSAGE occurs in a validation error for ROOT manifests.
Pass SOURCES, TESTS, SUPPORT and EDGES to the real validator."
  (let ((condition
         (should-error
          (hermes-test-manifest-validate root sources tests support edges)
          :type 'hermes-test-manifest-error)))
    (should (string-match-p (regexp-quote message)
                            (error-message-string condition)))))

(ert-deftest hermes-test-manifest-exact-pairs-and-normalized-paths ()
  (hermes-test-manifest-tests--tree
      '("lisp/hermes-a.el" "tests/hermes-a-tests.el" "tests/helper.el")
    (should (hermes-test-manifest-validate
             root '("./lisp/hermes-a.el") '("./tests/hermes-a-tests.el")
             '("./tests/helper.el") nil))))

(ert-deftest hermes-test-manifest-unmapped-listed-source ()
  (hermes-test-manifest-tests--tree '("lisp/hermes-orphan.el")
    (hermes-test-manifest-tests--assert-invalid
     "Unmapped source: lisp/hermes-orphan.el"
     root '("lisp/hermes-orphan.el") nil nil nil)))

(ert-deftest hermes-test-manifest-unmapped-listed-suite ()
  (hermes-test-manifest-tests--tree '("tests/hermes-orphan-tests.el")
    (hermes-test-manifest-tests--assert-invalid
     "Unmapped suite: tests/hermes-orphan-tests.el"
     root nil '("tests/hermes-orphan-tests.el") nil nil)))

(ert-deftest hermes-test-manifest-chat-prefix-is-not-an-exemption ()
  (hermes-test-manifest-tests--tree '("tests/hermes-chat-new-tests.el")
    (hermes-test-manifest-tests--assert-invalid
     "Unmapped suite" root nil '("tests/hermes-chat-new-tests.el") nil nil)))

(ert-deftest hermes-test-manifest-missing-exception-endpoints ()
  (hermes-test-manifest-tests--tree
      '("lisp/hermes-a.el" "tests/hermes-a-tests.el")
    (dolist (case '(("Missing exception owner endpoint"
                     ("lisp/hermes-absent.el" "tests/hermes-a-tests.el" forward))
                    ("Missing exception suite endpoint"
                     ("lisp/hermes-a.el" "tests/hermes-absent-tests.el" reverse))
                    ("Missing exception owner endpoint"
                     ("tests/absent.el" "tests/hermes-a-tests.el" infrastructure))))
      (hermes-test-manifest-tests--assert-invalid
       (car case) root '("lisp/hermes-a.el") '("tests/hermes-a-tests.el")
       nil (list (cadr case))))))

(ert-deftest hermes-test-manifest-duplicates-after-normalization ()
  (hermes-test-manifest-tests--tree
      '("lisp/hermes-a.el" "tests/hermes-a-tests.el" "tests/helper.el")
    (dolist (case
             '(("Duplicate SRCS"
                ("lisp/hermes-a.el" "./lisp/hermes-a.el")
                ("tests/hermes-a-tests.el") ("tests/helper.el"))
               ("Duplicate TESTS"
                ("lisp/hermes-a.el")
                ("tests/hermes-a-tests.el" "./tests/hermes-a-tests.el")
                ("tests/helper.el"))
               ("Duplicate TEST_SUPPORT"
                ("lisp/hermes-a.el") ("tests/hermes-a-tests.el")
                ("tests/helper.el" "./tests/helper.el"))
               ("Duplicate TESTS + TEST_SUPPORT"
                ("lisp/hermes-a.el") ("tests/hermes-a-tests.el")
                ("tests/helper.el" "tests/hermes-a-tests.el"))))
      (hermes-test-manifest-tests--assert-invalid
       (nth 0 case) root (nth 1 case) (nth 2 case) (nth 3 case) nil))))

(ert-deftest hermes-test-manifest-listed-missing-files ()
  (dolist (missing '("lisp/hermes-a.el" "tests/hermes-a-tests.el" "tests/helper.el"))
    (hermes-test-manifest-tests--tree
        '("lisp/hermes-a.el" "tests/hermes-a-tests.el" "tests/helper.el")
      (delete-file (expand-file-name missing root))
      (hermes-test-manifest-tests--assert-invalid
       (concat "lists missing file: " missing)
       root '("lisp/hermes-a.el") '("tests/hermes-a-tests.el")
       '("tests/helper.el") nil))))

(ert-deftest hermes-test-manifest-unlisted-files-fail-set-equality ()
  (dolist (unlisted '("lisp/hermes-hidden.el" "tests/hermes-hidden-tests.el"
                      "tests/unlisted-helper.el"))
    (hermes-test-manifest-tests--tree
        (list "lisp/hermes-a.el" "tests/hermes-a-tests.el" unlisted)
      (hermes-test-manifest-tests--assert-invalid
       "must match" root '("lisp/hermes-a.el") '("tests/hermes-a-tests.el") nil nil))))

(ert-deftest hermes-test-manifest-ignores-hidden-and-nested-files ()
  (hermes-test-manifest-tests--tree
      '("lisp/hermes-a.el" "tests/hermes-a-tests.el" "lisp/.hidden.el" "tests/.hidden.el")
    (make-directory (expand-file-name "tests/nested" root))
    (with-temp-file (expand-file-name "tests/nested/not-a-suite.el" root))
    (should (hermes-test-manifest-validate
             root '("lisp/hermes-a.el") '("tests/hermes-a-tests.el") nil nil))))

(ert-deftest hermes-test-manifest-rejects-invalid-manifest-paths ()
  (hermes-test-manifest-tests--tree nil
    (dolist (path '("../lisp/hermes-a.el" "lisp/nested/hermes-a.el"
                    "lisp/.hidden.el" "/lisp/hermes-a.el" 17))
      (should-error (hermes-test-manifest-validate root (list path) nil nil nil)
                    :type 'hermes-test-manifest-error))))

(ert-deftest hermes-test-manifest-support-cannot-satisfy-source-coverage ()
  (hermes-test-manifest-tests--tree '("lisp/hermes-a.el" "tests/hermes-a-tests.el")
    (hermes-test-manifest-tests--assert-invalid
     "Unmapped source" root '("lisp/hermes-a.el") nil '("tests/hermes-a-tests.el") nil)
    (hermes-test-manifest-tests--assert-invalid
     "Missing exception suite endpoint"
     root '("lisp/hermes-a.el") nil '("tests/hermes-a-tests.el")
     '(("lisp/hermes-a.el" "tests/hermes-a-tests.el" forward)))))

(ert-deftest hermes-test-manifest-browsers-spelling-is-exact ()
  (let ((edge '(("lisp/hermes-browser.el" "tests/hermes-browsers-tests.el" both))))
    (hermes-test-manifest-tests--tree '("lisp/hermes-browser.el" "tests/hermes-browsers-tests.el")
      (should (hermes-test-manifest-validate
               root '("lisp/hermes-browser.el") '("tests/hermes-browsers-tests.el") nil edge))
      ;; A synchronized manifest/file rename still leaves the exception stale.
      (rename-file (expand-file-name "tests/hermes-browsers-tests.el" root)
                   (expand-file-name "tests/hermes-browser-tests.el" root))
      (hermes-test-manifest-tests--assert-invalid
       "Missing exception suite endpoint" root '("lisp/hermes-browser.el")
       '("tests/hermes-browser-tests.el") nil edge))))

(ert-deftest hermes-test-manifest-stale-forward-role-after-exact-suite ()
  (hermes-test-manifest-tests--tree
      '("lisp/hermes-a.el" "tests/hermes-a-tests.el" "tests/hermes-topic-tests.el")
    (dolist (role '(forward both))
      (hermes-test-manifest-tests--assert-invalid
       "Stale forward exception" root '("lisp/hermes-a.el")
       '("tests/hermes-a-tests.el" "tests/hermes-topic-tests.el") nil
       `(("lisp/hermes-a.el" "tests/hermes-topic-tests.el" ,role))))
    ;; Retain the necessary topic role while retiring its forward exception.
    (should (hermes-test-manifest-validate
             root '("lisp/hermes-a.el")
             '("tests/hermes-a-tests.el" "tests/hermes-topic-tests.el") nil
             '(("lisp/hermes-a.el" "tests/hermes-topic-tests.el" reverse))))))

(ert-deftest hermes-test-manifest-stale-reverse-role-after-exact-source ()
  (hermes-test-manifest-tests--tree
      '("lisp/hermes-a.el" "lisp/hermes-b.el" "tests/hermes-b-tests.el")
    (hermes-test-manifest-tests--assert-invalid
     "Stale reverse exception" root '("lisp/hermes-a.el" "lisp/hermes-b.el")
     '("tests/hermes-b-tests.el") nil
     '(("lisp/hermes-a.el" "tests/hermes-b-tests.el" both)))
    (should (hermes-test-manifest-validate
             root '("lisp/hermes-a.el" "lisp/hermes-b.el")
             '("tests/hermes-b-tests.el") nil
             '(("lisp/hermes-a.el" "tests/hermes-b-tests.el" forward))))))

(ert-deftest hermes-test-manifest-roles-are-directional ()
  (hermes-test-manifest-tests--tree '("lisp/hermes-a.el" "tests/hermes-topic-tests.el")
    (hermes-test-manifest-tests--assert-invalid
     "Unmapped source" root '("lisp/hermes-a.el") '("tests/hermes-topic-tests.el") nil
     '(("lisp/hermes-a.el" "tests/hermes-topic-tests.el" reverse)))
    (hermes-test-manifest-tests--assert-invalid
     "Unmapped suite" root '("lisp/hermes-a.el") '("tests/hermes-topic-tests.el") nil
     '(("lisp/hermes-a.el" "tests/hermes-topic-tests.el" forward)))
    (should (hermes-test-manifest-validate
             root '("lisp/hermes-a.el") '("tests/hermes-topic-tests.el") nil
             '(("lisp/hermes-a.el" "tests/hermes-topic-tests.el" both))))))

(ert-deftest hermes-test-manifest-infrastructure-is-not-production-coverage ()
  (let ((edge '(("tests/helper.el" "tests/helper-tests.el" infrastructure))))
    (hermes-test-manifest-tests--tree '("tests/helper.el" "tests/helper-tests.el")
      (should (hermes-test-manifest-validate
               root nil '("tests/helper-tests.el") '("tests/helper.el") edge)))
    (hermes-test-manifest-tests--tree
        '("lisp/hermes-orphan.el" "tests/helper.el" "tests/helper-tests.el")
      (hermes-test-manifest-tests--assert-invalid
       "Unmapped source" root '("lisp/hermes-orphan.el")
       '("tests/helper-tests.el") '("tests/helper.el") edge))))

(ert-deftest hermes-test-manifest-malformed-and-duplicate-edges ()
  (hermes-test-manifest-tests--tree '("lisp/hermes-a.el" "tests/hermes-topic-tests.el")
    (dolist (edge '(("lisp/hermes-a.el" "tests/hermes-topic-tests.el" typo)
                    ("lisp/hermes-a.el" "tests/hermes-topic-tests.el")
                    ("lisp/hermes-a.el" "tests/hermes-topic-tests.el" both extra)
                    ("lisp/hermes-a.el" . invalid)))
      (hermes-test-manifest-tests--assert-invalid
       "Malformed exception edge" root '("lisp/hermes-a.el")
       '("tests/hermes-topic-tests.el") nil (list edge)))
    (hermes-test-manifest-tests--assert-invalid
     "Duplicate exception edge" root '("lisp/hermes-a.el")
     '("tests/hermes-topic-tests.el") nil
     '(("lisp/hermes-a.el" "tests/hermes-topic-tests.el" both)
       ("lisp/hermes-a.el" "tests/hermes-topic-tests.el" reverse)))))

(ert-deftest hermes-test-manifest-default-graph-all-roles ()
  ;; Keep the fixture's source inventory independent from the exception graph.
  ;; Ordinary exact-name modules outside this graph use the same pairing rule.
  (let* ((stems '("hermes-browser" "hermes-dashboard-api" "hermes-dashboard-rpc"
                  "hermes-dashboard-transport" "hermes-kanban-events" "hermes-kanban-log"
                  "hermes-preview-format" "hermes-profiles" "hermes-rollback"
                  "hermes-session-title" "hermes-subagents" "hermes-transport-cli" "hermes"
                  "hermes-transport" "hermes-kanban" "hermes-preview" "hermes-sessions"
                  "hermes-chat-dashboard" "hermes-chat" "hermes-chat-format"
                  "hermes-chat-render" "hermes-chat-buffer" "hermes-chat-slash"))
         (exact '("hermes-transport" "hermes-kanban" "hermes-preview" "hermes-sessions"
                  "hermes-chat-dashboard" "hermes-chat" "hermes-chat-format"
                  "hermes-chat-render" "hermes-chat-buffer" "hermes-chat-slash"))
         (topics '("hermes-browsers" "hermes-dashboard" "hermes-ui" "hermes-dependency"
                   "hermes-chat-reducer" "hermes-fence-guard" "hermes-chat-fences"
                   "hermes-chat-terminal" "hermes-chat-wire" "hermes-chat-history"
                   "hermes-chat-lifecycle" "hermes-chat-queue"
                   "hermes-test-manifest" "hermes-test-helpers"))
         (sources (mapcar (lambda (stem) (concat "lisp/" stem ".el")) stems))
         (tests (mapcar (lambda (stem) (concat "tests/" stem "-tests.el"))
                        (append exact topics)))
         (support '("tests/hermes-test-manifest.el" "tests/hermes-test-helpers.el")))
    (hermes-test-manifest-tests--tree (append sources tests support)
      (should (hermes-test-manifest-validate root sources tests support))
      ;; Every table endpoint must remain mandatory, not a best-effort edge.
      (dolist (endpoint (delete-dups (apply #'append
                                          (mapcar (lambda (edge) (list (car edge) (cadr edge)))
                                                  hermes-test-manifest-edges))))
        (let ((file (expand-file-name endpoint root)))
          (delete-file file)
          (unwind-protect
              (hermes-test-manifest-tests--assert-invalid
               "Missing exception" root (remove endpoint sources)
               (remove endpoint tests) (remove endpoint support) hermes-test-manifest-edges)
            (with-temp-file file)))))))

(ert-deftest hermes-test-manifest-batch-environment-contract ()
  (hermes-test-manifest-tests--tree '("lisp/hermes-a.el" "tests/hermes-a-tests.el")
    (let ((process-environment (copy-sequence process-environment))
          (hermes-test-manifest-edges nil))
      (setenv "HERMES_MANIFEST_ROOT" root)
      (setenv "HERMES_MANIFEST_SRCS" "lisp/hermes-a.el")
      (setenv "HERMES_MANIFEST_TESTS" "tests/hermes-a-tests.el")
      (setenv "HERMES_MANIFEST_SUPPORT" "")
      (hermes-test-manifest-batch)
      (setenv "HERMES_MANIFEST_TESTS" "tests/hermes-a-tests.el tests/hermes-a-tests.el")
      (should-error (hermes-test-manifest-batch) :type 'hermes-test-manifest-error)
      (setenv "HERMES_MANIFEST_TESTS" nil)
      (should-error (hermes-test-manifest-batch) :type 'hermes-test-manifest-error))))

(provide 'hermes-test-manifest-tests)
;;; hermes-test-manifest-tests.el ends here
