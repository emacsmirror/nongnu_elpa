;;; hermes-test-manifest.el --- Validate source and test manifests -*- lexical-binding: t; -*-

;;; Commentary:

;; This build helper needs only built-in libraries.  Pass the full, explicit
;; source, suite and support manifests to `hermes-test-manifest-validate'.
;; File membership is closed-world; coverage is an exact-name pair or an edge
;; in the single exception graph below.  This checks coverage locations, not
;; the completeness of the assertions in a suite.  Do not run this full gate
;; against an intentionally partial TESTS selection.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(define-error 'hermes-test-manifest-error "Invalid Hermes test manifest")

(defconst hermes-test-manifest-edges
  '(("lisp/hermes-browser.el" "tests/hermes-browsers-tests.el" both)
    ("lisp/hermes-dashboard-api.el" "tests/hermes-transport-tests.el" forward)
    ("lisp/hermes-dashboard-api.el" "tests/hermes-dashboard-tests.el" both)
    ("lisp/hermes-dashboard-rpc.el" "tests/hermes-transport-tests.el" forward)
    ("lisp/hermes-dashboard-rpc.el" "tests/hermes-dashboard-tests.el" both)
    ("lisp/hermes-dashboard-transport.el" "tests/hermes-dashboard-tests.el" both)
    ("lisp/hermes-dashboard-transport.el" "tests/hermes-transport-tests.el" forward)
    ("lisp/hermes-kanban-events.el" "tests/hermes-kanban-tests.el" forward)
    ("lisp/hermes-kanban-log.el" "tests/hermes-kanban-tests.el" forward)
    ("lisp/hermes-kanban-log.el" "tests/hermes-browsers-tests.el" both)
    ("lisp/hermes-preview-format.el" "tests/hermes-preview-tests.el" forward)
    ("lisp/hermes-profiles.el" "tests/hermes-browsers-tests.el" both)
    ("lisp/hermes-rollback.el" "tests/hermes-browsers-tests.el" both)
    ("lisp/hermes-session-title.el" "tests/hermes-sessions-tests.el" forward)
    ("lisp/hermes-subagents.el" "tests/hermes-browsers-tests.el" both)
    ("lisp/hermes-subagents.el" "tests/hermes-chat-dashboard-tests.el" forward)
    ("lisp/hermes-transport-cli.el" "tests/hermes-transport-tests.el" forward)
    ("lisp/hermes.el" "tests/hermes-ui-tests.el" both)
    ("lisp/hermes.el" "tests/hermes-dependency-tests.el" reverse)
    ("lisp/hermes-chat.el" "tests/hermes-chat-reducer-tests.el" reverse)
    ("lisp/hermes-chat-format.el" "tests/hermes-fence-guard-tests.el" reverse)
    ("lisp/hermes-chat.el" "tests/hermes-fence-guard-tests.el" reverse)
    ("lisp/hermes-chat-format.el" "tests/hermes-chat-fences-tests.el" reverse)
    ("lisp/hermes-chat-render.el" "tests/hermes-chat-fences-tests.el" reverse)
    ("lisp/hermes-chat.el" "tests/hermes-chat-fences-tests.el" reverse)
    ("lisp/hermes-chat-buffer.el" "tests/hermes-chat-terminal-tests.el" reverse)
    ("lisp/hermes-chat.el" "tests/hermes-chat-terminal-tests.el" reverse)
    ("lisp/hermes-chat-buffer.el" "tests/hermes-chat-wire-tests.el" reverse)
    ("lisp/hermes-chat-render.el" "tests/hermes-chat-wire-tests.el" reverse)
    ("lisp/hermes-chat-dashboard.el" "tests/hermes-chat-wire-tests.el" reverse)
    ("lisp/hermes-chat.el" "tests/hermes-chat-history-tests.el" reverse)
    ("lisp/hermes-chat-dashboard.el" "tests/hermes-chat-history-tests.el" reverse)
    ("lisp/hermes-chat.el" "tests/hermes-chat-lifecycle-tests.el" reverse)
    ("lisp/hermes-chat-buffer.el" "tests/hermes-chat-lifecycle-tests.el" reverse)
    ("lisp/hermes-chat-dashboard.el" "tests/hermes-chat-lifecycle-tests.el" reverse)
    ("lisp/hermes-chat.el" "tests/hermes-chat-queue-tests.el" reverse)
    ("lisp/hermes-chat-buffer.el" "tests/hermes-chat-queue-tests.el" reverse)
    ("lisp/hermes-chat-dashboard.el" "tests/hermes-chat-queue-tests.el" reverse)
    ("tests/hermes-test-manifest.el" "tests/hermes-test-manifest-tests.el" infrastructure)
    ("tests/hermes-test-helpers.el" "tests/hermes-test-helpers-tests.el" infrastructure))
  "Explicit (OWNER SUITE ROLE) edges, using root-relative filenames.
ROLE is `forward' for a source missing its exact-name suite, `reverse'
for a topic suite missing its exact-name source, or `both' when both
exceptions are needed.  Multiple owners are explicit individual edges.
An `infrastructure' edge connects an enumerated support file to a suite;
it never satisfies production-source coverage.  Adding an exact pair
requires removing the now-redundant role, not preserving stale exceptions.")

(defun hermes-test-manifest--fail (format-string &rest args)
  "Signal a manifest error using FORMAT-STRING and ARGS."
  (signal 'hermes-test-manifest-error
          (list (apply #'format format-string args))))

(defun hermes-test-manifest--paths (paths directory label)
  "Normalize PATHS in DIRECTORY, rejecting invalid or duplicate LABEL entries."
  (let ((normalized
         (mapcar
          (lambda (path)
            (unless (stringp path)
              (hermes-test-manifest--fail "%s entry is not a filename: %S" label path))
            (let ((name (string-remove-prefix "./" path)))
              (unless (string-match-p
                       (concat "\\`" directory "/[^./][^/]*\\.el\\'") name)
                (hermes-test-manifest--fail "%s has invalid path: %s" label path))
              name))
          paths)))
    (hermes-test-manifest--unique normalized label)
    normalized))

(defun hermes-test-manifest--unique (items label)
  "Reject repeated ITEMS with diagnostic LABEL."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (item items)
      (when (gethash item seen)
        (hermes-test-manifest--fail "Duplicate %s entry: %S" label item))
      (puthash item t seen))))

(defun hermes-test-manifest--files (root directory)
  "Return top-level, non-hidden Lisp files in DIRECTORY under ROOT."
  (let ((path (expand-file-name directory root)))
    (unless (file-directory-p path)
      (hermes-test-manifest--fail "Missing manifest directory: %s" path))
    (mapcar (lambda (name) (concat directory "/" name))
            (cl-remove-if-not
             (lambda (name) (file-regular-p (expand-file-name name path)))
             (directory-files path nil "\\`[^.].*\\.el\\'")))))

(defun hermes-test-manifest--membership (root listed directory label)
  "Check LISTED membership in DIRECTORY under ROOT, reporting LABEL."
  (dolist (path listed)
    (unless (file-regular-p (expand-file-name path root))
      (hermes-test-manifest--fail "%s lists missing file: %s" label path)))
  (let ((actual (hermes-test-manifest--files root directory)))
    (unless (equal (sort (copy-sequence listed) #'string<)
                   (sort (copy-sequence actual) #'string<))
      (hermes-test-manifest--fail
       "%s must match %s/*.el exactly; unlisted: %S; unexpected: %S"
       label directory (cl-set-difference actual listed :test #'equal)
       (cl-set-difference listed actual :test #'equal)))))

(defun hermes-test-manifest--suite (source)
  "Return the exact-name suite path for SOURCE."
  (concat "tests/" (file-name-base source) "-tests.el"))

(defun hermes-test-manifest--source (suite)
  "Return the exact-name production source path for SUITE."
  (concat "lisp/" (string-remove-suffix "-tests.el"
                                             (file-name-nondirectory suite)) ".el"))

(defun hermes-test-manifest--edge (edge sources tests support)
  "Validate EDGE against SOURCES, TESTS and SUPPORT, including role necessity."
  (unless (and (proper-list-p edge) (= (length edge) 3)
               (stringp (nth 0 edge)) (stringp (nth 1 edge))
               (memq (nth 2 edge) '(forward reverse both infrastructure)))
    (hermes-test-manifest--fail "Malformed exception edge: %S" edge))
  (pcase-let ((`(,owner ,suite ,role) edge))
    (unless (member owner (if (eq role 'infrastructure) support sources))
      (hermes-test-manifest--fail "Missing exception owner endpoint: %s" owner))
    (unless (member suite tests)
      (hermes-test-manifest--fail "Missing exception suite endpoint: %s" suite))
    (when (and (memq role '(forward both))
               (member (hermes-test-manifest--suite owner) tests))
      (hermes-test-manifest--fail "Stale forward exception for %s: exact suite exists" owner))
    (when (and (memq role '(reverse both infrastructure))
               (member (hermes-test-manifest--source suite) sources))
      (hermes-test-manifest--fail "Stale reverse exception for %s: exact source exists" suite))))

(cl-defun hermes-test-manifest-validate
    (root sources tests support &optional (edges hermes-test-manifest-edges))
  "Validate full explicit manifests under ROOT and return t.
SOURCES, TESTS and SUPPORT are lists of root-relative filenames.  One
leading ./ is accepted.  SOURCES must equal top-level lisp/*.el; TESTS
and SUPPORT together must equal top-level tests/*.el, without duplicates.
Every source needs an exact-name suite or a forward edge, and every suite
needs an exact-name source or a reverse/infrastructure edge.  SUPPORT
never counts as a suite.  EDGES defaults to `hermes-test-manifest-edges';
pass nil explicitly to validate an exact-pairs-only tree.
Signal `hermes-test-manifest-error' for invalid membership or mappings."
  (let ((sources (hermes-test-manifest--paths sources "lisp" "SRCS"))
        (tests (hermes-test-manifest--paths tests "tests" "TESTS"))
        (support (hermes-test-manifest--paths support "tests" "TEST_SUPPORT")))
    (hermes-test-manifest--unique (append tests support) "TESTS + TEST_SUPPORT")
    (hermes-test-manifest--membership root sources "lisp" "SRCS")
    (hermes-test-manifest--membership root (append tests support) "tests"
                                      "TESTS + TEST_SUPPORT")
    (dolist (suite tests)
      (unless (string-suffix-p "-tests.el" suite)
        (hermes-test-manifest--fail "Suite must end in -tests.el: %s" suite)))
    (dolist (edge edges)
      (hermes-test-manifest--edge edge sources tests support))
    (hermes-test-manifest--unique (mapcar (lambda (edge) (list (car edge) (cadr edge))) edges)
                                 "exception edge")
    (dolist (source sources)
      (unless (or (member (hermes-test-manifest--suite source) tests)
                  (cl-some (lambda (edge)
                             (and (equal source (car edge))
                                  (memq (nth 2 edge) '(forward both))))
                           edges))
        (hermes-test-manifest--fail "Unmapped source: %s" source)))
    (dolist (suite tests)
      (unless (or (member (hermes-test-manifest--source suite) sources)
                  (cl-some (lambda (edge)
                             (and (equal suite (cadr edge))
                                  (memq (nth 2 edge) '(reverse both infrastructure))))
                           edges))
        (hermes-test-manifest--fail "Unmapped suite: %s" suite)))
    t))

(defun hermes-test-manifest-batch ()
  "Validate manifests from environment variables for a native batch build.
Read HERMES_MANIFEST_ROOT, HERMES_MANIFEST_SRCS, HERMES_MANIFEST_TESTS and
HERMES_MANIFEST_SUPPORT.  The three manifest values are whitespace-separated
filenames; an explicitly empty value is permitted.  All variables must be
set.  Errors propagate to Emacs batch's nonzero exit status."
  (let ((values
         (mapcar (lambda (name)
                   (or (getenv name)
                       (hermes-test-manifest--fail "Unset environment variable: %s" name)))
                 '("HERMES_MANIFEST_ROOT" "HERMES_MANIFEST_SRCS"
                   "HERMES_MANIFEST_TESTS" "HERMES_MANIFEST_SUPPORT"))))
    (when (string-empty-p (car values))
      (hermes-test-manifest--fail "HERMES_MANIFEST_ROOT must not be empty"))
    (apply #'hermes-test-manifest-validate
           (car values) (mapcar #'split-string (cdr values)))
    (message "Source/test manifests and bidirectional mappings verified")))

(provide 'hermes-test-manifest)
;;; hermes-test-manifest.el ends here
