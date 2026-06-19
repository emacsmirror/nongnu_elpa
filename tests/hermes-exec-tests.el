;;; hermes-exec-tests.el --- Tests for hermes-exec  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" root)))

(require 'hermes-dashboard-transport)
(require 'hermes-exec)

;;; Group 1: evaluation

(ert-deftest hermes-exec-test-evaluate-returns-ok-result ()
  (let ((result (hermes-exec--evaluate "(+ 1 2)")))
    (should (plist-get result :ok))
    (should (equal "3" (plist-get result :result)))))

(ert-deftest hermes-exec-test-evaluate-captures-error ()
  "A signalling form returns an error plist, not a thrown error."
  (let ((result (hermes-exec--evaluate "(error \"boom\")")))
    (should-not (plist-get result :ok))
    (should (string-match-p "boom" (plist-get result :error)))))

(ert-deftest hermes-exec-test-evaluate-redacts-secret ()
  "A token-bearing result is redacted by the shared redaction helper."
  (let ((result (hermes-exec--evaluate "\"ws://h/api/ws?token=supersecret\"")))
    (should (plist-get result :ok))
    (should-not (string-match-p "supersecret" (plist-get result :result)))
    (should (string-match-p "<redacted>" (plist-get result :result)))))

(ert-deftest hermes-exec-test-evaluate-caps-output ()
  "An oversized result is truncated to `hermes-exec-max-output'."
  (let* ((hermes-exec-max-output 50)
         (result (hermes-exec--evaluate "(make-string 1000 ?x)")))
    (should (plist-get result :ok))
    (should (<= (length (plist-get result :result)) 50))))

;;; Group 2: approval gate

(defvar hermes-exec-test--canary nil)

(ert-deftest hermes-exec-test-approval-declined-skips-eval ()
  "Declining approval returns a declined plist without evaluating."
  (setq hermes-exec-test--canary nil)
  (let ((hermes-exec-require-approval t))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (let ((result (hermes-exec--maybe-evaluate
                     "(setq hermes-exec-test--canary 'ran)")))
        (should-not (plist-get result :ok))
        (should (equal "Evaluation declined by user" (plist-get result :error))))))
  (should (null hermes-exec-test--canary)))

(ert-deftest hermes-exec-test-approval-disabled-runs-unprompted ()
  "With approval disabled, eval runs and no prompt is shown."
  (setq hermes-exec-test--canary nil)
  (let ((hermes-exec-require-approval nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _) (error "should not prompt"))))
      (let ((result (hermes-exec--maybe-evaluate
                     "(setq hermes-exec-test--canary 'ran)")))
        (should (plist-get result :ok)))))
  (should (eq hermes-exec-test--canary 'ran)))

;;; Group 3: HTTP request parsing

(defun hermes-exec-test--raw-request (body)
  "Return a raw POST /eval request string carrying JSON BODY."
  (format "POST /eval HTTP/1.1\r\nHost: x\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s"
          (string-bytes body) body))

(ert-deftest hermes-exec-test-parse-complete-request ()
  (let* ((body "{\"code\":\"(+ 1 2)\"}")
         (request (hermes-exec--parse-request
                   (hermes-exec-test--raw-request body))))
    (should request)
    (should (equal "POST" (plist-get request :method)))
    (should (equal "/eval" (plist-get request :path)))
    (should (equal body (plist-get request :body)))))

(ert-deftest hermes-exec-test-parse-waits-for-full-body ()
  "Parsing only succeeds once the full Content-Length body has arrived."
  (let* ((body "{\"code\":\"(+ 1 2)\"}")
         (raw (hermes-exec-test--raw-request body))
         (split (+ (string-search "\r\n\r\n" raw) 4 4))
         (chunk1 (substring raw 0 split))
         (chunk2 (substring raw split)))
    (should-not (hermes-exec--parse-request chunk1))
    (should (hermes-exec--parse-request (concat chunk1 chunk2)))))

(ert-deftest hermes-exec-test-eval-response-body-roundtrip ()
  "An /eval body parses, evaluates, and serializes to ok+result JSON."
  (let* ((hermes-exec-require-approval nil)
         (json (hermes-exec--eval-response-body "{\"code\":\"(+ 1 2)\"}"))
         (object (json-parse-string json :object-type 'alist)))
    (should (eq t (cdr (assq 'ok object))))
    (should (equal "3" (cdr (assq 'result object))))))

;;; Group 4: host resolution

(ert-deftest hermes-exec-test-resolve-host-loopback ()
  "A loopback dashboard URL resolves to 127.0.0.1 when no host is set."
  (let ((hermes-exec-host nil)
        (hermes-dashboard-transport-url "http://127.0.0.1:9119"))
    (should (equal "127.0.0.1" (hermes-exec--resolve-host)))))

(ert-deftest hermes-exec-test-resolve-host-remote-nil ()
  "A remote dashboard URL with no explicit host resolves to nil."
  (let ((hermes-exec-host nil)
        (hermes-dashboard-transport-url "http://10.0.0.5:9119"))
    (should (null (hermes-exec--resolve-host)))))

(ert-deftest hermes-exec-test-resolve-host-explicit ()
  "An explicit `hermes-exec-host' wins over dashboard resolution."
  (let ((hermes-exec-host "100.64.0.1")
        (hermes-dashboard-transport-url "http://127.0.0.1:9119"))
    (should (equal "100.64.0.1" (hermes-exec--resolve-host)))))

(provide 'hermes-exec-tests)
;;; hermes-exec-tests.el ends here
