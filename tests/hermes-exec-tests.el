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

(ert-deftest hermes-exec-test-evaluate-redacts-error ()
  "A token-bearing error message is redacted, like the success path."
  (let ((result (hermes-exec--evaluate
                 "(error \"ws://h/api/ws?token=supersecret\")")))
    (should-not (plist-get result :ok))
    (should-not (string-match-p "supersecret" (plist-get result :error)))
    (should (string-match-p "<redacted>" (plist-get result :error)))))

(ert-deftest hermes-exec-test-eval-response-body-redacts-error ()
  "A token-bearing error in the response body is redacted."
  (let* ((hermes-exec-require-approval nil)
         (hermes-exec-enabled t)
         (json (hermes-exec--eval-response-body
                "{\"code\":\"(error \\\"ws://h/api/ws?token=supersecret\\\")\"}")))
    (should-not (string-match-p "supersecret" json))
    (should (string-match-p "<redacted>" json))))

(ert-deftest hermes-exec-test-evaluate-caps-output ()
  "An oversized result is truncated to `hermes-exec-max-output'."
  (let* ((hermes-exec-max-output 50)
         (result (hermes-exec--evaluate "(make-string 1000 ?x)")))
    (should (plist-get result :ok))
    (should (<= (length (plist-get result :result)) 50))))

;;; Group 2: eval outcome and approval gate

(defvar hermes-exec-test--canary nil)

(ert-deftest hermes-exec-test-evaluate-runs-multiple-forms ()
  "Several top-level forms all run; the last value is returned."
  (setq hermes-exec-test--canary nil)
  (let ((result (hermes-exec--evaluate
                 "(setq hermes-exec-test--canary 'first) (+ 40 2)")))
    (should (plist-get result :ok))
    (should (equal "42" (plist-get result :result)))
    (should (eq hermes-exec-test--canary 'first))))

(ert-deftest hermes-exec-test-no-approval-runs-unprompted ()
  "With approval disabled, the eval outcome runs and returns a result plist."
  (setq hermes-exec-test--canary nil)
  (let ((hermes-exec-require-approval nil)
        (hermes-exec-enabled t))
    (should (plist-get (hermes-exec--eval-outcome
                        "(setq hermes-exec-test--canary 'ran)")
                       :ok)))
  (should (eq hermes-exec-test--canary 'ran)))

(ert-deftest hermes-exec-test-always-ask-defers-without-evaluating ()
  "With the always-ask policy the eval outcome defers and nothing runs."
  (setq hermes-exec-test--canary nil)
  (let ((hermes-exec-enabled t)
        (hermes-exec-require-approval t))
    (should (eq 'defer (hermes-exec--eval-outcome
                        "(setq hermes-exec-test--canary 'ran)"))))
  (should (null hermes-exec-test--canary)))

(ert-deftest hermes-exec-test-approval-choices-use-char-prefixed-labels ()
  "Approval choices are literal, readable, and keyed by their first label char."
  (let ((base '((?a "approve once" "approve once - evaluate this request")
                (?d "deny" "deny - decline this request")
                (?v "view" "view - inspect the full request")))
        (trust '((?t "trust for this session"
                     "trust for this session - ordinary forms run, sensitive ones still prompt"))))
    (let ((hermes-exec-require-approval t))
      (should (equal (hermes-exec--approval-choices)
                     (append base trust))))
    (let ((hermes-exec-require-approval #'hermes-exec-confirm-by-risk))
      (should (equal (hermes-exec--approval-choices) base)))))

(ert-deftest hermes-exec-test-disabled-endpoint-refuses-to-evaluate ()
  "A disabled endpoint returns an error result without evaluating."
  (setq hermes-exec-test--canary nil)
  (let ((hermes-exec-enabled nil)
        (hermes-exec-require-approval nil))
    (let ((result (hermes-exec--eval-outcome
                   "(setq hermes-exec-test--canary 'ran)")))
      (should-not (plist-get result :ok))
      (should (string-match-p "disabled" (plist-get result :error)))))
  (should (null hermes-exec-test--canary)))

(ert-deftest hermes-exec-test-policy-deny-skips-eval ()
  "A policy returning `deny' refuses without evaluating."
  (setq hermes-exec-test--canary nil)
  (let ((hermes-exec-enabled t)
        (hermes-exec-require-approval (lambda (_code) 'deny)))
    (let ((result (hermes-exec--eval-outcome
                   "(setq hermes-exec-test--canary 'ran)")))
      (should-not (plist-get result :ok))
      (should (string-match-p "declined by policy" (plist-get result :error)))))
  (should (null hermes-exec-test--canary)))

(ert-deftest hermes-exec-test-skips-eval-when-client-disconnected ()
  "A dead connection makes the guarded evaluator skip the eval and report it."
  (setq hermes-exec-test--canary nil)
  (let ((hermes-exec--connection 'fake-conn))
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (p) (not (eq p 'fake-conn)))))
      (let ((result (hermes-exec--evaluate-guarded
                     "(setq hermes-exec-test--canary 'ran)")))
        (should-not (plist-get result :ok))
        (should (string-match-p "disconnected" (plist-get result :error))))))
  (should (null hermes-exec-test--canary)))

(ert-deftest hermes-exec-test-start-refuses-when-disabled ()
  "`hermes-exec-start' refuses to bind while `hermes-exec-enabled' is nil."
  (let ((hermes-exec-enabled nil)
        (hermes-exec--process nil))
    (cl-letf (((symbol-function 'hermes-exec--start-server)
               (lambda (&rest _) (error "must not start a disabled endpoint"))))
      (should-error (hermes-exec-start) :type 'user-error))))

;;; Group 2b: asynchronous approval queue

(defmacro hermes-exec-test--with-pending (proc-var sent-var &rest body)
  "Run BODY with a live pipe process in PROC-VAR and captured response in SENT-VAR.
`hermes-exec--send-response' is stubbed to store its response string in SENT-VAR
instead of writing to the socket, and the approval queue is reset and cleaned."
  (declare (indent 2))
  `(let ((,proc-var (make-pipe-process :name "hermes-exec-test" :noquery t))
         (hermes-exec--pending nil)
         (hermes-exec--active nil)
         (hermes-exec-enabled t)
         ,sent-var)
     (unwind-protect
         (cl-letf (((symbol-function 'hermes-exec--send-response)
                    (lambda (_proc response) (setq ,sent-var response)))
                   ((symbol-function 'hermes-exec--display-approval)
                    (lambda (_buffer) nil))
                   ((symbol-function 'hermes-exec--maybe-prompt)
                    #'ignore))
           ,@body)
       (delete-process ,proc-var)
       (when (get-buffer hermes-exec--approval-buffer-name)
         (kill-buffer hermes-exec--approval-buffer-name)))))

(ert-deftest hermes-exec-test-enqueue-shows-code ()
  "Queuing a request prepares a buffer with metadata and request code."
  (hermes-exec-test--with-pending proc _sent
                                  (hermes-exec--enqueue-approval proc "(message \"hi\")")
                                  (let ((text (with-current-buffer hermes-exec--approval-buffer-name
                                                (buffer-substring-no-properties
                                                 (point-min) (point-max)))))
                                    (should (string-match-p "Risk" text))
                                    (should (string-match-p "(message \\\"hi\\\")" text)))))

(ert-deftest hermes-exec-test-filter-captures-origin-before-display ()
  "A deferred request stores the selected origin buffer before approval display."
  (let ((proc (make-pipe-process :name "hermes-exec-test" :noquery t))
        (origin (get-buffer-create " *hermes-exec-origin*"))
        (previous (window-buffer (selected-window)))
        (hermes-exec--pending nil)
        (hermes-exec--active nil)
        (hermes-exec-enabled t)
        (hermes-exec-require-approval t))
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-exec--display-approval)
                   (lambda (_buffer) nil))
                  ((symbol-function 'hermes-exec--maybe-prompt)
                   #'ignore))
          (set-window-buffer (selected-window) origin)
          (hermes-exec--filter
           proc (hermes-exec-test--raw-request
                 "{\"code\":\"(buffer-name)\"}"))
          (should (eq origin (plist-get hermes-exec--active :origin-buffer)))
          (should (eq (selected-window)
                      (plist-get hermes-exec--active :origin-window))))
      (set-window-buffer (selected-window) previous)
      (delete-process proc)
      (kill-buffer origin)
      (when (get-buffer hermes-exec--approval-buffer-name)
        (kill-buffer hermes-exec--approval-buffer-name)))))

(ert-deftest hermes-exec-test-approve-uses-origin-buffer ()
  "Approving evaluates with the captured origin buffer current."
  (let ((origin (get-buffer-create " *hermes-exec-origin-eval*")))
    (unwind-protect
        (hermes-exec-test--with-pending proc sent
          (hermes-exec--enqueue-approval proc "(buffer-name)"
                                         :origin-buffer origin)
          (hermes-exec-approve)
          (should (string-match-p (regexp-quote (buffer-name origin)) sent)))
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest hermes-exec-test-approve-prefers-origin-window ()
  "Approving evaluates with the captured live origin window selected."
  (let ((origin (get-buffer-create " *hermes-exec-origin-window*"))
        (fallback (get-buffer-create " *hermes-exec-origin-buffer*"))
        (window (selected-window))
        (previous (window-buffer (selected-window))))
    (unwind-protect
        (progn
          (set-window-buffer window origin)
          (hermes-exec-test--with-pending proc sent
            (hermes-exec--enqueue-approval proc "(buffer-name)"
                                           :origin-buffer fallback
                                           :origin-window window)
            (hermes-exec-approve)
            (should (string-match-p (regexp-quote (buffer-name origin)) sent))
            (should-not (string-match-p (regexp-quote (buffer-name fallback)) sent))))
      (set-window-buffer window previous)
      (when (buffer-live-p origin)
        (kill-buffer origin))
      (when (buffer-live-p fallback)
        (kill-buffer fallback)))))

(ert-deftest hermes-exec-test-approve-falls-back-when-origin-dead ()
  "Approving still evaluates when the captured origin buffer is dead."
  (let ((origin (get-buffer-create " *hermes-exec-origin-dead*"))
        (fallback (get-buffer-create " *hermes-exec-fallback*")))
    (unwind-protect
        (progn
          (kill-buffer origin)
          (with-current-buffer fallback
            (hermes-exec-test--with-pending proc sent
              (hermes-exec--enqueue-approval proc "(buffer-name)"
                                             :origin-buffer origin)
              (hermes-exec-approve)
              (should (string-match-p (regexp-quote (buffer-name fallback)) sent)))))
      (when (buffer-live-p origin)
        (kill-buffer origin))
      (when (buffer-live-p fallback)
        (kill-buffer fallback)))))

(ert-deftest hermes-exec-test-approval-buffer-shows-metadata ()
  "The approval buffer includes useful metadata plus the full code."
  (let* ((origin (get-buffer-create " *hermes-exec-meta-origin*"))
         (hermes-exec-timeout 12)
         (buffer (hermes-exec--approval-buffer
                  (list :code "(delete-file \"/tmp/x\")"
                        :risk 'sensitive
                        :peer "127.0.0.1:8237"
                        :origin-buffer origin
                        :queue-total 3)))
         (text (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min) (point-max)))))
    (unwind-protect
        (progn
          (should (string-match-p "Risk[[:space:]]*: Sensitive" text))
          (should (string-match-p "Requester[[:space:]]*: 127.0.0.1:8237" text))
          (should (string-match-p "Origin[[:space:]]*:  \\*hermes-exec-meta-origin\\*" text))
          (should (string-match-p "Queue[[:space:]]*: 1 of 3" text))
          (should (string-match-p "Timeout[[:space:]]*: 12s" text))
          (should (string-match-p "delete-file" text)))
      (kill-buffer origin)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest hermes-exec-test-prompt-choice-maps-decisions ()
  "`read-multiple-choice' results map to approve/deny resolution."
  (let ((hermes-exec--active (list :buffer nil))
        (noninteractive nil)
        decisions)
    (cl-letf (((symbol-function 'hermes-exec--resolve-active)
               (lambda (approve) (push approve decisions)))
              ((symbol-function 'read-multiple-choice)
               (lambda (&rest _) (list ?a "approve once"))))
      (hermes-exec--prompt-choice))
    (cl-letf (((symbol-function 'hermes-exec--resolve-active)
               (lambda (approve) (push approve decisions)))
              ((symbol-function 'read-multiple-choice)
               (lambda (&rest _) (list ?d "deny"))))
      (hermes-exec--prompt-choice))
    (should (equal '(nil t) decisions))))

(ert-deftest hermes-exec-test-prompt-choice-trusts-ordinary-only ()
  "Trusting the session approves ordinary, but not sensitive, active requests."
  (let ((noninteractive nil)
        decisions)
    (let ((hermes-exec--active (list :buffer nil :risk 'ordinary))
          (hermes-exec-require-approval t))
      (cl-letf (((symbol-function 'hermes-exec--resolve-active)
                 (lambda (approve) (push approve decisions)))
                ((symbol-function 'read-multiple-choice)
                 (lambda (&rest _) (list ?t "trust for this session"))))
        (hermes-exec--prompt-choice))
      (should (eq hermes-exec-require-approval #'hermes-exec-confirm-by-risk)))
    (let ((hermes-exec--active (list :buffer nil :risk 'sensitive))
          (hermes-exec-require-approval t)
          (answers '((?t "trust for this session") (?d "deny"))))
      (cl-letf (((symbol-function 'hermes-exec--resolve-active)
                 (lambda (approve) (push approve decisions)))
                ((symbol-function 'read-multiple-choice)
                 (lambda (&rest _) (pop answers))))
        (hermes-exec--prompt-choice))
      (should (eq hermes-exec-require-approval #'hermes-exec-confirm-by-risk)))
    (should (equal '(nil t) decisions))))

(ert-deftest hermes-exec-test-prompt-choice-view-selects-buffer ()
  "The view choice shows the active approval buffer without resolving."
  (let ((buffer (get-buffer-create " *hermes-exec-view-choice*"))
        (noninteractive nil)
        resolved)
    (unwind-protect
        (let ((hermes-exec--active (list :buffer buffer)))
          (cl-letf (((symbol-function 'hermes-exec--resolve-active)
                     (lambda (&rest _) (setq resolved t)))
                    ((symbol-function 'read-multiple-choice)
                     (lambda (&rest _) (list ?v "view"))))
            (hermes-exec--prompt-choice))
          (should (eq (current-buffer) buffer))
          (should-not resolved))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest hermes-exec-test-approval-keymap-keeps-yes-no-aliases ()
  "The approval buffer keeps old yes/no/quit aliases while adding char choices."
  (should (eq (lookup-key hermes-exec-approval-mode-map (kbd "a"))
              #'hermes-exec-approve))
  (should (eq (lookup-key hermes-exec-approval-mode-map (kbd "y"))
              #'hermes-exec-approve))
  (should (eq (lookup-key hermes-exec-approval-mode-map (kbd "d"))
              #'hermes-exec-deny))
  (should (eq (lookup-key hermes-exec-approval-mode-map (kbd "n"))
              #'hermes-exec-deny))
  (should (eq (lookup-key hermes-exec-approval-mode-map (kbd "q"))
              #'hermes-exec-deny)))

(ert-deftest hermes-exec-test-peer-info-formats-host-and-port ()
  "Peer info displays the service element from `process-contact', not the cdr."
  (cl-letf (((symbol-function 'processp) (lambda (_proc) t))
            ((symbol-function 'process-live-p) (lambda (_proc) t))
            ((symbol-function 'process-contact)
             (lambda (&rest _) '("127.0.0.1" 54321))))
    (should (equal "127.0.0.1:54321"
                   (hermes-exec--peer-info 'fake-proc)))))

(ert-deftest hermes-exec-test-approve-evaluates-responds-and-cleans-up ()
  "Approving evaluates the code, sends an ok response, and kills the buffer."
  (setq hermes-exec-test--canary nil)
  (hermes-exec-test--with-pending proc sent
                                  (hermes-exec--enqueue-approval proc "(setq hermes-exec-test--canary 'ran)")
                                  (hermes-exec-approve)
                                  (should (eq hermes-exec-test--canary 'ran))
                                  (should (string-prefix-p "HTTP/1.1 200 OK" sent))
                                  (should (string-match-p "\"ok\":true" sent))
                                  (should-not (get-buffer hermes-exec--approval-buffer-name))
                                  (should-not hermes-exec--active)))

(ert-deftest hermes-exec-test-deny-skips-eval-responds-and-cleans-up ()
  "Denying skips the eval, sends a declined response, and kills the buffer."
  (setq hermes-exec-test--canary nil)
  (hermes-exec-test--with-pending proc sent
                                  (hermes-exec--enqueue-approval proc "(setq hermes-exec-test--canary 'ran)")
                                  (hermes-exec-deny)
                                  (should (null hermes-exec-test--canary))
                                  (should (string-match-p "declined by user" sent))
                                  (should-not (get-buffer hermes-exec--approval-buffer-name))
                                  (should-not hermes-exec--active)))

(ert-deftest hermes-exec-test-dead-client-dropped-from-queue ()
  "A connection that dies while queued is dropped and never evaluated."
  (setq hermes-exec-test--canary nil)
  (hermes-exec-test--with-pending proc _sent
                                  (hermes-exec--enqueue-approval proc "(setq hermes-exec-test--canary 'ran)")
                                  (delete-process proc)
                                  (hermes-exec--drop-pending proc)
                                  (should (null hermes-exec--active))
                                  (should (null hermes-exec--pending))
                                  (should-not (get-buffer hermes-exec--approval-buffer-name)))
  (should (null hermes-exec-test--canary)))

(ert-deftest hermes-exec-test-fifo-advances-to-next-request ()
  "Approving the active request promotes the next queued request in order."
  (let ((proc1 (make-pipe-process :name "hermes-exec-test-1" :noquery t))
        (proc2 (make-pipe-process :name "hermes-exec-test-2" :noquery t))
        (hermes-exec--pending nil)
        (hermes-exec--active nil)
        (hermes-exec-enabled t))
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-exec--send-response)
                   (lambda (&rest _) nil))
                  ((symbol-function 'hermes-exec--display-approval)
                   (lambda (_buffer) nil))
                  ((symbol-function 'hermes-exec--maybe-prompt)
                   #'ignore))
          (hermes-exec--enqueue-approval proc1 "(+ 1 1)")
          (hermes-exec--enqueue-approval proc2 "(+ 2 2)")
          (should (eq proc1 (plist-get hermes-exec--active :proc)))
          (should (= 1 (length hermes-exec--pending)))
          (should (string-match-p
                   "Queue[[:space:]]*: 1 of 2"
                   (with-current-buffer hermes-exec--approval-buffer-name
                     (buffer-substring-no-properties (point-min) (point-max)))))
          (hermes-exec-approve)
          (should (eq proc2 (plist-get hermes-exec--active :proc)))
          (should (null hermes-exec--pending))
          (should (string-match-p
                   (regexp-quote "(+ 2 2)")
                   (with-current-buffer hermes-exec--approval-buffer-name
                     (buffer-substring-no-properties (point-min) (point-max))))))
      (delete-process proc1)
      (delete-process proc2)
      (when (get-buffer hermes-exec--approval-buffer-name)
        (kill-buffer hermes-exec--approval-buffer-name)))))

(ert-deftest hermes-exec-test-queue-cap-declines-overflow ()
  "A request past `hermes-exec-max-pending' is declined rather than queued."
  (let ((proc (make-pipe-process :name "hermes-exec-test" :noquery t))
        (hermes-exec--pending nil)
        (hermes-exec--active nil)
        (hermes-exec-enabled t)
        (hermes-exec-max-pending 1)
        sent)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-exec--send-response)
                   (lambda (_p response) (setq sent response))))
          (hermes-exec--enqueue-approval proc "(+ 1 1)")
          (should hermes-exec--active)
          (hermes-exec--enqueue-approval proc "(+ 2 2)")
          (should (null hermes-exec--pending))
          (should (string-match-p "Too many pending" sent)))
      (delete-process proc)
      (when (get-buffer hermes-exec--approval-buffer-name)
        (kill-buffer hermes-exec--approval-buffer-name)))))

;;; Group 2c: trust policy and risk classification

(ert-deftest hermes-exec-test-classify-ordinary ()
  "Plain computation and messaging classify as ordinary."
  (should (eq 'ordinary (hermes-exec--classify-code "(+ 1 2)")))
  (should (eq 'ordinary (hermes-exec--classify-code "(message \"hi\")"))))

(ert-deftest hermes-exec-test-classify-sensitive ()
  "Blocklisted functions and dynamic dispatch classify as sensitive."
  (should (eq 'sensitive (hermes-exec--classify-code "(delete-file \"/tmp/x\")")))
  (should (eq 'sensitive (hermes-exec--classify-code "(shell-command \"ls\")")))
  (should (eq 'sensitive (hermes-exec--classify-code "(funcall fn)"))))

(ert-deftest hermes-exec-test-classify-unreadable-fails-closed ()
  "Code that cannot be read classifies as sensitive, not ordinary."
  (should (eq 'sensitive (hermes-exec--classify-code "(+ 1 2"))))

(ert-deftest hermes-exec-test-classify-forbidden ()
  "A configured forbidden function classifies as forbidden."
  (let ((hermes-exec-forbidden-functions '(kill-emacs)))
    (should (eq 'forbidden (hermes-exec--classify-code "(kill-emacs)")))))

(ert-deftest hermes-exec-test-confirm-by-risk ()
  "The risk predicate runs ordinary, prompts sensitive, denies forbidden."
  (should (null (hermes-exec-confirm-by-risk "(+ 1 2)")))
  (should (hermes-exec-confirm-by-risk "(delete-file \"/tmp/x\")"))
  (let ((hermes-exec-forbidden-functions '(kill-emacs)))
    (should (eq 'deny (hermes-exec-confirm-by-risk "(kill-emacs)")))))

(ert-deftest hermes-exec-test-approval-decision-mirrors-org-babel ()
  "t asks, nil runs, and a function classifies into run/ask/deny."
  (let ((hermes-exec-require-approval t))
    (should (eq 'ask (hermes-exec--approval-decision "(+ 1 2)"))))
  (let ((hermes-exec-require-approval nil))
    (should (eq 'run (hermes-exec--approval-decision "(delete-file \"/x\")"))))
  (let ((hermes-exec-require-approval #'hermes-exec-confirm-by-risk))
    (should (eq 'run (hermes-exec--approval-decision "(+ 1 2)")))
    (should (eq 'ask (hermes-exec--approval-decision "(shell-command \"ls\")")))
    (let ((hermes-exec-forbidden-functions '(kill-emacs)))
      (should (eq 'deny (hermes-exec--approval-decision "(kill-emacs)"))))))

(ert-deftest hermes-exec-test-trust-untrust-set-policy ()
  "The trust commands flip the policy between risk-based and always-ask."
  (let ((hermes-exec-require-approval t))
    (hermes-exec-trust)
    (should (eq hermes-exec-require-approval #'hermes-exec-confirm-by-risk))
    (hermes-exec-untrust)
    (should (eq hermes-exec-require-approval t))))

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
         (hermes-exec-enabled t)
         (json (hermes-exec--eval-response-body "{\"code\":\"(+ 1 2)\"}"))
         (object (json-parse-string json :object-type 'alist)))
    (should (eq t (cdr (assq 'ok object))))
    (should (equal "3" (cdr (assq 'result object))))))

;;; Group 4: request-size cap

(ert-deftest hermes-exec-test-oversized-request-gets-413 ()
  "A request past `hermes-exec-max-request-bytes' yields a 413 before parsing."
  (let* ((hermes-exec-max-request-bytes 64)
         (buffer (concat "POST /eval HTTP/1.1\r\n" (make-string 200 ?x)))
         (response (hermes-exec--request-response buffer)))
    (should (string-prefix-p "HTTP/1.1 413 Payload Too Large" response))
    (should (string-match-p "request too large" response))))

(ert-deftest hermes-exec-test-incomplete-request-under-cap-waits ()
  "An incomplete request under the cap returns nil so more bytes are read."
  (let ((hermes-exec-max-request-bytes 1048576))
    (should-not (hermes-exec--request-response "POST /eval HTTP/1.1\r\nHost: x"))))

(ert-deftest hermes-exec-test-complete-request-under-cap-dispatches ()
  "A complete request under the cap dispatches to a 200 response."
  (let* ((hermes-exec-require-approval nil)
         (hermes-exec-max-request-bytes 1048576)
         (raw (hermes-exec-test--raw-request "{\"code\":\"(+ 1 2)\"}"))
         (response (hermes-exec--request-response raw)))
    (should (string-prefix-p "HTTP/1.1 200 OK" response))))

(ert-deftest hermes-exec-test-request-at-exact-cap-is-allowed ()
  "A request whose size equals the cap is allowed; the bound is `>', not `>='."
  (let* ((hermes-exec-require-approval nil)
         (raw (hermes-exec-test--raw-request "{\"code\":\"(+ 1 2)\"}"))
         (hermes-exec-max-request-bytes (string-bytes raw))
         (response (hermes-exec--request-response raw)))
    (should (string-prefix-p "HTTP/1.1 200 OK" response))))

;;; Group 5: host resolution

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

(ert-deftest hermes-exec-test-allows-entire-ipv4-loopback-range ()
  "Every address in 127.0.0.0/8 is safe without Tailscale discovery."
  (should (hermes-exec--allowed-bind-host-p "127.99.8.7")))

(ert-deftest hermes-exec-test-allows-only-assigned-tailscale-addresses ()
  "Non-loopback binds must match a locally assigned Tailscale address."
  (cl-letf (((symbol-function 'hermes-exec--tailscale-ipv4-addresses)
             (lambda () '("100.71.2.3" "198.18.4.5"))))
    (should (hermes-exec--allowed-bind-host-p "100.71.2.3"))
    (should (hermes-exec--allowed-bind-host-p "198.18.4.5"))
    (should-not (hermes-exec--allowed-bind-host-p "100.71.2.4"))
    (should-not (hermes-exec--allowed-bind-host-p "10.0.0.8"))
    (should-not (hermes-exec--allowed-bind-host-p "192.168.1.10"))))

;;; Group 6: authentication

(defun hermes-exec-tests--request (&optional authorization)
  "Return a parsed-request plist carrying an optional AUTHORIZATION header."
  (list :method "POST" :path "/eval"
        :headers (and authorization (list (cons "authorization" authorization)))
        :body "{}"))

(defmacro hermes-exec-tests--without-env-token (&rest body)
  "Run BODY with the EMACS_EXEC_TOKEN environment variable removed."
  `(let ((process-environment
          (cl-remove-if (lambda (e) (string-prefix-p "EMACS_EXEC_TOKEN=" e))
                        process-environment)))
     ,@body))

(ert-deftest hermes-exec-test-secure-equal ()
  "Constant-time compare matches equal strings and rejects others."
  (should (hermes-exec--secure-equal "abc123" "abc123"))
  (should-not (hermes-exec--secure-equal "abc123" "abc124"))
  (should-not (hermes-exec--secure-equal "abc" "abcdef"))
  (should-not (hermes-exec--secure-equal "abc" nil)))

(ert-deftest hermes-exec-test-request-bearer ()
  "The bearer token is parsed from the Authorization header, case-insensitively."
  (should (equal "tok"
                 (hermes-exec--request-bearer
                  (hermes-exec-tests--request "Bearer tok"))))
  (should (equal "tok"
                 (hermes-exec--request-bearer
                  (hermes-exec-tests--request "bearer   tok"))))
  (should (null (hermes-exec--request-bearer (hermes-exec-tests--request))))
  (should (null (hermes-exec--request-bearer
                 (hermes-exec-tests--request "Basic abc")))))

(ert-deftest hermes-exec-test-authorized-without-token ()
  "With no token configured every request is authorized."
  (hermes-exec-tests--without-env-token
   (let ((hermes-exec-token nil))
     (should (hermes-exec--request-authorized-p (hermes-exec-tests--request)))
     (should (hermes-exec--request-authorized-p
              (hermes-exec-tests--request "Bearer anything"))))))

(ert-deftest hermes-exec-test-authorized-with-token ()
  "A configured token requires a matching bearer header."
  (let ((hermes-exec-token "s3cret"))
    (should (hermes-exec--request-authorized-p
             (hermes-exec-tests--request "Bearer s3cret")))
    (should-not (hermes-exec--request-authorized-p
                 (hermes-exec-tests--request "Bearer wrong")))
    (should-not (hermes-exec--request-authorized-p
                 (hermes-exec-tests--request)))))

(ert-deftest hermes-exec-test-token-from-env ()
  "EMACS_EXEC_TOKEN is used when `hermes-exec-token' is nil."
  (let ((hermes-exec-token nil)
        (process-environment (cons "EMACS_EXEC_TOKEN=envtok" process-environment)))
    (should (equal "envtok" (hermes-exec--expected-token)))
    (should (hermes-exec--request-authorized-p
             (hermes-exec-tests--request "Bearer envtok")))))

(ert-deftest hermes-exec-test-dispatch-rejects-bad-token ()
  "Dispatch answers 401 when a configured token is not matched."
  (let ((hermes-exec-token "s3cret"))
    (should (string-prefix-p
             "HTTP/1.1 401 Unauthorized"
             (hermes-exec--dispatch (hermes-exec-tests--request "Bearer wrong"))))))

(ert-deftest hermes-exec-test-start-refuses-non-loopback-without-token ()
  "Starting on a non-loopback host without a token is refused."
  (hermes-exec-tests--without-env-token
   (let ((hermes-exec-enabled t)
         (hermes-exec-token nil)
         (hermes-exec-host "100.64.0.1")
         (hermes-exec--process nil))
     (should-error (hermes-exec-start) :type 'user-error))))

(ert-deftest hermes-exec-test-start-allows-ipv4-loopback-without-token ()
  "Starting anywhere in 127.0.0.0/8 does not require a bearer token."
  (hermes-exec-tests--without-env-token
   (let ((hermes-exec-enabled t)
         (hermes-exec-token nil)
         (hermes-exec-host "127.99.8.7")
         (hermes-exec--process nil)
         started)
     (cl-letf (((symbol-function 'hermes-exec--start-server)
                (lambda (host) (setq started host) 'server))
               ((symbol-function 'message) #'ignore))
       (hermes-exec-start)
       (should (equal started "127.99.8.7"))))))

(ert-deftest hermes-exec-test-start-refuses-wildcard-with-token ()
  "A bearer token must not permit binding the eval endpoint to all interfaces."
  (let ((hermes-exec-enabled t)
        (hermes-exec-token "secret")
        (hermes-exec-host "0.0.0.0")
        (hermes-exec--process nil)
        started)
    (cl-letf (((symbol-function 'hermes-exec--start-server)
               (lambda (host) (setq started host))))
      (should-error (hermes-exec-start) :type 'user-error)
      (should-not started))))

(ert-deftest hermes-exec-test-start-refuses-public-address-with-token ()
  "A bearer token must not permit binding the eval endpoint to a public address."
  (let ((hermes-exec-enabled t)
        (hermes-exec-token "secret")
        (hermes-exec-host "203.0.113.10")
        (hermes-exec--process nil)
        started)
    (cl-letf (((symbol-function 'hermes-exec--start-server)
               (lambda (host) (setq started host))))
      (should-error (hermes-exec-start) :type 'user-error)
      (should-not started))))

;;; Group 7: server lifecycle helpers

(ert-deftest hermes-exec-test-bound-host-prefers-process-contact ()
  "While live, the bound host comes from the process, not re-resolution.
The resolver would say 100.64.0.9, so reading 127.0.0.1 proves the bound host
wins."
  (let* ((hermes-exec-host "100.64.0.9")
         (hermes-exec-port t)
         (hermes-exec--process (hermes-exec--start-server "127.0.0.1")))
    (unwind-protect
        (should (equal "127.0.0.1" (hermes-exec--bound-host)))
      (delete-process hermes-exec--process))))

(ert-deftest hermes-exec-test-accept-tags-connection ()
  "The accept handler tags a connection so teardown can match it."
  (let ((conn (make-pipe-process :name "hermes-exec-test" :noquery t)))
    (unwind-protect
        (progn
          (hermes-exec--accept nil conn nil)
          (should (process-get conn 'hermes-exec-connection)))
      (delete-process conn))))

;;; Group 8: bridge registration

(ert-deftest hermes-exec-test-show-bridge-command-uses-packaged-entry-point ()
  "The bridge command uses the packaged script and keeps environment options parsed."
  (let ((hermes-exec-port 8237))
    (cl-letf (((symbol-function 'hermes-exec--detect-host)
               (lambda () "127.0.0.1"))
              ((symbol-function 'hermes-exec--expected-token)
               (lambda () nil)))
      (should
       (equal
        (hermes-exec-show-bridge-command)
        "hermes mcp add emacs --command hermes-emacs-mcp --env EMACS_EXEC_HOST=127.0.0.1 EMACS_EXEC_PORT=8237")))))

(provide 'hermes-exec-tests)
;;; hermes-exec-tests.el ends here
