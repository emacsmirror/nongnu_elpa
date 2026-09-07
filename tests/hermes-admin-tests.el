;;; hermes-admin-tests.el --- Administrative browser tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'hermes-test-helpers)
(require 'hermes-admin)

(ert-deftest hermes-admin-native-browsers-available ()
  "Both administrative views are native list browsers with guarded actions."
  (dolist (mode '(hermes-pairing-mode hermes-webhooks-mode))
    (should (fboundp mode))
    (with-temp-buffer
      (funcall mode)
      (should (derived-mode-p 'tabulated-list-mode))
      (should (commandp (key-binding (kbd "?"))))
      (should-error (hermes-admin--require-ready) :type 'user-error))))

(defmacro hermes-admin-test--with (mode &rest body)
  "Run BODY in MODE with deferred authenticated REST calls."
  (declare (indent 1))
  `(with-temp-buffer
     (,mode)
     (setq hermes-instance (list :id "fixture" :name "fixture" :url "http://example.test"))
     (let ((client (make-hermes-dashboard-transport-client
                    :base-url "http://example.test" :token "fixture-token"))
           requests (releases 0) notices)
       (cl-letf (((symbol-function 'hermes-browser--with-client)
                  (lambda (fn) (funcall fn client (lambda () (cl-incf releases)))))
                 ((symbol-function 'hermes-dashboard-transport-api-request-async)
                  (lambda (method path &rest args)
                    (let ((promise (hermes--promise-make)))
                      (push (list method path args promise) requests)
                      promise)))
                 ((symbol-function 'message)
                  (lambda (format &rest args)
                    (push (apply #'format format args) notices)))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         ,@body))))

(defconst hermes-admin-test--pairing
  '((pending . (((platform . "xmpp") (user_id . "pending-user")
                 (user_name . "Pending") (request_id . "request-123"))))
    (approved . (((platform . "xmpp") (user_id . "trusted-user")
                  (user_name . "Trusted"))))))

(defconst hermes-admin-test--webhooks
  '((enabled . t)
    (subscriptions . (((name . "route-one") (enabled . t)
                       (deliver . "log") (description . "Fixture"))))))

(ert-deftest hermes-admin-pairing-route-journey ()
  "List, approve, revoke and clear use request IDs and authoritative readback."
  (hermes-admin-test--with hermes-pairing-mode
    (hermes-admin--revert)
    (should (eq hermes-admin--state 'loading))
    (should (equal (cadar requests) "/api/pairing"))
    (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--pairing)
    (should (eq hermes-admin--state 'ready))
    (should (= releases 1))
    (goto-char (point-min))
    (hermes-pairing-approve)
    (should (eq hermes-admin--state 'mutating))
    (should-error (hermes-admin--revert) :type 'user-error)
    (should-error (hermes-pairing-approve) :type 'user-error)
    (should (equal (cadar requests) "/api/pairing/approve"))
    (should (equal (plist-get (nth 2 (car requests)) :body)
                   '((platform . "xmpp") (request_id . "request-123"))))
    (hermes--promise-resolve (nth 3 (car requests)) '((ok . t)))
    (should (eq hermes-admin--state 'loading))
    (should (equal (cadar requests) "/api/pairing"))
    (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--pairing)
    (goto-char (point-min)) (forward-line 1)
    (hermes-pairing-revoke)
    (should (equal (cadar requests) "/api/pairing/revoke"))
    (should (equal (plist-get (nth 2 (car requests)) :body)
                   '((platform . "xmpp") (user_id . "trusted-user"))))
    (hermes--promise-resolve (nth 3 (car requests)) '((ok . t)))
    (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--pairing)
    (hermes-pairing-clear-pending)
    (should (equal (cadar requests) "/api/pairing/clear-pending"))
    (hermes--promise-resolve (nth 3 (car requests)) '((ok . t) (cleared . 1)))
    (hermes--promise-resolve (nth 3 (car requests)) '((pending) (approved)))
    (should (eq hermes-admin--state 'ready))
    (should-not tabulated-list-entries)
    (should (= releases (length requests)))))

(ert-deftest hermes-admin-webhook-route-journey-and-secret ()
  "Create, toggle and delete never put one-time secrets into ordinary surfaces."
  (hermes-admin-test--with hermes-webhooks-mode
    (let ((kill-ring nil) (minibuffer-history nil)
          (inputs '("new-route" "Description" "Prompt")))
      (hermes-admin--revert)
      (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--webhooks)
      (goto-char (point-min))
      (hermes-webhooks-toggle)
      (should (equal (caar requests) "PUT"))
      (should (equal (cadar requests) "/api/webhooks/route-one/enabled"))
      (should (equal (plist-get (nth 2 (car requests)) :body) '((enabled . :false))))
      (hermes--promise-resolve (nth 3 (car requests)) '((ok . t)))
      (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--webhooks)
      (goto-char (point-min))
      (hermes-webhooks-delete)
      (should (equal (caar requests) "DELETE"))
      (should (equal (cadar requests) "/api/webhooks/route-one"))
      (hermes--promise-resolve (nth 3 (car requests)) '((ok . t)))
      (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--webhooks)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (pop inputs))))
        (hermes-webhooks-create))
      (should (equal (cadar requests) "/api/webhooks"))
      (should-not (assq 'secret (plist-get (nth 2 (car requests)) :body)))
      (hermes--promise-resolve (nth 3 (car requests))
                              '((name . "new-route") (secret . "one-time-fixture")))
      (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--webhooks)
      (should (equal hermes-admin--secret "one-time-fixture"))
      (should-not (string-match-p "one-time-fixture"
                                  (format "%s%s%s%s%s" (buffer-string)
                                          hermes-admin--snapshot notices
                                          kill-ring minibuffer-history)))
      (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
        (hermes-webhooks-reveal-secret))
      (let ((reveal hermes-admin--secret-buffer))
        (with-current-buffer reveal
          (should (equal (buffer-string) "one-time-fixture"))
          (should (eq buffer-undo-list t))
          (should-not buffer-file-name))
        (hermes-admin--forget-secret)
        (should-not (buffer-live-p reveal))
        (should-not hermes-admin--secret)))))

(ert-deftest hermes-admin-failure-and-timeout-never-replay ()
  "Timeout and rejection disable changes until an explicit authoritative read."
  (dolist (reason '("timed out" "disconnected" "HTTP 401 fixture-secret" "HTTP 500"))
    (hermes-admin-test--with hermes-pairing-mode
      (hermes-admin--revert)
      (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--pairing)
      (hermes-pairing-clear-pending)
      (hermes--promise-reject (nth 3 (car requests)) reason)
      (should (eq hermes-admin--state 'uncertain))
      (should (= (length requests) 2))
      (should-error (hermes-pairing-clear-pending) :type 'user-error)
      (should-not notices)
      (hermes-admin--revert)
      (hermes--promise-reject (nth 3 (car requests)) reason)
      (should (eq hermes-admin--state 'failed))
      (should-not notices))))

(ert-deftest hermes-admin-list-replacement-and-disconnect ()
  "Old reads cannot replace a newer request, instance, mode or connection."
  (dolist (ending '(refresh instance mutate-instance mode kill disconnect endpoint))
    (hermes-admin-test--with hermes-pairing-mode
      (hermes-admin--revert)
      (let ((promise (nth 3 (car requests))))
        (pcase ending
          ('refresh (hermes-admin--revert))
          ('instance (setq hermes-instance (copy-tree hermes-instance)))
          ('mutate-instance (setf (plist-get hermes-instance :name) "replacement"))
          ('mode (fundamental-mode))
          ('kill (kill-buffer (current-buffer)))
          ('disconnect (cl-incf (hermes-dashboard-transport-client-generation client)))
          ('endpoint (setf (hermes-dashboard-transport-client-base-url client)
                           "http://other.test")))
        (hermes--promise-resolve promise hermes-admin-test--pairing)
        (should-not hermes-admin--snapshot)
        (should-not notices)
        (should (= releases 1))))))

(ert-deftest hermes-admin-pending-mutation-cannot-reach-successor ()
  "Late creates neither reveal a secret nor dispatch reads for replaced owners."
  (dolist (ending '(instance mode disconnect))
    (hermes-admin-test--with hermes-webhooks-mode
      (hermes-admin--revert)
      (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--webhooks)
      (hermes-admin--change "Create" "POST" "/api/webhooks" '((name . "new")) t)
      (pcase ending
        ('instance (setq hermes-instance (copy-tree hermes-instance)))
        ('mode (fundamental-mode))
        ('disconnect (cl-incf (hermes-dashboard-transport-client-generation client))))
      (hermes--promise-resolve (nth 3 (car requests)) '((secret . "retired-secret")))
      (should-not hermes-admin--secret)
      (should (= (length requests) 2))
      (should-not notices))))

(ert-deftest hermes-admin-confirmation-is-an-admission-fence ()
  "Changing an instance or declining a confirmation sends no mutation."
  (dolist (ending '(decline replace refresh))
    (hermes-admin-test--with hermes-pairing-mode
      (hermes-admin--revert)
      (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--pairing)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _)
                   (pcase ending
                     ('replace (setq hermes-instance (copy-tree hermes-instance)))
                     ('refresh (hermes-admin--revert)))
                   (not (eq ending 'decline)))))
        (if (eq ending 'decline)
            (hermes-pairing-clear-pending)
          (should-error (hermes-pairing-clear-pending) :type 'user-error)))
      (should (cl-every (lambda (request) (equal (car request) "GET")) requests)))))

(ert-deftest hermes-admin-authentication-is-a-transport-entry-fence ()
  "A mutation awaiting REST authentication never sends after owner replacement."
  (hermes-admin-test--with hermes-pairing-mode
    (hermes-admin--revert)
    (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--pairing)
    (setf (hermes-dashboard-transport-client-token client) nil)
    (let ((auth (hermes--promise-make)) sent)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-api-auth-async)
                 (lambda () auth))
                ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                 (lambda (&rest _) (setq sent t) (hermes--promise-resolved nil))))
        (hermes-pairing-clear-pending)
        (setq hermes-instance (copy-tree hermes-instance))
        (hermes--promise-resolve auth '(:base-url "http://example.test" :token "fixture"))
        (should-not sent)))))

(ert-deftest hermes-admin-secret-teardown-and-copy-require-intent ()
  "Copy is explicit; mode replacement destroys an open reveal."
  (hermes-admin-test--with hermes-webhooks-mode
    (hermes-admin--revert)
    (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--webhooks)
    (setq hermes-admin--secret (copy-sequence "fixture-secret")
          hermes-admin--secret-instance
          (list hermes-instance (hermes-admin--instance-value hermes-instance)))
    (let ((kill-ring nil) (interprogram-cut-function nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (hermes-webhooks-copy-secret))
      (should-not kill-ring)
      (hermes-webhooks-copy-secret)
      (should (equal (car kill-ring) "fixture-secret"))
      (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
        (hermes-webhooks-reveal-secret))
      (let ((reveal hermes-admin--secret-buffer))
        (fundamental-mode)
        (should-not (buffer-live-p reveal))
        (should-not hermes-admin--secret)
        ;; Forgetting browser memory does not pretend to erase copied data.
        (should (equal (car kill-ring) "fixture-secret"))))))

(ert-deftest hermes-admin-malformed-and-empty-lists-differ ()
  "Only a structurally present empty list is authoritative empty state."
  (hermes-admin-test--with hermes-webhooks-mode
    (hermes-admin--revert)
    (hermes--promise-resolve (nth 3 (car requests)) '((error . "fixture-secret")))
    (should (eq hermes-admin--state 'failed))
    (should-not notices)
    (hermes-admin--revert)
    (hermes--promise-resolve (nth 3 (car requests)) '((subscriptions)))
    (should (eq hermes-admin--state 'ready))
    (should-not tabulated-list-entries)))

(ert-deftest hermes-admin-synchronous-acquisition-failure-is-contained ()
  "Acquisition errors cannot strand locks or echo credentials."
  (hermes-admin-test--with hermes-pairing-mode
    (hermes-admin--revert)
    (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--pairing)
    (cl-letf (((symbol-function 'hermes-browser--with-client)
               (lambda (&rest _) (error "fixture-credential"))))
      (hermes-pairing-clear-pending))
    (should (eq hermes-admin--state 'uncertain))
    (should (= (length requests) 1))
    (should-not notices)))

(ert-deftest hermes-admin-retained-promises-scrub-secrets-even-when-stale ()
  "The raw response cannot retain another plaintext copy after settlement."
  (dolist (stale '(nil t))
    (hermes-admin-test--with hermes-webhooks-mode
      (hermes-admin--revert)
      (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--webhooks)
      (hermes-admin--change "Create" "POST" "/api/webhooks" '((name . "new")) t)
      (let ((secret (copy-sequence "one-time-secret"))
            (request (nth 3 (car requests))))
        (when stale (fundamental-mode))
        (hermes--promise-resolve request (list (cons 'secret secret)))
        (should-not (equal secret "one-time-secret"))
        (should-not (string-match-p "one-time-secret"
                                    (format "%s" (hermes--promise-value request))))
        (if stale (should-not hermes-admin--secret)
          (should (equal hermes-admin--secret "one-time-secret")))))))

(ert-deftest hermes-admin-instance-string-mutation-retires-snapshot ()
  "In-place edits to an instance string cannot inherit snapshot authority."
  (hermes-admin-test--with hermes-pairing-mode
    (setf (plist-get hermes-instance :name) (copy-sequence "fixture"))
    (hermes-admin--revert)
    (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--pairing)
    (aset (plist-get hermes-instance :name) 0 ?X)
    (should-error (hermes-pairing-clear-pending) :type 'user-error)
    (should (= (length requests) 1))))

(ert-deftest hermes-admin-popup-actions-are-native-and-bounded ()
  "Direct commands and popup descriptions share ordinary keymaps."
  (dolist (mode '(hermes-pairing-mode hermes-webhooks-mode))
    (with-temp-buffer
      (funcall mode)
      (dolist (key '("g" "q" "?" "a" "d"))
        (should (commandp (key-binding (kbd key))))))
    (let* ((map (intern (format "%s-map" mode)))
           (metadata (keymap-popup--meta (symbol-value map) 'descriptions)))
      (dolist (group metadata)
        (should (<= (length (plist-get group :entries)) 4))))))

(ert-deftest hermes-admin-create-preserves-name-and-replacement-consent ()
  "Reject aliases and duplicates; explicit consent names replacement and scope."
  (dolist (name '("Route-one" " route-one" "route one" "route-one" "new-route"))
    (hermes-admin-test--with hermes-webhooks-mode
      (hermes-admin--revert)
      (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--webhooks)
      (let ((inputs (list name "Description" "Prompt")) question
            (case-fold-search t))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (pop inputs)))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (text) (setq question text) nil)))
          (if (equal name "new-route")
              (hermes-webhooks-create)
            (should-error (hermes-webhooks-create) :type 'user-error)))
        (should (= (length requests) 1))
        (if (equal name "new-route")
            (progn
              (should (string-match-p "REPLACES" question))
              (should (string-match-p "new-route" question))
              (should (string-match-p "fixture (server profile)" question)))
          (should-not question))))))

(ert-deftest hermes-admin-selected-webhook-rejects-source-aliases ()
  "Reject source aliases before consent, even beside their canonical sibling."
  (dolist (name '("Route-one" " route-one" "route-one " "\troute-one\n"
                  "\u001croute-one\u001f" "\vroute-one\f" "" " \t"
                  "\u00a0route-one\u3000" "\u001croute-one\u0085"))
    (dolist (command '(hermes-webhooks-toggle hermes-webhooks-delete))
      (hermes-admin-test--with hermes-webhooks-mode
        (hermes-admin--revert)
        (hermes--promise-resolve
         (nth 3 (car requests))
         `((subscriptions . (((name . ,name) (enabled . t))
                             ((name . "route-one") (enabled . t))))))
        (goto-char (point-min))
        (should (equal (car (tabulated-list-get-id)) name))
        (let ((case-fold-search t) question)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (text) (setq question text) t)))
            (should-error (funcall command) :type 'user-error))
          (should-not question))
        (should (equal (mapcar #'car requests) '("GET")))
        (should (eq hermes-admin--state 'ready))
        (should (equal (caar (car hermes-admin--snapshot)) name))))))

(ert-deftest hermes-admin-selected-webhook-rejects-unsupported-unicode ()
  "Keep Unicode source names visible but reject both mutations before consent."
  (dolist (name '("Kroute" "\u0080route" "café" "route😀" "route\u00a0one"))
    (dolist (command '(hermes-webhooks-toggle hermes-webhooks-delete))
      (hermes-admin-test--with hermes-webhooks-mode
        (hermes-admin--revert)
        (hermes--promise-resolve
         (nth 3 (car requests))
         `((subscriptions . (((name . ,name) (enabled . t))
                             ((name . "kroute") (enabled . t))))))
        (goto-char (point-min))
        (should (equal (car (tabulated-list-get-id)) name))
        (should (string-search name (buffer-string)))
        (let ((case-fold-search t) question)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (text) (setq question text) t)))
            (should (equal (cadr (should-error (funcall command) :type 'user-error))
                           "Unsupported webhook source name: only ASCII names can be changed")))
          (should-not question))
        (should (equal (mapcar #'car requests) '("GET")))
        (should (eq hermes-admin--state 'ready))
        (should (equal (caar (car hermes-admin--snapshot)) name))))))

(ert-deftest hermes-admin-selected-webhook-preserves-canonical-legacy-name ()
  "Exact canonical names retain confirmation identity and encoded request path."
  (dolist (name '("route-one" "kroute" "legacy.route" "legacy route" "_legacy"
                  "legacy?tag%#" "route\u007f"))
    (dolist (command '(hermes-webhooks-toggle hermes-webhooks-delete))
      (hermes-admin-test--with hermes-webhooks-mode
        (hermes-admin--revert)
        (hermes--promise-resolve
         (nth 3 (car requests))
         `((subscriptions . (((name . ,name) (enabled . t))))))
        (goto-char (point-min))
        (let ((toggle (eq command 'hermes-webhooks-toggle)) question)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (text) (setq question text) t)))
            (funcall command))
          (should (equal question
                         (format (if toggle
                                     "Disable webhook %s on fixture (server profile)? "
                                   "Delete webhook %s and its secret on fixture (server profile)? ")
                                 name)))
          (should (equal (mapcar #'car requests)
                         (list (if toggle "PUT" "DELETE") "GET")))
          (should (equal (cadar requests)
                         (concat "/api/webhooks/" (url-hexify-string name)
                                 (if toggle "/enabled" "")))))))))

(provide 'hermes-admin-tests)
;;; hermes-admin-tests.el ends here
