;;; hermes-admin-tests.el --- Administrative browser tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'hermes-test-helpers)
(require 'hermes-admin)
(require 'hermes-inventory)

(ert-deftest hermes-admin-destructive-profile-wire-journeys ()
  "Choose a backend profile, confirm it, and retain it through readback."
  (dolist (kind '(memory webhook))
    (dolist (profile '("default" "second"))
      (with-temp-buffer
        (if (eq kind 'memory) (hermes-memory-status-mode) (hermes-webhooks-mode))
        (hermes-buffer--claim major-mode (and (eq kind 'memory) "*Hermes Memory*"))
        (let* (requests confirmations
               (url "http://scope.example.test")
               (hermes-instances `((:id "scope" :name "Scope" :url ,url)))
               (hermes-dashboard-transport-url url)
               (client (make-hermes-dashboard-transport-client :base-url url))
               (hermes-dashboard-transport-http-request-async-function
                (lambda (target &rest args)
                  (push (cons target args) requests)
                  (hermes--promise-resolved
                   (list :body
                         (cond ((string-suffix-p "/api/profiles" target)
                                '(:profiles ((:name "default") (:name "second"))))
                               ((equal (plist-get args :method) "GET")
                                (if (eq kind 'memory) '(:active "built-in" :builtin_files (:memory 7 :user 5))
                                  '(:subscriptions ((:name "fixture" :enabled t)))))
                               (t '(:ok t :deleted ("MEMORY.md")))))))))
          (setq hermes-instance (hermes-instance-resolve))
          (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire) (lambda (&rest _) client))
                    ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
                    ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                     (lambda (&rest _) (hermes--promise-resolved (list :base-url url))))
                    ((symbol-function 'completing-read) (lambda (&rest _) profile))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (prompt) (push prompt confirmations) t)))
            (if (eq kind 'memory) (hermes-memory-status) (hermes-admin--revert))
            (goto-char (point-min))
            (if (eq kind 'memory) (hermes-memory-reset "memory") (hermes-webhooks-delete)))
          (let ((writes (seq-filter (lambda (r) (not (equal (plist-get (cdr r) :method) "GET"))) requests)))
            (should (= (length writes) 1))
            (should (string-suffix-p (concat "?profile=" profile) (caar writes))))
          (should (= (length requests) 4))
          (should (string-match-p (regexp-quote profile) (car confirmations)))
          (dolist (request requests)
            (unless (string-suffix-p "/api/profiles" (car request))
              (should (string-suffix-p (concat "?profile=" profile) (car request))))))))))

(ert-deftest hermes-admin-profile-ownership-refusals ()
  "Unknown selections and profile replacement never borrow destructive authority."
  (dolist (kind '(memory webhook))
    (dolist (phase '(unknown select consent auth readback))
      (with-temp-buffer
        (if (eq kind 'memory) (hermes-memory-status-mode) (hermes-webhooks-mode))
        (hermes-buffer--claim major-mode (and (eq kind 'memory) "*Hermes Memory*"))
        (let* (requests entered pending
               (url "http://scope.example.test")
               (hermes-instances `((:id "scope" :name "Scope" :url ,url)))
               (hermes-dashboard-transport-url url)
               (client (make-hermes-dashboard-transport-client :base-url url))
               (auth (hermes--promise-make))
               (hermes-dashboard-transport-http-request-async-function
                (lambda (target &rest args)
                  (push (cons target args) requests)
                  (unless (equal (plist-get args :method) "GET")
                    (when (eq phase 'readback) (setq pending t)))
                  (hermes--promise-resolved
                   (list :body
                         (cond ((string-suffix-p "/api/profiles" target)
                                '(:profiles ((:name "default") (:name "second"))))
                               ((equal (plist-get args :method) "GET")
                                (if (eq kind 'memory) '(:active "built-in")
                                  '(:subscriptions ((:name "fixture" :enabled t)))))
                               (t '(:ok t :deleted ("MEMORY.md")))))))))
          (setq hermes-instance (hermes-instance-resolve))
          (cl-labels ((retire ()
                        (setq entered t)
                        (if (eq kind 'memory) (setq hermes-memory--profile "other")
                          (setq hermes-admin--profile "other"))))
            (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire) (lambda (&rest _) client))
                      ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
                      ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                       (lambda (&rest _) (if pending auth (hermes--promise-resolved (list :base-url url)))))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (setq entered t)
                         (when (eq phase 'select) (retire))
                         (if (eq phase 'unknown) "missing" "second")))
                      ((symbol-function 'yes-or-no-p)
                       (lambda (_prompt) (when (eq phase 'consent) (retire)) t)))
              (if (eq kind 'memory) (hermes-memory-status) (hermes-admin--revert))
              (goto-char (point-min))
              (setq pending (eq phase 'auth))
              (condition-case nil
                  (if (eq kind 'memory) (hermes-memory-reset "all") (hermes-webhooks-delete))
                (user-error nil))
              (when (memq phase '(auth readback))
                (retire)
                (hermes--promise-resolve auth (list :base-url url)))))
          (should entered)
          (should (= (length requests)
                     (pcase phase ((or 'unknown 'select) 1) ('readback 3) (_ 2))))
          (let ((writes (seq-filter (lambda (r) (not (equal (plist-get (cdr r) :method) "GET"))) requests)))
            (should (= (length writes) (if (eq phase 'readback) 1 0)))))))))

(ert-deftest hermes-memory-profile-read-settles-only-its-origin ()
  "Delayed initial status installs scope in its owner, never a replacement."
  (dolist (retirement '(nil profile claim))
    (with-temp-buffer
      (hermes-memory-status-mode)
      (hermes-buffer--claim major-mode "*Hermes Memory*")
      (let* ((origin (current-buffer)) (response (hermes--promise-make))
             (url "http://scope.example.test")
             (hermes-instances `((:id "scope" :name "Scope" :url ,url)))
             (hermes-dashboard-transport-url url)
             (client (make-hermes-dashboard-transport-client :base-url url :token "fixture"))
             (hermes-dashboard-transport-http-request-async-function
              (lambda (target &rest _)
                (if (string-suffix-p "/api/profiles" target)
                    (hermes--promise-resolved '(:body (:profiles ((:name "second")))))
                  response))))
        (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire) (lambda (&rest _) client))
                  ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
                  ((symbol-function 'completing-read) (lambda (&rest _) "second")))
          (hermes-memory-status)
          (pcase retirement
            ('profile (setq hermes-memory--profile "other"))
            ('claim (hermes-buffer--claim major-mode "*Hermes Memory*")))
          (with-temp-buffer
            (insert "unrelated draft")
            (hermes--promise-resolve response '(:body (:active "built-in" :builtin_files (:memory 9))))
            (should (equal (buffer-string) "unrelated draft"))
            (should-not hermes-memory--profile))
          (with-current-buffer origin
            (if retirement
                (should-not (string-match-p "Active provider" (buffer-string)))
              (should (equal hermes-memory--profile "second"))
              (should (string-match-p "Profile: second" (buffer-string))))))))))

(defun hermes-admin-test--native-result (kind &optional old)
  "Return a synthetic KIND read body, with OLD identities when non-nil."
  (pcase kind
    ('memory `(:active "built-in" :builtin_files (:memory ,(if old 3 9))))
    ('pairing `(:pending nil :approved ((:platform "xmpp" :user_id ,(if old "old-user" "user-a")))))
    (_ `(:subscriptions ((:name ,(if old "old-route" "route-a") :enabled t))))))

(defun hermes-admin-test--native-publication (kind phase transition &optional fault)
  "Exercise KIND publication with PHASE hook, TRANSITION and optional FAULT."
  (with-temp-buffer
    (let* ((memory (eq kind 'memory))
           (pairing (eq kind 'pairing))
           (mode (pcase kind ('memory #'hermes-memory-status-mode)
                         ('pairing #'hermes-pairing-mode) (_ #'hermes-webhooks-mode)))
           (a '(:id "a" :name "A" :url "http://a.invalid"))
           (b '(:id "b" :name "B" :url "http://b.invalid"))
           (hermes-instances (list a b))
           (response (hermes--promise-make))
           requests confirmations fired successor accepted)
      (funcall mode)
      (hermes-buffer--claim major-mode (and memory "*Hermes Memory*"))
      (hermes-browser--own-instance a)
      (let ((inhibit-read-only t)) (insert "accepted text\n"))
      (let ((hermes-dashboard-transport-http-request-async-function
             (lambda (url &rest args)
               (push (cons url args) requests)
               (cond ((string-suffix-p "/api/profiles" url)
                      (hermes--promise-resolved '(:body (:profiles ((:name "worker"))))))
                     ((equal (plist-get args :method) "GET") response)
                     (t (hermes--promise-resolved '(:body (:ok t :deleted ("MEMORY.md")))))))))
        (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                   (lambda (&rest _)
                     (make-hermes-dashboard-transport-client
                      :base-url (hermes-instance-url hermes-instance) :token "fixture")))
                  ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
                  ((symbol-function 'completing-read) (lambda (&rest _) "worker"))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (prompt) (push prompt confirmations) t)))
          (when (and fault (not transition))
            (if memory (hermes-memory-status) (hermes-admin--revert))
            (hermes--promise-resolve response (list :body (hermes-admin-test--native-result kind t)))
            (setq accepted (list (buffer-string) tabulated-list-entries
                                 hermes-admin--snapshot hermes-memory--status
                                 hermes-admin--profile hermes-memory--profile)
                  response (hermes--promise-make)))
          (if memory (hermes-memory-status) (hermes-admin--revert))
          (when phase
            (add-hook
             phase
             (lambda (&rest _)
               (unless fired
                 (setq fired t)
                 (when transition
                   (pcase transition
                     ('backend (hermes-browser--own-instance b))
                     ('profile (if memory (setq hermes-memory--profile "other")
                                 (setq hermes-admin--profile "other")))
                     ('claim (hermes-buffer--claim major-mode))
                     ('mode (text-mode)))
                   (let ((inhibit-read-only t) (inhibit-modification-hooks t))
                     (erase-buffer) (insert "successor draft\n"))
                   (setq tabulated-list-entries nil hermes-admin--snapshot nil
                         hermes-admin--snapshot-owner nil hermes-admin--state 'unknown
                         hermes-memory--status nil hermes-memory--status-owner nil
                         hermes-browser--status "Successor")
                   (setq successor
                         (list hermes-instance hermes-admin--profile hermes-memory--profile
                               hermes-buffer--owner major-mode)))
                 (when fault (signal fault '("Native publication fault"))))) nil t))
          (condition-case nil
              (hermes--promise-resolve
               response (list :body (hermes-admin-test--native-result kind)))
            (quit nil))
          (when phase (should fired))
          (cond
           (transition
            (should (equal (buffer-string) "successor draft\n"))
            (should (equal successor
                           (list hermes-instance hermes-admin--profile hermes-memory--profile
                                 hermes-buffer--owner major-mode)))
            (should-not tabulated-list-entries)
            (should-not hermes-admin--snapshot)
            (should-not hermes-memory--status)
            (should (eq hermes-admin--state 'unknown))
            (should (equal hermes-browser--status "Successor")))
           (fault
            (should (equal (buffer-string) (car accepted)))
            (should (eq tabulated-list-entries (nth 1 accepted)))
            (should (eq hermes-admin--snapshot (nth 2 accepted)))
            (should (eq hermes-memory--status (nth 3 accepted)))
            (should (equal hermes-admin--profile (nth 4 accepted)))
            (should (equal hermes-memory--profile (nth 5 accepted))))
           (t
            (should (string-match-p (pcase kind ('memory "Active provider")
                                          ('pairing "user-a") (_ "route-a"))
                                    (buffer-string)))))
          (goto-char (point-min))
          (condition-case nil
              (pcase kind ('memory (hermes-memory-reset "memory"))
                     ('pairing (hermes-pairing-revoke)) (_ (hermes-webhooks-delete)))
            (user-error nil))
          (let ((writes (seq-filter
                         (lambda (request) (not (equal (plist-get (cdr request) :method) "GET")))
                         requests)))
            (if (or transition fault)
                (progn (should-not confirmations) (should-not writes))
              (should (= (length writes) 1))
              (should (string-prefix-p "http://a.invalid/" (caar writes)))
              (unless pairing (should (string-suffix-p "?profile=worker" (caar writes)))))))))))

(ert-deftest hermes-admin-native-publication-retains-owner ()
  "Both native hook phases preserve successors and refuse obsolete row actions."
  (dolist (kind '(webhook pairing memory))
    (dolist (phase '(before-change-functions after-change-functions))
      (dolist (transition '(backend profile claim mode))
        (dolist (fault '(nil error quit))
          (ert-info ((format "%s %s %s %s" kind phase transition fault))
            (hermes-admin-test--native-publication kind phase transition fault)))))))

(ert-deftest hermes-admin-native-publication-transaction-controls ()
  "Unchanged owners accept snapshots; native errors and quits keep accepted text."
  (dolist (kind '(webhook pairing memory))
    (dolist (phase '(before-change-functions after-change-functions))
      (dolist (fault '(nil error quit))
        (ert-info ((format "%s %s %s" kind phase fault))
          (hermes-admin-test--native-publication kind phase nil fault))))))

(ert-deftest hermes-admin-native-publication-reentrant-successor ()
  "A hook's accepted replacement remains actionable only for its own rows."
  (dolist (phase '(before-change-functions after-change-functions))
    (dolist (fault '(nil error quit))
      (with-temp-buffer
        (hermes-webhooks-mode)
        (hermes-buffer--claim major-mode)
        (let* ((a '(:id "a" :name "A" :url "http://a.invalid"))
               (b '(:id "b" :name "B" :url "http://b.invalid"))
               (hermes-instances (list a b))
               (response (hermes--promise-make))
               (pending response) requests fired accepted
               (hermes-dashboard-transport-http-request-async-function
                (lambda (url &rest args)
                  (push (cons url args) requests)
                  (cond ((string-suffix-p "/api/profiles" url)
                         (hermes--promise-resolved '(:body (:profiles ((:name "worker"))))))
                        ((equal (plist-get args :method) "GET") response)
                        (t (hermes--promise-resolved '(:body (:ok t))))))))
          (hermes-browser--own-instance a)
          (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                     (lambda (&rest _)
                       (make-hermes-dashboard-transport-client
                        :base-url (hermes-instance-url hermes-instance) :token "fixture")))
                    ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
                    ((symbol-function 'completing-read) (lambda (&rest _) "worker"))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (hermes-admin--revert)
            (add-hook
             phase
             (lambda (&rest _)
               (unless fired
                 (setq fired t)
                 (hermes-browser--own-instance b)
                 (setq response (hermes--promise-resolved
                                 '(:body (:subscriptions ((:name "route-b" :enabled t))))))
                 (hermes-admin--revert)
                 (setq accepted (list (buffer-string) tabulated-list-entries
                                      hermes-admin--snapshot-owner))
                 (when fault (signal fault '("Native hook fault"))))) nil t)
            (condition-case nil
                (hermes--promise-resolve pending
                                        '(:body (:subscriptions ((:name "route-a" :enabled t)))))
              (quit nil))
            (should fired)
            (should (equal (buffer-string) (car accepted)))
            (should (eq tabulated-list-entries (cadr accepted)))
            (should (eq hermes-admin--snapshot-owner (nth 2 accepted)))
            (should (eq hermes-admin--state 'ready))
            (goto-char (point-min))
            (hermes-webhooks-delete)
            (should (equal
                     (mapcar #'car (seq-filter
                                    (lambda (r) (equal (plist-get (cdr r) :method) "DELETE"))
                                    requests))
                     '("http://b.invalid/api/webhooks/route-b?profile=worker")))))))))

(ert-deftest hermes-admin-native-sort-retains-successor ()
  "Native sorting and resizing cannot overwrite an accepted successor."
  (dolist (phase '(before-change-functions after-change-functions))
    (dolist (sort '(tabulated-list-sort tabulated-list--sort-by-column-name
                   tabulated-list-widen-current-column))
      (with-temp-buffer
        (hermes-webhooks-mode)
        (hermes-buffer--claim major-mode)
        (let* ((a '(:id "a" :name "A" :url "http://a.invalid"))
               (b '(:id "b" :name "B" :url "http://b.invalid"))
               (hermes-instances (list a b)) requests fired accepted
               (hermes-dashboard-transport-http-request-async-function
                (lambda (url &rest args)
                  (push (cons url args) requests)
                  (hermes--promise-resolved
                   (list :body
                         (cond ((string-suffix-p "/api/profiles" url)
                                '(:profiles ((:name "worker"))))
                               ((equal (plist-get args :method) "GET")
                                `(:subscriptions ((:name ,(if (string-prefix-p "http://a.invalid/" url)
                                                             "route-a" "route-b") :enabled t))))
                               (t '(:ok t))))))))
          (hermes-browser--own-instance a)
          (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                     (lambda (&rest _)
                       (make-hermes-dashboard-transport-client
                        :base-url (hermes-instance-url hermes-instance) :token "fixture")))
                    ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
                    ((symbol-function 'completing-read) (lambda (&rest _) "worker"))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (hermes-admin--revert)
            (add-hook phase
                      (lambda (&rest _)
                        (unless fired
                          (setq fired t)
                          (hermes-browser--own-instance b)
                          (hermes-admin--revert)
                          ;; A successor may finish its own native presentation.
                          (goto-char (point-min))
                          (tabulated-list-widen-current-column 1)
                          (setq-local header-line-format "Accepted successor header")
                          (setq accepted (list (buffer-string) tabulated-list-entries
                                               hermes-admin--snapshot-owner)))) nil t)
            (goto-char (point-min))
            (funcall sort (pcase sort
                            ('tabulated-list-sort 0)
                            ('tabulated-list-widen-current-column 1)
                            (_ "Name")))
            (should fired)
            (should (equal header-line-format "Accepted successor header"))
            (should (equal-including-properties (buffer-string) (car accepted)))
            (should (eq tabulated-list-entries (cadr accepted)))
            (should (eq hermes-admin--snapshot-owner (nth 2 accepted)))
            (should (eq hermes-admin--state 'ready))
            (goto-char (point-min))
            (hermes-webhooks-delete)
            (should (equal
                     (mapcar #'car (seq-filter
                                    (lambda (r) (equal (plist-get (cdr r) :method) "DELETE"))
                                    requests))
                     '("http://b.invalid/api/webhooks/route-b?profile=worker")))))))))

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
     (setq hermes-admin--profile "fixture")
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

(ert-deftest hermes-admin-native-publication-printer-rollback-and-order ()
  "Keep native sorting and row position; partial printer faults retain the list."
  (dolist (fault '(error quit))
    (hermes-admin-test--with hermes-webhooks-mode
      (setq tabulated-list-sort-key '("Name" . nil))
      (let ((result '(:subscriptions ((:name "z-route" :enabled t)
                                     (:name "a-route" :enabled t)))))
        (hermes-admin--revert)
        (hermes--promise-resolve (nth 3 (car requests)) result)
        (should (equal (mapcar #'caar tabulated-list-entries) '("a-route" "z-route")))
        (goto-char (point-min)) (forward-line 1) (move-to-column 3)
        (let ((text (buffer-string)) (entries tabulated-list-entries)
              (printer tabulated-list-printer) entered)
          (setq tabulated-list-printer
                (lambda (id columns)
                  (funcall printer id columns)
                  (setq entered t)
                  (signal fault '("Partial native printer"))))
          (hermes-admin--revert)
          (condition-case nil
              (hermes--promise-resolve (nth 3 (car requests)) result)
            (quit nil))
          (should entered)
          (should (equal-including-properties (buffer-string) text))
          (should (eq tabulated-list-entries entries))
          (should (eq hermes-admin--snapshot entries))
          (should-error (hermes-webhooks-delete) :type 'user-error)
          (setq tabulated-list-printer printer)
          (hermes-admin--revert)
          (hermes--promise-resolve (nth 3 (car requests)) result)
          (should (eq hermes-admin--state 'ready))
          (should (equal (car (tabulated-list-get-id)) "z-route"))
          (should (= (current-column) 3))
          (should-not (buffer-modified-p)))))))

(ert-deftest hermes-admin-native-resize-preserves-foreign-header ()
  "Both native hook phases retain a foreign editor's draft, mode and header."
  (dolist (phase '(before-change-functions after-change-functions))
    (hermes-admin-test--with hermes-webhooks-mode
      (hermes-admin--revert)
      (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--webhooks)
      (let (fired)
        (add-hook phase
                  (lambda (&rest _)
                    (unless fired
                      (setq fired t)
                      (text-mode)
                      (setq-local header-line-format "Successor editor header")
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert "Successor editor draft\n")))) nil t)
        (goto-char (point-min))
        (tabulated-list-widen-current-column 1)
        (should fired)
        (should (eq major-mode 'text-mode))
        (should (equal (buffer-string) "Successor editor draft\n"))
        (should (equal header-line-format "Successor editor header"))))))

(ert-deftest hermes-admin-native-resize-controls ()
  "Keep ordinary native widening, narrowing and same-owner error/quit rollback."
  (dolist (phase '(before-change-functions after-change-functions))
    (dolist (fault '(nil error quit))
      (hermes-admin-test--with hermes-webhooks-mode
        (hermes-admin--revert)
        (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--webhooks)
        (goto-char (point-min))
        (let* ((width (cadr (aref tabulated-list-format 0)))
               (text (buffer-string))
               (header header-line-format)
               (snapshot hermes-admin--snapshot)
               fired)
          (add-hook phase (lambda (&rest _)
                            (setq fired t)
                            (when fault (signal fault '("Resize fault")))) nil t)
          (condition-case err
              (progn (tabulated-list-widen-current-column 1)
                     (should-not fault))
            ((error quit) (should (eq (car err) fault))))
          (should fired)
          (if fault
              (progn
                (should (equal-including-properties text (buffer-string)))
                (should (eq header header-line-format))
                (should (eq snapshot hermes-admin--snapshot))
                (should (eq hermes-admin--state 'failed))
                (should-error (hermes-webhooks-delete) :type 'user-error))
            (should (= (cadr (aref tabulated-list-format 0)) (1+ width)))
            (should-not (eq header header-line-format))
            (should (eq hermes-admin--state 'ready))
            (should (hermes-admin--current-p hermes-admin--snapshot-owner))
            (tabulated-list-narrow-current-column 1)
            (should (= (cadr (aref tabulated-list-format 0)) width))
            (should (equal-including-properties text (buffer-string)))))))))

(ert-deftest hermes-admin-native-sort-controls ()
  "Preserve native reverse sorting and roll back same-owner hook failures."
  (dolist (phase '(before-change-functions after-change-functions))
    (dolist (fault '(error quit))
      (hermes-admin-test--with hermes-webhooks-mode
        (let ((result '(:subscriptions ((:name "z-route" :enabled t)
                                       (:name "a-route" :enabled t)))))
          (hermes-admin--revert)
          (hermes--promise-resolve (nth 3 (car requests)) result)
          (goto-char (point-min))
          (tabulated-list-sort 0)
          (should (equal (mapcar #'caar tabulated-list-entries) '("a-route" "z-route")))
          (tabulated-list--sort-by-column-name "Name")
          (should (equal (mapcar #'caar tabulated-list-entries) '("z-route" "a-route")))
          (let* ((entries tabulated-list-entries) (text (buffer-string))
                 fired
                 (hook (lambda (&rest _) (setq fired t) (signal fault '("Sort fault")))))
            (add-hook phase hook nil t)
            (condition-case err
                (progn (tabulated-list-sort 0) (ert-fail "Sort did not signal"))
              ((error quit) (should (eq (car err) fault))))
            (should fired)
            (should (equal-including-properties text (buffer-string)))
            (should (eq entries tabulated-list-entries))
            (should (eq entries hermes-admin--snapshot))
            (should (eq hermes-admin--state 'failed))
            (should-error (hermes-webhooks-delete) :type 'user-error)
            (remove-hook phase hook t)
            (hermes-admin--revert)
            (hermes--promise-resolve (nth 3 (car requests)) result)
            (should (eq hermes-admin--state 'ready))))))))

(ert-deftest hermes-admin-native-sort-preserves-fake-header ()
  "Retain native fake-header text, properties and overlay across sorting."
  (let ((tabulated-list-use-header-line nil))
    (hermes-admin-test--with hermes-webhooks-mode
      (hermes-admin--revert)
      (hermes--promise-resolve (nth 3 (car requests)) hermes-admin-test--webhooks)
      (should (string-prefix-p " " (buffer-string)))
      (should (string-match-p "Name" (buffer-substring (point-min) (line-end-position))))
      (should (tabulated-list-header-overlay-p))
      (let ((map (get-text-property (1+ (point-min)) 'keymap)))
        (should (keymapp map)))
      (tabulated-list--sort-by-column-name "Name")
      (should (tabulated-list-header-overlay-p))
      (forward-line 1)
      (should (equal (car (tabulated-list-get-id)) "route-one")))))

(ert-deftest hermes-admin-native-sort-leaves-other-modes-alone ()
  "The printer adapter leaves unrelated native lists and hooks unchanged."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)]
          tabulated-list-entries '(("z" ["z"]) ("a" ["a"])))
    (let (entered)
      (add-hook 'after-change-functions (lambda (&rest _) (setq entered t)) nil t)
      (tabulated-list-print)
      (tabulated-list-sort 0)
      (should entered)
      (should (equal (mapcar #'car tabulated-list-entries) '("a" "z")))
      (should (string-match-p "a" (buffer-string)))
      (goto-char (point-min))
      (tabulated-list-widen-current-column 1)
      (should (= (cadr (aref tabulated-list-format 0)) 21))
      (should header-line-format)
      (tabulated-list-narrow-current-column 1)
      (should (= (cadr (aref tabulated-list-format 0)) 20)))))

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
      (should (hermes-admin-test--bounded-groups-p metadata)))))

(defun hermes-admin-test--bounded-groups-p (rows)
  "Return non-nil when each popup group in ROWS has at most four entries."
  (cl-every (lambda (row)
              (cl-every (lambda (group)
                          (<= (length (plist-get group :entries)) 4))
                        row))
            rows))

(ert-deftest hermes-admin-popup-bounds-reject-oversized-group ()
  "The bounds check measures entries, not the enclosing rows."
  (should-not (hermes-admin-test--bounded-groups-p
               (list (list (list :name "Oversized" :entries '(a b c d e)))))))

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
              (should (string-match-p "fixture (profile fixture)" question)))
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
                                     "Disable webhook %s on fixture (profile fixture)? "
                                   "Delete webhook %s and its secret on fixture (profile fixture)? ")
                                 name)))
          (should (equal (mapcar #'car requests)
                         (list (if toggle "PUT" "DELETE") "GET")))
          (should (equal (cadar requests)
                         (concat "/api/webhooks/" (url-hexify-string name)
                                 (if toggle "/enabled" "")))))))))

(provide 'hermes-admin-tests)
;;; hermes-admin-tests.el ends here
