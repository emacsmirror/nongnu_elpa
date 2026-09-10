;;; hermes-projects-tests.el --- Workspace and pin journeys -*- lexical-binding: t; -*-

;;; Code:
(require 'hermes-test-helpers)
(require 'hermes-projects)

(defconst hermes-projects-test--project
  '((id . "same") (name . "Notebook") (primary_path . "/backend/one")
    (folders . (((path . "/backend/one") (is_primary . t))
                ((path . "/backend/two") (is_primary . nil))))))

(defmacro hermes-projects-test--client (&rest body)
  "Run BODY with a real transport client and controlled acquisition."
  (declare (indent 0) (debug t))
  `(let ((client (make-hermes-dashboard-transport-client)) (released 0)
         (api-request (symbol-function 'hermes-dashboard-transport-api-request-async)))
     (cl-letf (((symbol-function 'hermes-browser--with-client)
                (lambda (fn) (funcall fn client (lambda () (cl-incf released)))))
               ((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (method path &rest args)
                  (if (equal path "/api/profiles")
                      (hermes--promise-resolved
                       '((profiles . (((name . "default")) ((name . "work")) ((name . "play"))))))
                    (apply api-request method path args)))))
       ,@body)))

(defmacro hermes-projects-test--view (&rest body)
  "Run BODY in a fresh project browser with a backend snapshot."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (hermes-projects-mode)
     (setq hermes-projects--profile "work")
     (hermes-projects--accept
      (list (cons 'projects (list (copy-tree hermes-projects-test--project)))
            '(active_id . "same")))
     (goto-char (point-min))
     ,@body))

(defmacro hermes-projects-test--sessions (&rest body)
  "Run BODY with colliding stored IDs owned by different profiles."
  (declare (indent 0) (debug t))
  `(let ((session-buffer (generate-new-buffer " *pin session*")))
     (unwind-protect
         (with-current-buffer session-buffer
	   (hermes-sessions-mode)
	   (hermes-sessions--render
	    '((sessions . (((id . "s/a") (profile . "work") (title . "One") (cwd . "/keep"))
			   ((id . "s/a") (profile . "play") (title . "Two") (pinned . t))))))
	   (goto-char (point-min))
	   ,@body)
       (when (buffer-live-p session-buffer) (kill-buffer session-buffer)))))

(ert-deftest hermes-projects-pin-public-rest-readback-profile-isolation ()
  (hermes-projects-test--client
   (let ((patch (hermes--promise-make)) (read (hermes--promise-make)) calls)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (method path &rest args)
                  (push (list method path args) calls)
                  (if (equal method "PATCH") patch read))))
       (hermes-projects-test--sessions
        (call-interactively #'hermes-sessions-pin)
        (should (equal (caar calls) "PATCH"))
        (should (equal (cadar calls) "/api/sessions/s%2Fa"))
        (should (equal (plist-get (caddar calls) :body)
                       '((profile . "work") (pinned . t))))
        (should (eq (hermes-sessions--pin-state (hermes-sessions--selected-session)) 'unknown))
        (hermes--promise-resolve patch '((pinned . t)))
        (should (equal (caar calls) "GET"))
        (should (equal (plist-get (caddar calls) :query) '((profile . "work"))))
        (hermes--promise-resolve read '((id . "s/a") (pinned . 1)))
        (should (eq (hermes-sessions--pin-state (hermes-sessions--selected-session)) 'pinned))
        (should (equal (hermes-transport--get (hermes-sessions--selected-session) 'cwd) "/keep"))
        (should (eq (hermes-sessions--pin-state
                     (gethash '("play" . "s/a") hermes-sessions--session-map)) 'pinned))
        (should (= released 1))
        (should-not hermes-browser--owned-cleanup))))))

(ert-deftest hermes-projects-pin-failed-readback-stays-unknown ()
  (hermes-projects-test--client
   (let (calls)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (method _path &rest _args)
                  (push method calls)
                  (hermes--promise-rejected "Connection lost"))))
       (hermes-projects-test--sessions
        (hermes-sessions-unpin)
        (should (equal (reverse calls) '("PATCH" "GET")))
        (should (eq (hermes-sessions--pin-state (hermes-sessions--selected-session)) 'unknown))
        (should (string-match-p "Failed" hermes-browser--status)))))))

(ert-deftest hermes-projects-pin-unknown-toggle-only-reads ()
  (hermes-projects-test--client
   (let (calls)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (method _path &rest _args)
                  (push method calls)
                  (hermes--promise-resolved '((id . "s/a") (pinned . 0))))))
       (hermes-projects-test--sessions
        (call-interactively (key-binding (kbd "k")))
        (should (equal calls '("GET")))
        (should (eq (hermes-sessions--pin-state (hermes-sessions--selected-session)) 'unpinned)))))))

(ert-deftest hermes-projects-pin-auth-wait-rechecks-owner ()
  (hermes-projects-test--client
   (let ((auth (hermes--promise-make)) sent)
     (setf (hermes-dashboard-transport-client-base-url client) "http://example.invalid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-auth-async) (lambda () auth))
               ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                (lambda (_) (setq sent t) (hermes--promise-resolved nil))))
       (hermes-projects-test--sessions
        (hermes-sessions-pin)
        (hermes-browser--next-request-generation)
        (hermes--promise-resolve auth '(:session-token "test"))
        (should-not sent)
        (should (= released 1)))))))

(ert-deftest hermes-projects-pin-kill-and-generation-retire ()
  (dolist (retire '(kill refresh stop))
    (hermes-projects-test--client
     (let ((read (hermes--promise-make)))
       (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                  (lambda (&rest _) read)))
         (hermes-projects-test--sessions
          (hermes-sessions-pin)
          (pcase retire
            ('kill (kill-buffer (current-buffer)))
            ('refresh (hermes-browser--next-request-generation))
            ('stop (hermes-dashboard-transport-stop client)))
          (ert-info ((format "Retirement kind: %s" retire))
		    (should (= released 1)))
          (hermes--promise-resolve read '((id . "s/a") (pinned . 1)))
          (should (= released 1))))))))

(ert-deftest hermes-projects-pin-detail-and-stale-selection ()
  (hermes-projects-test--client
   (let ((read (hermes--promise-make)))
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _) read)))
       (hermes-projects-test--sessions
        (hermes-sessions-toggle-pin)
        (forward-line 1)
        (hermes--promise-resolve read '((id . "s/a") (pinned . 0)))
        (should (eq (hermes-sessions--pin-state (hermes-sessions--selected-session)) 'pinned)))
       (with-temp-buffer
         (hermes-sessions--render-detail-contents
          '((id . "s/a") (profile . "work")) nil nil)
         (hermes-sessions-toggle-pin)
         (should (string-match-p "Pin: unpinned" (buffer-string))))))))

(ert-deftest hermes-projects-catalogue-pinned-extras-do-not-skip-windows ()
  (hermes-projects-test--client
   (let (offsets)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (_method _path &rest args)
                  (let* ((query (plist-get args :query)) (offset (alist-get 'offset query))
                         (rows (cl-loop for i from offset below (+ offset 100)
                                        collect `((id . ,(number-to-string i)) (profile . "work")))))
                    (push offset offsets)
                    (hermes--promise-resolved
                     `((total . 220) (sessions . ,(append rows
							  '(((id . "219") (profile . "work") (pinned . t))
							    ((id . "219") (profile . "play") (pinned . t)))))))))))
       (with-temp-buffer
         (hermes-sessions-mode)
         (setq hermes-sessions--catalogue-profile "work")
         (hermes-sessions--revert)
         (should (= (length tabulated-list-entries) 102))
         (hermes-sessions-next-page)
         (should (equal offsets '(100 0)))
         (should (= (length tabulated-list-entries) 202))
         (should (= (hash-table-count hermes-sessions--session-map) 202)))))))

(ert-deftest hermes-projects-public-list-detail-and-typed-profile ()
  (save-window-excursion
    (hermes-projects-test--client
     (let (calls buffers)
       (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                  (lambda (_client method params resolve _reject)
                    (push (cons method params) calls)
                    (funcall resolve
                             (if (equal method "projects.get")
                                 `((project . ,hermes-projects-test--project))
                               `((projects . (,hermes-projects-test--project)) (active_id . "same")))))))
         (unwind-protect
             (progn
               (hermes-list-projects)
               (push (current-buffer) buffers)
               (should (equal (caar calls) "projects.list"))
               (should-not (alist-get 'profile (cdar calls)))
               (call-interactively (key-binding (kbd "g")))
               (should (equal hermes-browser--status "Ready"))
               (goto-char (point-min))
               (hermes-projects-view)
               (push (current-buffer) buffers)
               (should (derived-mode-p 'hermes-project-detail-mode))
               (should (equal (caar calls) "projects.get"))
               (should (equal (alist-get 'id (cdar calls)) "same"))
               (should (string-match-p "/backend/two" (buffer-string)))
               (should (string-match-p "Active: Notebook" (buffer-string))))
           (mapc #'kill-buffer buffers)))))))

(ert-deftest hermes-projects-public-mutations-use-exact-backend-methods ()
  (hermes-projects-test--client
   (let (calls (inputs '("Renamed" "/other")))
     (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                (lambda (_client method params resolve _reject)
                  (push (cons method params) calls)
                  (funcall resolve `((projects . (,hermes-projects-test--project))))))
               ((symbol-function 'read-string) (lambda (&rest _) (or (pop inputs) "/backend/one")))
               ((symbol-function 'yes-or-no-p) (lambda (_) t))
               ((symbol-function 'read-directory-name) (lambda (&rest _) (ert-fail "Local directory completion"))))
       (hermes-projects-test--view
        (dolist (pair '((hermes-projects-rename . "update")
                        (hermes-projects-add-folder . "add_folder")
                        (hermes-projects-remove-folder . "remove_folder")
                        (hermes-projects-set-primary . "set_primary")
                        (hermes-projects-archive . "archive")
                        (hermes-projects-restore . "archive")
                        (hermes-projects-set-active . "set_active")
                        (hermes-projects-delete . "delete")))
          (setq calls nil)
          (call-interactively (car pair))
          (should (equal (mapcar #'car (reverse calls))
                         (list (concat "projects." (cdr pair)) "projects.list")))
          (should (equal (alist-get 'id (cdar (last calls))) "same"))
          (should (equal (alist-get 'profile (cdar (last calls))) "work")))
        (setq calls nil inputs '("New" "/remote/new"))
        (hermes-projects-create)
        (let ((params (cdar (last calls))))
          (should (equal (alist-get 'name params) "New"))
          (should (equal (alist-get 'folders params) ["/remote/new"]))
          (should (eq (alist-get 'use params) :false)))
        (setq calls nil)
        (hermes-projects-clear-active)
        (should-not (alist-get 'id (cdar (last calls)))))))))

(ert-deftest hermes-projects-stale-prompt-cannot-cross-profile ()
  (hermes-projects-test--client
   (let (sent)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-request) (lambda (&rest _) (setq sent t)))
               ((symbol-function 'read-string)
                (lambda (&rest _) (setq hermes-projects--profile "play") "Changed")))
       (hermes-projects-test--view
        (hermes-projects-rename)
        (should-not sent))))))

(ert-deftest hermes-projects-auth-queued-rpc-retired-before-dispatch ()
  (hermes-projects-test--client
   (let ((ready (hermes--promise-make)) sent)
     (setf (hermes-dashboard-transport-client-ready-promise client) ready)
     (let ((hermes-dashboard-transport-websocket-send-function
            (lambda (&rest _) (setq sent t))))
       (hermes-projects-test--view
        (hermes-projects-archive)
        (should (= (hash-table-count (hermes-dashboard-transport-client-pending client)) 1))
        (setq hermes-projects--profile "play")
        (hermes--promise-resolve ready t)
        (should-not sent)
        (should (= (hash-table-count (hermes-dashboard-transport-client-pending client)) 0))
        (should (= released 1)))))))

(ert-deftest hermes-projects-uncertain-write-reconciles-without-retry ()
  (hermes-projects-test--client
   (let (calls)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                (lambda (_client method _params resolve reject)
                  (push method calls)
                  (if (equal method "projects.list")
                      (funcall resolve `((projects . (,hermes-projects-test--project))))
                    (funcall reject "timed out")))))
       (hermes-projects-test--view
        (hermes-projects-archive)
        (should (equal (reverse calls) '("projects.archive" "projects.list")))
        (should-not hermes-projects--unsupported)
        (should (string-match-p "Failed" hermes-browser--status)))))))

(ert-deftest hermes-projects-cold-errors-and-missing-method-remain-usable ()
  (save-window-excursion
    (let ((hermes-instance '("test" . "http://127.0.0.1:19391"))
          (hermes-dashboard-transport-start-mode 'spawn)
          (hermes-dashboard-transport-command "/nonexistent-hermes-test")
          (hermes-dashboard-transport--clients (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (hermes-list-projects)
            (should (string-match-p "Failed" hermes-browser--status))
            (should (eq (key-binding (kbd "c")) #'hermes-projects-create))
            (should (eq (key-binding (kbd "P")) #'hermes-projects-profile))
            (hermes-projects-test--client
             (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                        (lambda (_client _method _params _resolve reject)
                          (funcall reject "unknown method: projects.list"))))
               (hermes-projects-refresh)
               (should (member "list" hermes-projects--unsupported))
               (should (string-match-p "Unsupported" hermes-browser--status)))))
        (when (get-buffer "*Hermes Projects*") (kill-buffer "*Hermes Projects*"))))))

(ert-deftest hermes-projects-delete-keeps-directories-sessions-and-active-reference ()
  (hermes-projects-test--client
   (let ((session '((id . "live") (cwd . "/backend/one"))) calls)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
               ((symbol-function 'hermes-dashboard-transport-request)
                (lambda (_client method _params resolve _reject)
                  (push method calls)
                  (funcall resolve '((projects . nil) (active_id . "same"))))))
       (hermes-projects-test--view
        (let ((directory default-directory))
          (hermes-projects-delete)
          (should (equal default-directory directory))
          (should (equal session '((id . "live") (cwd . "/backend/one"))))
          (should (equal calls '("projects.list" "projects.delete")))
          (should (string-match-p "unlisted project" (hermes-projects--description)))))))))

(ert-deftest hermes-projects-title-picker-disambiguates-and-opens-exact-id ()
  (hermes-projects-test--client
   (hermes-projects-test--view
    (let* ((projects '(((id . "one") (name . "Same"))
                       ((id . "two") (name . "Same"))
                       ((id . "three") (name . "Same [two]"))))
           (choices (hermes-projects--choices projects)) opened)
      (should (= (length (delete-dups (mapcar #'car choices))) 3))
      (hermes-projects--accept `((projects . ,projects)))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (car (nth 1 choices))))
                ((symbol-function 'hermes-projects--view-project)
                 (lambda (project) (setq opened (hermes-projects--field project 'id)))))
        (hermes-projects-choose)
        (should (equal opened "two")))))))

(ert-deftest hermes-projects-stale-readback-does-not-repaint-successor ()
  (hermes-projects-test--client
   (let (reply)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                (lambda (_client _method _params resolve _reject) (setq reply resolve))))
       (hermes-projects-test--view
        (hermes-projects-refresh)
        (setq hermes-projects--profile "play")
        (funcall reply '((projects . nil)))
        (should (equal (hermes-projects--field (hermes-projects--selected) 'name) "Notebook"))
        (should (= released 1)))))))

(ert-deftest hermes-projects-bounded-groups-preserve-profile-qualified-session-ids ()
  (let* ((result '((project . ((repos . (((label . "Repo")
					  (groups . (((label . "Main")
						      (sessions . (((id . "same") (title . "A") (profile . "work") (cwd . "/a"))
								   ((id . "same") (title . "B") (profile . "play") (cwd . "/b"))))))))))))))
         (rows (hermes-project-sessions--rows result)) opened)
    (with-temp-buffer
      (hermes-project-sessions-mode)
      (hermes-project-sessions--render result)
      (should (= (length rows) 2))
      (should-not (equal (caar rows) (caadr rows)))
      (should (string-match-p "Bounded subset" hermes-browser--status))
      (goto-char (point-min)) (forward-line 1)
      (cl-letf (((symbol-function 'hermes-chat-resume-session)
                 (lambda (id _title profile _instance) (setq opened (cons profile id)))))
        (hermes-project-sessions-open)
        (should (equal opened '("play" . "same")))))))


(ert-deftest hermes-projects-readback-owns-deferred-rpc-and-cancels-on-kill ()
  (hermes-projects-test--client
   (let ((ready (hermes--promise-make)) mutation)
     (setf (hermes-dashboard-transport-client-ready-promise client)
           (hermes--promise-resolved t))
     (let ((hermes-dashboard-transport-websocket-send-function
            (lambda (_socket frame) (setq mutation (json-parse-string frame :object-type 'alist)))))
       (let ((buffer (generate-new-buffer " *project readback*")))
         (unwind-protect
             (with-current-buffer buffer
               (hermes-projects-mode)
               (hermes-projects--accept `((projects . (,hermes-projects-test--project))))
               (goto-char (point-min))
               (hermes-projects-archive)
               (setf (hermes-dashboard-transport-client-ready-promise client) ready)
               (let* ((id (alist-get 'id mutation))
                      (pending (hermes-dashboard-transport--take-pending client id)))
                 (funcall (plist-get pending :resolve) nil))
               (should (= (hash-table-count (hermes-dashboard-transport-client-pending client)) 1))
               (kill-buffer buffer)
               (should (= (hash-table-count (hermes-dashboard-transport-client-pending client)) 0))
               (hermes--promise-resolve ready t)
               (should (= released 1)))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest hermes-projects-pin-readback-auth-guard-survives-async-write ()
  (hermes-projects-test--client
   (let ((auth (hermes--promise-resolved '(:session-token "test")))
         (write (hermes--promise-make)) (sent 0))
     (setf (hermes-dashboard-transport-client-base-url client) "http://example.invalid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-auth-async) (lambda () auth))
               ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                (lambda (_) (cl-incf sent) write)))
       (hermes-projects-test--sessions
        (hermes-sessions-pin)
        (should (= sent 1))
        (setq auth (hermes--promise-make))
        (hermes--promise-resolve write nil)
        (hermes-browser--next-request-generation)
        (hermes--promise-resolve auth '(:session-token "test"))
        (should (= sent 1))
        (should (= released 1)))))))

(ert-deftest hermes-projects-two-profile-prompts-preserve-exact-owner ()
  (hermes-projects-test--client
   (let ((other (generate-new-buffer " *other profile*")) calls)
     (unwind-protect
         (progn
           (with-current-buffer other
             (hermes-projects-mode)
             (setq hermes-projects--profile "play")
             (hermes-projects--accept `((projects . (,hermes-projects-test--project)))))
           (cl-letf (((symbol-function 'read-string)
                      (lambda (&rest _) (set-buffer other) "Renamed"))
                     ((symbol-function 'hermes-dashboard-transport-request)
                      (lambda (_client method params resolve _reject)
                        (push (cons method params) calls)
                        (funcall resolve `((projects . (,hermes-projects-test--project)))))))
             (hermes-projects-test--view
              (hermes-projects-rename)
              (should (equal (alist-get 'profile (cdar (last calls))) "work"))
              (should (equal (buffer-local-value 'hermes-projects--profile other) "play")))))
       (kill-buffer other)))))

(ert-deftest hermes-projects-pin-rpc-row-public-key-uses-resolved-launch-scope ()
  (dolist (launch '("default" "named-launch"))
    (hermes-projects-test--client
     (let ((pinned 0) calls)
       (setf (hermes-dashboard-transport-client-base-url client) "http://pin.invalid"
             (hermes-dashboard-transport-client-token client) "test")
       (cl-letf (((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                  (lambda (request &rest _)
                    (push request calls)
                    (when (equal (plist-get request :method) "PATCH")
                      (setq pinned (if (eq (alist-get 'pinned (plist-get request :body)) t) 1 0)))
                    (hermes--promise-resolved
                     `(:status 200 :body ((id . "stored") (profile . ,launch) (pinned . ,pinned)))))))
         (with-temp-buffer
           (hermes-sessions-mode)
           ;; Released session.list omits both profile and pinned.
           (hermes-sessions--render
            '((sessions . (((id . "stored") (title . "Example") (preview . "")
                            (started_at . 1) (message_count . 1) (source . "cli"))
                           ((id . "stored") (profile . "other") (pinned . t))))))
           (goto-char (point-min))
           (call-interactively (key-binding (kbd "k")))
           (should (equal (plist-get (car calls) :url) "http://pin.invalid/api/sessions/stored"))
           (should (eq (hermes-sessions--pin-state (hermes-sessions--selected-session)) 'unpinned))
           (should (equal (tabulated-list-get-id) '("" . "stored")))
           (call-interactively (key-binding (kbd "k")))
           (should (eq (hermes-sessions--pin-state (hermes-sessions--selected-session)) 'pinned))
           (call-interactively (key-binding (kbd "k")))
           (should (eq (hermes-sessions--pin-state (hermes-sessions--selected-session)) 'unpinned))
           (should (equal (mapcar (lambda (r) (plist-get r :method)) (reverse calls))
                          '("GET" "PATCH" "GET" "PATCH" "GET")))
           (dolist (request (butlast calls))
             (if (equal (plist-get request :method) "PATCH")
                 (should (equal (alist-get 'profile (plist-get request :body)) launch))
               (should (equal (plist-get request :url)
                              (concat "http://pin.invalid/api/sessions/stored?profile=" launch)))))
           (should (eq (hermes-sessions--pin-state
                        (gethash '("other" . "stored") hermes-sessions--session-map)) 'pinned))
           (should (= (hash-table-count hermes-sessions--session-map) 2))
           (dolist (action '(hermes-sessions-pin hermes-sessions-unpin))
             (setq calls nil)
             (hermes-sessions--render '((sessions . (((id . "stored"))))))
             (goto-char (point-min))
             (call-interactively action)
             (should-not (assq 'profile (plist-get (car (last calls)) :body)))
             (should (equal (mapcar (lambda (r) (plist-get r :method)) (reverse calls))
                            '("PATCH" "GET")))
             (should (eq (hermes-sessions--pin-state (hermes-sessions--selected-session))
                         (if (eq action 'hermes-sessions-pin) 'pinned 'unpinned))))))))))

(ert-deftest hermes-projects-profile-public-validation-fails-closed ()
  (dolist (profile '("typo-profile" " work " "work" "default"))
    (hermes-projects-test--client
     (let (calls (catalogue '((profiles . (((name . "work")) ((name . "play"))
                                          ((name . "default")))))))
       (cl-letf (((symbol-function 'read-string) (lambda (&rest _) profile))
                 ((symbol-function 'hermes-dashboard-transport-api-request-async)
                  (lambda (method path &rest args)
                    (should (equal method "GET"))
                    (should (equal path "/api/profiles"))
                    (should (eq (plist-get args :client) client))
                    (should (funcall (plist-get args :current-p)))
                    (hermes--promise-resolved catalogue)))
                 ((symbol-function 'hermes-dashboard-transport-request)
                  (lambda (_client method params resolve _reject)
                    (push (cons method params) calls)
                    (funcall resolve `((projects . (,hermes-projects-test--project)))))))
         (hermes-projects-test--view
          (call-interactively (key-binding (kbd "P")))
          (should (equal hermes-projects--profile profile))
          (if (not (member profile '("work" "default")))
              (progn
                (should-not calls)
                (should-not hermes-projects--snapshot)
                (should-error (hermes-projects-rename) :type 'user-error)
                ;; Creation needs no selected row but must still refuse this scope.
                (hermes-projects-create)
                (should-not calls))
            (should (equal (alist-get 'profile (cdar calls)) profile))
            (setq catalogue '((profiles . (((name . "play"))))))
            (hermes-projects-rename)
            (should-not (assoc "projects.update" calls))
            (should (string-match-p "Failed" hermes-browser--status)))))))))

(ert-deftest hermes-projects-profile-catalogue-unavailable-keeps-launch-baseline ()
  (hermes-projects-test--client
   (let (calls)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _) (hermes--promise-rejected "Profiles unavailable")))
               ((symbol-function 'hermes-dashboard-transport-request)
                (lambda (_client method params resolve _reject)
                  (push (cons method params) calls)
                  (funcall resolve '((projects . nil))))))
       (with-temp-buffer
         (hermes-projects-mode)
         (hermes-projects-refresh)
         (should (equal (caar calls) "projects.list"))
         (should-not (assq 'profile (cdar calls)))
         (should (string-match-p "launch profile" (hermes-projects--description)))
         (setq calls nil hermes-projects--profile "default")
         (hermes-projects-refresh)
         (should-not calls)
         (should (string-match-p "Failed" hermes-browser--status)))))))

(ert-deftest hermes-projects-catalogue-auth-and-queued-dispatch-retain-owner ()
  (dolist (retire '(auth catalogue queued readback))
    (hermes-projects-test--client
     (let ((auth (hermes--promise-make))
           (catalogue (hermes--promise-make))
           (ready (hermes--promise-make)) sent (reads 0))
       (setf (hermes-dashboard-transport-client-base-url client) "http://catalogue.invalid"
             (hermes-dashboard-transport-client-ready-promise client) ready)
       (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async) api-request)
                 ((symbol-function 'hermes-dashboard-transport-api-auth-async) (lambda () auth))
                 ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                  (lambda (request &rest _)
                    (should (equal (plist-get request :url) "http://catalogue.invalid/api/profiles"))
                    (cl-incf reads)
                    catalogue)))
         (let ((hermes-dashboard-transport-websocket-send-function
                (lambda (_socket frame) (push (json-parse-string frame :object-type 'alist) sent))))
           (hermes-projects-test--view
            (hermes-projects-archive)
            (when (eq retire 'auth) (hermes-browser--next-request-generation))
            (hermes--promise-resolve auth '(:session-token "test" :base-url "http://catalogue.invalid"))
            (when (eq retire 'catalogue) (hermes-browser--next-request-generation))
            (hermes--promise-resolve catalogue
                                    '(:status 200 :body ((profiles . (((name . "work")))))))
            (when (eq retire 'queued)
              (should (= (hash-table-count (hermes-dashboard-transport-client-pending client)) 1))
              (hermes-browser--next-request-generation))
            (hermes--promise-resolve ready t)
            (if (not (eq retire 'readback))
                (should-not sent)
              (should (= (length sent) 1))
              (setq auth (hermes--promise-make))
              (let ((pending (hermes-dashboard-transport--take-pending client (alist-get 'id (car sent)))))
                (funcall (plist-get pending :resolve) nil))
              (hermes-browser--next-request-generation)
              (hermes--promise-resolve auth '(:session-token "test" :base-url "http://catalogue.invalid"))
              (should (= (length sent) 1)))
            (should (= reads (if (eq retire 'auth) 0 1)))
            (should (= (hash-table-count (hermes-dashboard-transport-client-pending client)) 0))
            (should (= released 1)))))))))

(provide 'hermes-projects-tests)
;;; hermes-projects-tests.el ends here
