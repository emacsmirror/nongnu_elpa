;;; hermes-sessions-tests.el --- sessions tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(defun hermes-sessions-test--render (sessions)
  "Render SESSIONS through the browser's `session.list' result shape."
  (hermes-sessions--render `((sessions . ,sessions))))

(ert-deftest hermes-sessions-rows-from-session-list ()
  "Session rows map the `session.list' result fields to columns."
  (let* ((rows (hermes-sessions--rows
                '(((id . "s1") (title . "First") (message_count . 3)
                   (source . "tui") (profile . "work")))))
         (entry (cadr (car rows))))
    (should (equal (caar rows) '("work" . "s1")))
    (should (equal (aref entry 0) "s1"))
    (should (equal (aref entry 1) "First"))
    (should (equal (aref entry 2) "3"))
    (should (equal (aref entry 3) "tui"))
    (should (equal (aref entry 4) "work"))))

(ert-deftest hermes-sessions-rows-format-canonical-title-with-date ()
  "Canonical Emacs titles retain useful date and time context in session rows."
  (let* ((rows (hermes-sessions--rows
                '(((id . "s1")
                   (title . "emacs-hermes--20260807T183045.123456Z--emacs")))))
         (entry (cadr (car rows))))
    (should (equal (aref entry 1) "emacs-hermes · 2026-08-07 18:30"))))

(ert-deftest hermes-session-title-policy-is-strict-and-bounded ()
  "Canonical titles parse strictly, preserve identity, and fit backend limits."
  (let* ((time (encode-time 45 30 18 7 8 2026 t))
         (title (hermes-session-title-canonicalize "emacs-hermes" nil time))
         (renamed (hermes-session-title-canonicalize "frontend" title time))
         (long (hermes-session-title-canonicalize (make-string 120 ?x) nil time)))
    (should (equal title "emacs-hermes--20260807T183045.000000Z--emacs"))
    (should (equal renamed "frontend--20260807T183045.000000Z--emacs"))
    (should (= (length long) 100))
    (should (equal (hermes-session-title-chat-display title) "emacs-hermes"))
    (should (equal (hermes-session-title-chat-display
                    "emacs-hermes--20260807T183045.000000Z--other")
                   "emacs-hermes--20260807T183045.000000Z--other"))))

(ert-deftest hermes-sessions-rows-face-every-column ()
  "Session rows give every column its own face."
  (let* ((row (car (hermes-sessions--rows
                    '(((id . "s1") (title . "First")
                       (message_count . 3) (source . "tui")
                       (profile . "work"))))))
         (entry (cadr row)))
    (should (eq (get-text-property 0 'face (aref entry 0))
                'hermes-browser-identifier))
    (should (eq (get-text-property 0 'face (aref entry 1))
                'hermes-browser-title))
    (should (eq (get-text-property 0 'face (aref entry 2))
                'hermes-browser-message-count))
    (should (eq (get-text-property 0 'face (aref entry 3))
                'hermes-browser-source))
    (should (eq (get-text-property 0 'face (aref entry 4))
                'hermes-browser-profile))))

(ert-deftest hermes-sessions-mode-keymap-keeps-ret-and-adds-actions ()
  "The browser keeps RET resume and exposes native history/actions."
  (should (eq (keymap-lookup hermes-sessions-mode-map "RET")
              #'hermes-sessions-open))
  (should (eq (keymap-lookup hermes-sessions-mode-map "v")
              #'hermes-sessions-view))
  (should (eq (keymap-lookup hermes-sessions-mode-map "r")
              #'hermes-sessions-rename))
  (should (eq (keymap-lookup hermes-sessions-mode-map "d")
              #'hermes-sessions-delete))
  (should (eq (keymap-lookup hermes-sessions-mode-map "a")
              #'hermes-sessions-archive))
  (should (eq (keymap-lookup hermes-sessions-mode-map "s")
              #'hermes-sessions-search))
  (should (eq (keymap-lookup hermes-sessions-mode-map "w")
              #'hermes-sessions-export)))

(ert-deftest hermes-sessions-fetches-search-and-all-profile-rest-routes ()
  "Search and aggregate modes use their documented dashboard REST routes."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-sessions--rest)
               (lambda (_client method path &optional _body query)
                 (push (list method path query) calls)
                 (hermes--promise-resolved '((sessions . nil))))))
      (with-temp-buffer
        (setq hermes-sessions--search-query "needle"
              hermes-sessions--search-profile "work")
        (hermes-sessions--fetch 'client)
        (setq hermes-sessions--search-query nil
              hermes-sessions--all-profiles t)
        (hermes-sessions--fetch 'client)))
    (should (member '("GET" "/api/sessions/search"
                      ((q . "needle") (limit . 100) (profile . "work"))) calls))
    (should (member '("GET" "/api/profiles/sessions"
                      ((profile . "all") (limit . 100)
                       (archived . "exclude")))
                    calls))))

(ert-deftest hermes-sessions-search-rejects-with-message ()
  "Search failure reports the dashboard error without replacing current rows."
  (let (shown)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'hermes-sessions--rest)
               (lambda (&rest _) (hermes--promise-rejected "search failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render '(((id . "s1"))))
            (with-current-buffer "*Hermes Sessions*"
              (hermes-sessions-search "needle" "work")
              (should (assoc '("" . "s1") tabulated-list-entries)))
            (should (equal shown "Hermes: search failed")))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-search-results-render-and-cache ()
  "The REST search `results' envelope feeds rows and selected-session cache."
  (let ((result '((results . (((session_id . "search-1")
                               (snippet . "Hit")))))))
    (with-temp-buffer
      (setq hermes-sessions--search-profile "work")
      (should (equal (caar (hermes-sessions--result-rows result))
                     '("work" . "search-1")))
      (hermes-sessions--record-result result)
      (should (equal (hermes-transport--get
                      (gethash '("work" . "search-1")
                               hermes-sessions--session-map)
                      'title)
                     "Hit")))))

(ert-deftest hermes-sessions-duplicate-ids-keep-profile-identity ()
  "Rows with one durable id remain distinct across profiles."
  (let ((rows (hermes-sessions--rows
               '(((id . "same") (profile . "one"))
                 ((id . "same") (profile . "two"))))))
    (should (equal (mapcar #'car rows)
                   '(("one" . "same") ("two" . "same"))))))

(ert-deftest hermes-sessions-archive-patches-selected-profile-session ()
  "Archive sends the selected session id, profile, and archived flag."
  (let (request)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional _on-success)
                 (funcall make-promise 'client)))
              ((symbol-function 'hermes-sessions--rest)
               (lambda (_client method path &optional body _query)
                 (setq request (list method path body))
                 (hermes--promise-resolved '((ok . t))))))
      (hermes-sessions-test--render
       '(((id . "s1") (title . "First") (profile . "work"))))
      (unwind-protect
          (with-current-buffer "*Hermes Sessions*"
            (goto-char (point-min))
            (search-forward "s1")
            (beginning-of-line)
            (hermes-sessions-archive))
        (kill-buffer "*Hermes Sessions*")))
    (should (equal request
                   '("PATCH" "/api/sessions/s1"
                     ((archived . t) (profile . "work")))))))

(ert-deftest hermes-sessions-unarchive-sends-json-false ()
  "Unarchive sends the encoder's exact JSON false sentinel."
  (let (body)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional _on-success)
                 (funcall make-promise 'client)))
              ((symbol-function 'hermes-sessions--rest)
               (lambda (_client _method _path &optional sent _query)
                 (setq body sent)
                 (hermes--promise-resolved '((ok . t))))))
      (hermes-sessions-test--render
       '(((id . "s1") (profile . "work") (archived . t))))
      (unwind-protect
          (with-current-buffer "*Hermes Sessions*"
            (goto-char (point-min))
            (search-forward "s1")
            (beginning-of-line)
            (hermes-sessions-unarchive))
        (kill-buffer "*Hermes Sessions*")))
    (should (eq (alist-get 'archived body) :false))))

(ert-deftest hermes-sessions-archive-rejects-with-message ()
  "Archive failure reports the dashboard error and keeps the selected row."
  (let (shown)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'hermes-sessions--rest)
               (lambda (&rest _) (hermes--promise-rejected "archive failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "s1") (profile . "work"))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-archive)
              (should (assoc '("work" . "s1") tabulated-list-entries)))
            (should (equal shown "Hermes: archive failed")))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-archive-stale-success-does-not-report-or-refresh ()
  "A late archive cannot report success or refresh a superseded list."
  (let ((patch (hermes--promise-make)) messages refreshed)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client) on-success)))
              ((symbol-function 'hermes-sessions--rest)
               (lambda (_client method &rest _)
                 (if (equal method "PATCH")
                     patch
                   (error "A stale archive must not start a new API read"))))
              ((symbol-function 'hermes-sessions--revert)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "s1") (profile . "work"))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-archive)
              (hermes-browser--next-request-generation)
              (hermes--promise-resolve patch '((ok . t)))
              (should-not refreshed)
              (should-not (cl-find-if (lambda (text)
                                        (string-match-p "archived" text))
                                      messages))))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-open-preserves-profile ()
  "Opening a row resumes its durable id under its owning profile."
  (let ((instance '("remote" . "https://hermes.example.test")) resumed)
    (cl-letf (((symbol-function 'hermes-instance-resolve)
               (lambda () instance))
              ((symbol-function 'hermes-chat-resume-session)
               (lambda (&rest args) (setq resumed args))))
      (hermes-sessions-test--render
       '(((id . "same") (title . "Other") (profile . "work"))))
      (unwind-protect
          (with-current-buffer "*Hermes Sessions*"
            (goto-char (point-min))
            (search-forward "same")
            (beginning-of-line)
            (hermes-sessions-open))
        (kill-buffer "*Hermes Sessions*")))
    (should (equal resumed (list "same" "Other" "work" instance)))))

(ert-deftest hermes-sessions-profile-history-and-rename-use-rest ()
  "Profile-owned view and rename requests target the owning REST database."
  (let (requests)
    (cl-letf (((symbol-function 'hermes-sessions--rest)
               (lambda (_client method path &optional body query)
                 (push (list method path body query) requests)
                 (hermes--promise-resolved '((messages . nil))))))
      (hermes-sessions--history-promise 'client "s1" "s1" "work")
      (hermes-sessions--set-title-promise 'client "s1" "Renamed" "work"))
    (should (member '("GET" "/api/sessions/s1/messages" nil
                      ((profile . "work")))
                    requests))
    (should (member '("PATCH" "/api/sessions/s1"
                      ((title . "Renamed") (profile . "work")) nil)
                    requests))))

(ert-deftest hermes-sessions-delete-preserves-profile-query ()
  "Delete targets the selected row's owning profile."
  (let (query)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional _on-success)
                 (funcall make-promise 'client)))
              ((symbol-function 'hermes-sessions--rest)
               (lambda (_client _method _path &optional _body sent-query)
                 (setq query sent-query)
                 (hermes--promise-resolved '((ok . t))))))
      (hermes-sessions-test--render
       '(((id . "s1") (profile . "work"))))
      (unwind-protect
          (with-current-buffer "*Hermes Sessions*"
            (goto-char (point-min))
            (search-forward "s1")
            (beginning-of-line)
            (hermes-sessions-delete))
        (kill-buffer "*Hermes Sessions*")))
    (should (equal query '((profile . "work"))))))

(ert-deftest hermes-sessions-late-rename-does-not-touch-retargeted-buffers ()
  "An instance A rename cannot update instance B list or detail buffers."
  (let ((a '("a" . "http://a")) (b '("b" . "http://b"))
        (identity '("work" . "same")))
    (unwind-protect
        (progn
          (hermes-sessions-test--render
           '(((id . "same") (profile . "work") (title . "B title"))))
          (with-current-buffer "*Hermes Sessions*"
            (hermes-browser--own-instance b))
          (hermes-sessions--render-detail
           '((id . "same") (profile . "work") (title . "B title")) nil 0 nil b)
          (hermes-sessions--after-rename nil a identity "A title")
          (with-current-buffer "*Hermes Sessions*"
            (should (equal (substring-no-properties
                            (aref (cadr (assoc identity tabulated-list-entries)) 1))
                           "B title")))
          (with-current-buffer "*Hermes Session: work/same*"
            (should (equal (hermes-transport--get
                            hermes-sessions--detail-session 'title)
                           "B title"))))
      (dolist (name '("*Hermes Sessions*" "*Hermes Session: work/same*"))
        (when (get-buffer name) (kill-buffer name))))))

(ert-deftest hermes-sessions-late-delete-does-not-touch-retargeted-buffers ()
  "An instance A delete cannot remove instance B list or detail state."
  (let ((a '("a" . "http://a")) (b '("b" . "http://b"))
        (identity '("work" . "same")))
    (unwind-protect
        (progn
          (hermes-sessions-test--render
           '(((id . "same") (profile . "work") (title . "B title"))))
          (with-current-buffer "*Hermes Sessions*"
            (hermes-browser--own-instance b))
          (hermes-sessions--render-detail
           '((id . "same") (profile . "work") (title . "B title")) nil 0 nil b)
          (hermes-sessions--after-delete nil a identity)
          (with-current-buffer "*Hermes Sessions*"
            (should (assoc identity tabulated-list-entries)))
          (should (buffer-live-p (get-buffer "*Hermes Session: work/same*"))))
      (dolist (name '("*Hermes Sessions*" "*Hermes Session: work/same*"))
        (when (get-buffer name) (kill-buffer name))))))

(ert-deftest hermes-sessions-late-rename-does-not-report-success ()
  "An instance A rename cannot report success after retargeting to B."
  (let ((pending (hermes--promise-make)) messages)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "A title"))
              ((symbol-function 'hermes-sessions--set-title-promise)
               (lambda (&rest _) pending))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "same") (profile . "work") (title . "Old"))))
            (with-current-buffer "*Hermes Sessions*"
              (hermes-browser--own-instance '("a" . "http://a"))
              (goto-char (point-min))
              (hermes-sessions-rename)
              (hermes-browser--own-instance '("b" . "http://b")))
            (hermes--promise-resolve pending '((ok . t)))
            (should-not messages))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-late-delete-does-not-report-success ()
  "An instance A delete cannot report success after retargeting to B."
  (let ((pending (hermes--promise-make)) messages)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-sessions--rest)
               (lambda (&rest _) pending))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "same") (profile . "work") (title . "Old"))))
            (with-current-buffer "*Hermes Sessions*"
              (hermes-browser--own-instance '("a" . "http://a"))
              (goto-char (point-min))
              (hermes-sessions-delete)
              (hermes-browser--own-instance '("b" . "http://b")))
            (hermes--promise-resolve pending '((ok . t)))
            (should-not messages))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-export-writes-detail-markdown ()
  "Export writes the already-loaded detail history as Markdown."
  (let ((file (make-temp-file "hermes-session-" nil ".md")))
    (unwind-protect
        (let ((buffer (hermes-sessions--render-detail
                       '((id . "s1") (title . "First"))
                       '(((role . "user") (text . "hello"))) 1)))
          (unwind-protect
              (with-current-buffer buffer
                (hermes-sessions-export file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (should (string-match-p "# First" (buffer-string)))
                  (should (string-match-p "## user" (buffer-string)))
                  (should (string-match-p "hello" (buffer-string)))))
            (kill-buffer buffer)))
      (delete-file file))))

(ert-deftest hermes-sessions-export-fallback-keeps-chat-transport-owner ()
  "Export falls back to REST without rebinding its live chat session."
  (let ((chat-owner 'shared-chat-client)
        started stopped resumed request exported)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () chat-owner))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (setq started t)))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (&rest _) (setq stopped t)))
              ((symbol-function 'hermes-dashboard-transport-session-history)
               (lambda (_client _session-id &rest args)
                 (funcall (plist-get args :reject) "session not found")))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (&rest _) (setq resumed t)))
              ((symbol-function 'hermes-sessions--rest)
               (lambda (client method path &optional body query)
                 (should (eq client chat-owner))
                 (setq request (list method path body query))
                 (hermes--promise-resolved '((messages . nil)))))
              ((symbol-function 'hermes-sessions--write-export)
               (lambda (&rest _) (setq exported t))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render '(((id . "stored"))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "stored")
              (beginning-of-line)
              (hermes-sessions-export "unused.md"))
            (should exported)
            (should (equal request
                           '("GET" "/api/sessions/stored/messages" nil nil)))
            (should (eq chat-owner 'shared-chat-client))
            (should-not resumed)
            (should-not started)
            (should-not stopped))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-detail-renders-history-messages ()
  "The detail buffer renders user, assistant, and tool history readably."
  (let ((buffer (hermes-sessions--render-detail
                 '((id . "s1") (title . "First") (source . "tui"))
                 '(((role . "user") (text . "hi there"))
                   ((role . "assistant") (text . "hello back"))
                   ((role . "tool") (name . "terminal")
                    (context . "make test")))
                 3)))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'hermes-session-detail-mode))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "Session: First" text))
            (should (string-match-p "ID: s1" text))
            (should (string-match-p "Messages: 3" text))
            (should (string-match-p "\\[user\\]" text))
            (should (string-match-p "hi there" text))
            (should (string-match-p "\\[assistant\\]" text))
            (should (string-match-p "hello back" text))
            (should (string-match-p "\\[tool: terminal\\]" text))
            (should (string-match-p "make test" text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest hermes-sessions-display-string-renders-structured-fallbacks ()
  "Structured history values render type or object fallback text."
  (should (equal (hermes-sessions--display-string '((type . "image_url")))
                 "[image_url]"))
  (should (string-match-p "foo"
                          (hermes-sessions--display-string
                           '((foo . "bar"))))))

(ert-deftest hermes-sessions-detail-renders-tool-name-fallbacks ()
  "Tool messages and tool calls use alternate name fields."
  (let ((buffer (hermes-sessions--render-detail
                 '((id . "s1") (title . "First"))
                 '(((role . "tool") (tool_name . "terminal")
                    (output . "done"))
                   ((role . "assistant") (text . "running")
                    (tool_calls . [((id . "call-1")
                                    (name . "terminal")
                                    (arguments . "make test"))])))
                 2)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "\\[tool: terminal\\]" text))
            (should (string-match-p "tool-call: terminal" text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest hermes-sessions-view-fetches-history-and-renders-detail ()
  "Viewing a row requests `session.history' and renders the result."
  (let ((history-result '((count . 2)
                          (messages . (((role . "user") (text . "question"))
                                       ((role . "assistant")
                                        (text . "answer"))))))
        (instance '("remote" . "https://hermes.example.test"))
        (hermes-instances
         '(("remote" . "https://hermes.example.test")))
        history-session stopped)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-session-history)
               (lambda (client session-id &rest args)
                 (should (eq client 'fake-client))
                 (setq history-session session-id)
                 (funcall (plist-get args :resolve) history-result))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (setq hermes-instance instance)
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-view))
            (should (equal history-session "s1"))
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Session: s1*"
              (should (equal hermes-instance instance))
              (should (string-match-p "question" (buffer-string)))
              (should (string-match-p "answer" (buffer-string)))))
        (dolist (name '("*Hermes Sessions*" "*Hermes Session: s1*"))
          (when (get-buffer name)
            (kill-buffer name)))))))

(ert-deftest hermes-sessions-view-stale-detail-live-id-reads-durable-rest ()
  "Detail refresh reads durable history after stale live history fails."
  (let ((history-result '((count . 1)
                          (messages . (((role . "assistant")
                                        (text . "stored"))))))
        history-session rest-request resume-called stopped)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-session-history)
               (lambda (client session-id &rest args)
                 (should (eq client 'fake-client))
                 (setq history-session session-id)
                 (funcall (plist-get args :reject) "session not found")))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (&rest _) (setq resume-called t)))
              ((symbol-function 'hermes-sessions--rest)
               (lambda (client method path &optional body query)
                 (should (eq client 'fake-client))
                 (setq rest-request (list method path body query))
                 (hermes--promise-resolved history-result))))
      (unwind-protect
          (progn
            (hermes-sessions--render-detail
             '((id . "durable-1") (live_session_id . "dead-live")
               (title . "Stored"))
             nil 0)
            (with-current-buffer "*Hermes Session: durable-1*"
              (hermes-sessions-view))
            (should (equal history-session "dead-live"))
            (should (equal rest-request
                           '("GET" "/api/sessions/durable-1/messages" nil nil)))
            (should-not resume-called)
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Session: durable-1*"
              (should (string-match-p "stored" (buffer-string)))))
        (when (get-buffer "*Hermes Session: durable-1*")
          (kill-buffer "*Hermes Session: durable-1*"))))))

(ert-deftest hermes-sessions-view-fallback-keeps-chat-transport-owner ()
  "History fallback reads REST without rebinding its live chat session."
  (let ((chat-owner 'shared-chat-client)
        started stopped resumed request)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () chat-owner))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (setq started t)))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (&rest _) (setq stopped t)))
              ((symbol-function 'hermes-dashboard-transport-session-history)
               (lambda (_client _session-id &rest args)
                 (funcall (plist-get args :reject) "session not found")))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (&rest _) (setq resumed t)))
              ((symbol-function 'hermes-sessions--rest)
               (lambda (client method path &optional body query)
                 (should (eq client chat-owner))
                 (setq request (list method path body query))
                 (hermes--promise-resolved '((messages . nil))))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render '(((id . "stored"))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "stored")
              (beginning-of-line)
              (hermes-sessions-view))
            (should (equal request
                           '("GET" "/api/sessions/stored/messages" nil nil)))
            (should (eq chat-owner 'shared-chat-client))
            (should-not resumed)
            (should-not started)
            (should-not stopped))
        (dolist (name '("*Hermes Sessions*" "*Hermes Session: stored*"))
          (when (get-buffer name) (kill-buffer name)))))))

(ert-deftest hermes-sessions-rename-fallback-keeps-chat-transport-owner ()
  "Rename fallback patches REST without rebinding its live chat session."
  (let ((chat-owner 'shared-chat-client)
        started stopped resumed request)
    (cl-letf (((symbol-function 'hermes-browser--existing-client)
               (lambda () chat-owner))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (setq started t)))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (&rest _) (setq stopped t)))
              ((symbol-function 'read-string) (lambda (&rest _) "Renamed"))
              ((symbol-function 'hermes-dashboard-transport-session-title)
               (lambda (_client &rest args)
                 (funcall (plist-get args :reject) "session not found")))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (&rest _) (setq resumed t)))
              ((symbol-function 'hermes-sessions--rest)
               (lambda (client method path &optional body query)
                 (should (eq client chat-owner))
                 (setq request (list method path body query))
                 (hermes--promise-resolved '((title . "Renamed"))))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "stored") (title . "Old"))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "stored")
              (beginning-of-line)
              (hermes-sessions-rename))
            (should (equal request
                           '("PATCH" "/api/sessions/stored"
                             ((title . "Renamed")) nil)))
            (should (eq chat-owner 'shared-chat-client))
            (should-not resumed)
            (should-not started)
            (should-not stopped))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-view-rejects-with-message ()
  "A failed history request reports the gateway error without rendering detail."
  (let (shown)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-history)
               (lambda (_client _session-id &rest args)
                 (funcall (plist-get args :reject) "history failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq shown (apply #'format fmt args)))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-view))
            (should (equal shown "Hermes: history failed"))
            (should-not (get-buffer "*Hermes Session: s1*")))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-detail-refresh-does-not-resurrect-killed-buffer ()
  "A late detail refresh does not recreate its killed session buffer."
  (let ((promise (hermes--promise-make))
        (buffer (hermes-sessions--render-detail '((id . "s1")) nil 0)))
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'fake-client)
                                       on-success)))
              ((symbol-function 'hermes-sessions--history-promise)
               (lambda (&rest _) promise)))
      (with-current-buffer buffer (hermes-sessions-view))
      (kill-buffer buffer)
      (hermes--promise-resolve promise '((messages . nil) (count . 0)))
      (should-not (get-buffer "*Hermes Session: s1*")))))

(ert-deftest hermes-sessions-detail-refresh-keeps-newest-result ()
  "An older detail refresh cannot replace a newer history result."
  (let ((first (hermes--promise-make))
        (second (hermes--promise-make))
        (requests 0)
        (buffer (hermes-sessions--render-detail '((id . "s1")) nil 0)))
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'fake-client)
                                       on-success)))
              ((symbol-function 'hermes-sessions--history-promise)
               (lambda (&rest _)
                 (setq requests (1+ requests))
                 (if (= requests 1) first second))))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (hermes-sessions-view)
              (hermes-sessions-view))
            (hermes--promise-resolve
             second '((messages . (((role . "assistant") (text . "new"))))
                      (count . 1)))
            (hermes--promise-resolve
             first '((messages . (((role . "assistant") (text . "old"))))
                     (count . 1)))
            (with-current-buffer buffer
              (should (string-match-p "new" (buffer-string)))
              (should-not (string-match-p "old" (buffer-string)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-sessions-replaced-title-keeps-title-face ()
  "An in-place session rename preserves the title column face."
  (unwind-protect
      (progn
        (hermes-sessions-test--render '(((id . "s1") (title . "First"))))
        (with-current-buffer "*Hermes Sessions*"
          (hermes-sessions--replace-browser-row-title '("" . "s1") "Renamed")
          (let ((title (aref (cadr (assoc '("" . "s1")
                                          tabulated-list-entries)) 1)))
            (should (equal title "Renamed"))
            (should (eq (get-text-property 0 'face title)
                        'hermes-browser-title)))))
    (when (get-buffer "*Hermes Sessions*")
      (kill-buffer "*Hermes Sessions*"))))

(ert-deftest hermes-sessions-rename-prompts-and-dispatches-title ()
  "Renaming a selected row prompts and dispatches `session.title'."
  (let (sent stopped)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'read-string)
               (lambda (&rest _) "Renamed"))
              ((symbol-function 'hermes-dashboard-transport-session-title)
               (lambda (client &rest args)
                 (setq sent (cons client args))
                 (funcall (plist-get args :resolve)
                          '((pending . :json-false) (title . "Renamed"))))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-rename))
            (should (eq (car sent) 'fake-client))
            (should (equal (plist-get (cdr sent) :session-id) "s1"))
            (should (equal (plist-get (cdr sent) :title) "Renamed"))
            (should (eq stopped 'fake-client)))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-rename-from-detail-updates-open-browser ()
  "Renaming from detail keeps an open browser row in sync."
  (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
            ((symbol-function 'hermes-dashboard-transport-start)
             (lambda (&rest _) 'fake-client))
            ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
            ((symbol-function 'read-string)
             (lambda (&rest _) "Renamed"))
            ((symbol-function 'hermes-dashboard-transport-session-title)
             (lambda (_client &rest args)
               (funcall (plist-get args :resolve)
                        '((pending . :json-false) (title . "Renamed"))))))
    (unwind-protect
        (progn
          (hermes-sessions-test--render
           '(((id . "s1") (title . "First") (message_count . 2))))
          (hermes-sessions--render-detail
           '((id . "s1") (title . "First") (message_count . 2))
           '(((role . "user") (text . "question")))
           1)
          (with-current-buffer "*Hermes Session: s1*"
            (hermes-sessions-rename))
          (with-current-buffer "*Hermes Sessions*"
            (should (equal (aref (cadr (assoc '("" . "s1")
                                             tabulated-list-entries)) 1)
                           "Renamed")))
          (with-current-buffer "*Hermes Session: s1*"
            (should (string-match-p "Session: Renamed" (buffer-string)))))
      (dolist (name '("*Hermes Sessions*" "*Hermes Session: s1*"))
        (when (get-buffer name)
          (kill-buffer name))))))

(ert-deftest hermes-sessions-rename-rejects-empty-title-before-dispatch ()
  "Empty rename input is rejected locally before an RPC is sent."
  (let (sent)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  "))
              ((symbol-function 'hermes-dashboard-transport-session-title)
               (lambda (&rest _) (setq sent t))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (should-error (hermes-sessions-rename) :type 'user-error))
            (should-not sent))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-rename-rejects-with-message ()
  "A failed rename request reports the gateway error without updating the row."
  (let (shown)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) "Renamed"))
              ((symbol-function 'hermes-dashboard-transport-session-title)
               (lambda (_client &rest args)
                 (funcall (plist-get args :reject) "rename failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq shown (apply #'format fmt args)))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-rename)
              (should (equal (aref (cadr (assoc '("" . "s1")
                                               tabulated-list-entries)) 1)
                             "First")))
            (should (equal shown "Hermes: rename failed")))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-delete-prompts-and-dispatches-delete ()
  "Deleting a selected row asks for confirmation before `session.delete'."
  (let (deleted stopped)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-sessions--rest)
               (lambda (client method path &optional _body query)
                 (setq deleted (list client method path query))
                 (hermes--promise-resolved '((ok . t))))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (hermes-sessions--render-detail
             '((id . "s1") (title . "First") (message_count . 2))
             '(((role . "user") (text . "question")))
             1)
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-delete)
              (should-not (assoc '("" . "s1") tabulated-list-entries)))
            (should-not (get-buffer "*Hermes Session: s1*"))
            (should (equal deleted
                           '(fake-client "DELETE" "/api/sessions/s1" nil)))
            (should (eq stopped 'fake-client)))
        (dolist (name '("*Hermes Sessions*" "*Hermes Session: s1*"))
          (when (get-buffer name)
            (kill-buffer name)))))))

(ert-deftest hermes-sessions-delete-cancel-does-not-dispatch ()
  "Answering no to the delete prompt leaves the session untouched."
  (let (deleted)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'hermes-dashboard-transport-session-delete)
               (lambda (&rest _) (setq deleted t))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-delete))
            (should-not deleted))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-delete-rejects-with-message ()
  "A failed delete request reports the gateway error."
  (let (shown)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-sessions--rest)
               (lambda (&rest _)
                 (hermes--promise-rejected "delete failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq shown (apply #'format fmt args)))))
      (unwind-protect
          (progn
            (hermes-sessions-test--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-delete))
            (should (equal shown "Hermes: delete failed")))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-list-renders-and-stops-transient-client ()
  "Listing connects a transient client, renders rows, then stops it."
  (let (listed stopped)
    (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-session-list)
               (lambda (client &rest args)
                 (setq listed client)
                 (funcall (plist-get args :resolve)
                          '((sessions
                             . (((id . "s1") (title . "First") (message_count . 3))
                                ((id . "s2") (title . "Second") (message_count . 0)))))))))
      (unwind-protect
          (progn
            (hermes-list-sessions)
            (should (eq listed 'fake-client))
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Sessions*"
              (should (derived-mode-p 'hermes-sessions-mode))
              (should (equal (mapcar #'car tabulated-list-entries)
                             '(("" . "s1") ("" . "s2"))))))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(provide 'hermes-sessions-tests)
;;; hermes-sessions-tests.el ends here
