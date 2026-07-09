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
                '(((id . "s1") (title . "First") (message_count . 3) (source . "tui")))))
         (entry (cadr (car rows))))
    (should (equal (caar rows) "s1"))
    (should (equal (aref entry 0) "s1"))
    (should (equal (aref entry 1) "First"))
    (should (equal (aref entry 2) "3"))
    (should (equal (aref entry 3) "tui"))))

(ert-deftest hermes-sessions-mode-keymap-keeps-ret-and-adds-actions ()
  "The browser keeps RET resume and exposes native history/actions."
  (should (eq (keymap-lookup hermes-sessions-mode-map "RET")
              #'hermes-sessions-open))
  (should (eq (keymap-lookup hermes-sessions-mode-map "v")
              #'hermes-sessions-view))
  (should (eq (keymap-lookup hermes-sessions-mode-map "r")
              #'hermes-sessions-rename))
  (should (eq (keymap-lookup hermes-sessions-mode-map "d")
              #'hermes-sessions-delete)))

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
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-view))
            (should (equal history-session "s1"))
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Session: s1*"
              (should (string-match-p "question" (buffer-string)))
              (should (string-match-p "answer" (buffer-string)))))
        (dolist (name '("*Hermes Sessions*" "*Hermes Session: s1*"))
          (when (get-buffer name)
            (kill-buffer name)))))))

(ert-deftest hermes-sessions-view-stale-detail-live-id-resumes-durable-id ()
  "Detail refresh resumes the durable id after stale live history fails."
  (let ((history-result '((count . 1)
                          (messages . (((role . "assistant")
                                        (text . "resumed"))))))
        history-session resume-session stopped)
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
               (lambda (client session-id &rest args)
                 (should (eq client 'fake-client))
                 (setq resume-session session-id)
                 (funcall (plist-get args :resolve) history-result))))
      (unwind-protect
          (progn
            (hermes-sessions--render-detail
             '((id . "durable-1") (live_session_id . "dead-live")
               (title . "Stored"))
             nil 0)
            (with-current-buffer "*Hermes Session: durable-1*"
              (hermes-sessions-view))
            (should (equal history-session "dead-live"))
            (should (equal resume-session "durable-1"))
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Session: durable-1*"
              (should (string-match-p "resumed" (buffer-string)))))
        (when (get-buffer "*Hermes Session: durable-1*")
          (kill-buffer "*Hermes Session: durable-1*"))))))

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
            (should (equal (aref (cadr (assoc "s1" tabulated-list-entries)) 1)
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
              (should (equal (aref (cadr (assoc "s1" tabulated-list-entries)) 1)
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
              ((symbol-function 'hermes-dashboard-transport-session-delete)
               (lambda (client session-id &rest args)
                 (setq deleted (list client session-id))
                 (funcall (plist-get args :resolve)
                          '((deleted . "s1"))))))
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
              (should-not (assoc "s1" tabulated-list-entries)))
            (should-not (get-buffer "*Hermes Session: s1*"))
            (should (equal deleted '(fake-client "s1")))
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
              ((symbol-function 'hermes-dashboard-transport-session-delete)
               (lambda (_client _session-id &rest args)
                 (funcall (plist-get args :reject) "delete failed")))
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
              (should (equal (sort (mapcar #'car tabulated-list-entries) #'string<)
                             '("s1" "s2")))))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(provide 'hermes-sessions-tests)
;;; hermes-sessions-tests.el ends here
