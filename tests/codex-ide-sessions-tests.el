;;; codex-ide-sessions-tests.el --- Session list tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;;; Commentary:

;; Isolated buffer fixtures; no CLI, network, or terminal process is started.

;;; Code:

(require 'ert)
(require 'codex-ide-sessions)

(defmacro codex-ide-sessions-test--with-sessions (&rest body)
  "Run BODY with isolated session buffers and a private registry."
  (declare (indent 0) (debug t))
  `(let* ((codex-ide--sessions (make-hash-table :test #'equal))
          (codex-ide--active-session-ids (make-hash-table :test #'equal))
          (first-buffer (generate-new-buffer " *session-first*"))
          (second-buffer (generate-new-buffer " *session-second*"))
          (first (codex-ide--make-session "/first/project/" 1 first-buffer nil))
          (second (codex-ide--make-session "/second/project/" 2 second-buffer nil)))
     (puthash "/first/project/" (list first) codex-ide--sessions)
     (puthash "/second/project/" (list second) codex-ide--sessions)
     (puthash "/first/project/" 1 codex-ide--active-session-ids)
     (with-current-buffer first-buffer (setq-local codex-ide-term--backend 'eat))
     (with-current-buffer second-buffer (setq-local codex-ide-term--backend 'neo-term))
     (unwind-protect
         (cl-letf (((symbol-function 'codex-ide--recover-live-sessions) #'ignore)
                   ((symbol-function 'codex-ide--session-live-p)
                    (lambda (session) (buffer-live-p (plist-get session :buffer)))))
           (with-temp-buffer
             (codex-ide-sessions-mode)
             (tabulated-list-print)
             ,@body))
       (when (buffer-live-p first-buffer) (kill-buffer first-buffer))
       (when (buffer-live-p second-buffer) (kill-buffer second-buffer)))))

(ert-deftest codex-ide-sessions-columns-and-navigation ()
  "Rows expose live session data and native navigation bindings."
  (codex-ide-sessions-test--with-sessions
    (should (equal (mapcar #'car (append tabulated-list-format nil))
                   '("Project" "Label / Buffer" "Backend" "Status" "Directory")))
    (let ((columns (cadr (codex-ide-sessions--entry first))))
      (should (equal (append columns nil)
                     (list "project" (buffer-name first-buffer) "eat"
                           "Active" "/first/project/")))
      (should (eq (get-text-property 0 'face (aref columns 3)) 'success)))
    (should (eq (key-binding (kbd "n")) #'next-line))
    (should (eq (key-binding (kbd "p")) #'previous-line))
    (should (commandp (key-binding (kbd "?"))))
    (goto-char (point-min))
    (should (tabulated-list-get-id))
    (forward-line 1)
    (should (tabulated-list-get-id))))

(ert-deftest codex-ide-sessions-refresh-preserves-row-and-column ()
  "Refresh and rename keep point on the same session."
  (codex-ide-sessions-test--with-sessions
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 5)
    (let ((session (tabulated-list-get-id)) (column (current-column)))
      (with-current-buffer (plist-get session :buffer)
        (setq-local codex-ide--session-label "review"))
      (revert-buffer)
      (should (eq (tabulated-list-get-id) session))
      (should (= (current-column) column))
      (should (equal (aref (tabulated-list-get-entry) 1) "review")))))

(ert-deftest codex-ide-sessions-switch-uses-existing-helper ()
  "RET delegates exact selected ownership and original source context."
  (codex-ide-sessions-test--with-sessions
    (goto-char (point-min))
    (let ((selected (tabulated-list-get-id)) switched recorded)
      (setq codex-ide-sessions--source-buffer first-buffer)
      (cl-letf (((symbol-function 'codex-ide--switch-to-session)
                 (lambda (session) (setq switched session)))
                ((symbol-function 'codex-ide--record-source-buffer)
                 (lambda (root buffer) (setq recorded (list root buffer)))))
        (call-interactively (key-binding (kbd "RET"))))
      (should (eq switched selected))
      (should (equal recorded (list (plist-get selected :root) first-buffer))))))

(ert-deftest codex-ide-sessions-kill-confirms-exact-row ()
  "Declined kill preserves buffers; accepted kill affects only the row."
  (codex-ide-sessions-test--with-sessions
    (goto-char (point-min))
    (let ((selected (tabulated-list-get-id)) asked)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt) (setq asked prompt) nil)))
        (call-interactively (key-binding (kbd "k"))))
      (should (string-match-p "Kill Codex session" asked))
      (should (buffer-live-p first-buffer))
      (should (buffer-live-p second-buffer))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (codex-ide-sessions-kill))
      (should-not (buffer-live-p (plist-get selected :buffer)))
      (should (= (count-lines (point-min) (point-max)) 1)))))

(ert-deftest codex-ide-sessions-rejects-replaced-owner-after-confirmation ()
  "A nested confirmation cannot authorize killing a replacement owner."
  (codex-ide-sessions-test--with-sessions
    (goto-char (point-min))
    (let* ((selected (tabulated-list-get-id))
           (replacement (copy-sequence selected)))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_)
                   (puthash (plist-get selected :root) (list replacement)
                            codex-ide--sessions)
                   t)))
        (should-error (codex-ide-sessions-kill) :type 'user-error))
      (should (buffer-live-p (plist-get selected :buffer))))))

(ert-deftest codex-ide-sessions-rename-uses-label-api ()
  "Rename passes the selected row and trimmed input to the label API."
  (codex-ide-sessions-test--with-sessions
    (goto-char (point-min))
    (let ((selected (tabulated-list-get-id)) renamed)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) " review "))
                ((symbol-function 'codex-ide--set-session-label)
                 (lambda (session label) (setq renamed (list session label)))))
        (call-interactively (key-binding (kbd "r"))))
      (should (equal renamed (list selected "review"))))))

(ert-deftest codex-ide-sessions-empty-and-dead-rows ()
  "Empty lists open normally and vanished sessions cannot be operated on."
  (codex-ide-sessions-test--with-sessions
    (goto-char (point-min))
    (kill-buffer (plist-get (tabulated-list-get-id) :buffer))
    (should-error (codex-ide-sessions-switch) :type 'user-error)
    (clrhash codex-ide--sessions)
    (revert-buffer)
    (should (= (point-min) (point-max)))
    (should-error (codex-ide-sessions-kill) :type 'user-error)))

(ert-deftest codex-ide-sessions-public-list-displays-native-buffer ()
  "The public list command displays a tabulated buffer even when empty."
  (let ((codex-ide--sessions (make-hash-table :test #'equal))
        (codex-ide-sessions--buffer nil)
        (name "*Codex Sessions*") displayed)
    (should-not (get-buffer name))
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide--recover-live-sessions) #'ignore)
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq displayed buffer))))
          (codex-ide-list-sessions)
          (with-current-buffer displayed
            (should (derived-mode-p 'tabulated-list-mode))
            (should (= (buffer-size) 0)))
          (let ((first displayed))
            (codex-ide-list-sessions)
            (should (eq first displayed))))
      (when (get-buffer name) (kill-buffer name)))))

(ert-deftest codex-ide-sessions-list-preserves-foreign-and-retired-buffers ()
  "Name collisions and retired list buffers never lose their contents."
  (dolist (previously-owned '(nil retired file))
    (let* ((codex-ide--sessions (make-hash-table :test #'equal))
           (foreign (generate-new-buffer "*Codex Sessions*"))
           (codex-ide-sessions--buffer (and previously-owned foreign))
           (path "/tmp/codex-list-unwritten.txt")
           displayed)
      (unwind-protect
          (progn
            (with-current-buffer foreign
              (when previously-owned (codex-ide-sessions-mode))
              (unless (eq previously-owned 'file) (text-mode))
              (setq buffer-file-name path buffer-read-only nil)
              (insert "UNSAVED USER DRAFT"))
            (let ((map (with-current-buffer foreign (current-local-map))))
              (cl-letf (((symbol-function 'codex-ide--recover-live-sessions) #'ignore)
                        ((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _) (setq displayed buffer))))
                (codex-ide-list-sessions))
              (should-not (eq foreign displayed))
              (with-current-buffer foreign
                (should (equal (buffer-string) "UNSAVED USER DRAFT"))
                (should (buffer-modified-p))
                (should (equal buffer-file-name path))
                (should (eq major-mode (if (eq previously-owned 'file)
                                           'codex-ide-sessions-mode 'text-mode)))
                (should (eq (current-local-map) map))))
            (with-current-buffer displayed
              (should (derived-mode-p 'codex-ide-sessions-mode))))
        (when (buffer-live-p displayed) (kill-buffer displayed))
        (kill-buffer foreign)))))

(ert-deftest codex-ide-sessions-mode-hook-cannot-transfer-buffer-ownership ()
  "A mode hook that repurposes the new list keeps its data intact."
  (let ((codex-ide-sessions--buffer nil)
        (codex-ide-sessions-mode-hook
         (list (lambda ()
                 (setq buffer-file-name "/tmp/codex-list-hook-unwritten.txt")
                 (let ((inhibit-read-only t)) (insert "HOOK DRAFT")))))
        created)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
          (should-error (codex-ide-list-sessions) :type 'user-error)
          (setq created (get-buffer "*Codex Sessions*"))
          (should (buffer-live-p created))
          (with-current-buffer created
            (should (equal (buffer-string) "HOOK DRAFT"))
            (should (buffer-modified-p))))
      (when (buffer-live-p created) (kill-buffer created)))))

(provide 'codex-ide-sessions-tests)

;;; codex-ide-sessions-tests.el ends here
