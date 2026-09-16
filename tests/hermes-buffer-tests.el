;;; hermes-buffer-tests.el --- Named view ownership tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise named views through commands and buttons, with disposable buffers.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)
(require 'hermes-exec)

(defun hermes-buffer-test--snapshot ()
  "Return the current buffer's user-owned state."
  (list (buffer-string) buffer-undo-list (buffer-modified-p)
        major-mode buffer-file-name))

(defun hermes-buffer-test--collision (name open &optional mode no-file)
  "Assert OPEN preserves a collision at NAME and reuses its own view.
MODE makes the unrelated buffer look like a Hermes view.
NO-FILE leaves it as an ordinary buffer rather than a visited file."
  (let ((before (buffer-list))
        (collision (generate-new-buffer name))
        (hermes-dashboard-stale-refresh-interval nil))
    (unwind-protect
        (with-current-buffer collision
          (when mode (funcall mode))
          (setq buffer-read-only nil)
          (buffer-enable-undo)
          (insert "Unrelated unsaved text\n")
          (unless no-file
            (setq buffer-file-name "/nonexistent/hermes-view-collision.txt"))
          (let ((snapshot (hermes-buffer-test--snapshot))
                (view (funcall open)))
            (should (buffer-live-p view))
            (should-not (eq collision view))
            (should (equal snapshot (hermes-buffer-test--snapshot)))
            (should (eq view (funcall open)))
            (should (equal snapshot (hermes-buffer-test--snapshot)))))
      (dolist (buffer (seq-difference (buffer-list) before))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil)
            (setq buffer-file-name nil))
          (kill-buffer buffer))))))

(ert-deftest hermes-buffer-dashboard-preserves-collision ()
  "The public dashboard preserves an unrelated file even in dashboard mode."
  (cl-letf (((symbol-function 'hermes-dashboard--check-auth) #'ignore)
            ((symbol-function 'hermes-dashboard--warm-profile-cache) #'ignore)
            ((symbol-function 'keymap-popup) #'ignore))
    (hermes-buffer-test--collision
     hermes-dashboard-buffer-name
     (lambda () (save-current-buffer (hermes) (current-buffer)))
     #'hermes-dashboard-mode)))

(ert-deftest hermes-buffer-buttons-preserve-collisions ()
  "Native diff and background buttons do not erase same-name user files."
  (dolist (spec '(("*Hermes Diff*" hermes-chat--view-diff-button
                  hermes-chat-diff "--- a/a\n+++ b/a\n@@ -1 +1 @@\n-old\n+new\n")
                 ("*hermes-bg #37*" hermes-chat--view-background-button
                  hermes-chat-background-content "Background result")))
    (with-temp-buffer
      (insert-text-button "Open" 'action (nth 1 spec)
                          (nth 2 spec) (nth 3 spec)
                          'hermes-chat-background-number 37)
      (let ((button (button-at (point-min))))
        (hermes-buffer-test--collision
         (car spec)
         (lambda ()
           (save-current-buffer (button-activate button) (current-buffer))))))))

(ert-deftest hermes-buffer-queue-preserves-collision ()
  "The public queue command keeps collisions intact and reuses its panel."
  (with-temp-buffer
    (hermes-chat-mode)
    (let ((chat (current-buffer)))
      (hermes-buffer-test--collision
       (format "*Hermes Queue: %s*" (buffer-name chat))
       (lambda () (with-current-buffer chat (hermes-chat-queue-panel)))))))

(ert-deftest hermes-buffer-browser-orphan-does-not-adopt-successor ()
  "A pending public browser read cannot paint a same-name successor."
  (let ((before (buffer-list))
        callbacks first second)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--run-on-client)
                   (lambda (_fetch success &rest _) (push success callbacks))))
          (save-current-buffer (hermes-list-profiles) (setq first (current-buffer)))
          (with-current-buffer first (fundamental-mode)
                                     (let ((inhibit-read-only t)) (insert "Retired view")))
          (let ((old-text (with-current-buffer first (buffer-string))))
            (save-current-buffer (hermes-list-profiles) (setq second (current-buffer)))
            (should-not (eq first second))
            (funcall (cadr callbacks) '((profiles . (((name . "orphan"))))))
            (should (equal old-text (with-current-buffer first (buffer-string)))))
          (funcall (car callbacks) '((profiles . (((name . "current"))))))
          (with-current-buffer second
            (should (string-match-p "current" (buffer-string)))
            (should-not (string-match-p "orphan" (buffer-string))))
          (save-current-buffer (hermes-list-profiles) (should (eq second (current-buffer)))))
      (mapc #'kill-buffer (seq-difference (buffer-list) before)))))

(ert-deftest hermes-buffer-file-reassociation-retires-browser ()
  "Visiting then detaching a file retires reuse and a pending browser read."
  (let ((before (buffer-list)) callbacks first)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--run-on-client)
                   (lambda (_fetch success &rest _) (push success callbacks))))
          (save-current-buffer (hermes-list-profiles) (setq first (current-buffer)))
          (with-current-buffer first
            (set-visited-file-name "/nonexistent/hermes-retired-view.txt" t)
            (set-visited-file-name nil t)
            (let ((inhibit-read-only t)) (erase-buffer) (insert "Retired file text")))
          (let ((old-text (with-current-buffer first (buffer-string))))
            (funcall (car callbacks) '((profiles . (((name . "orphan"))))))
            (should (equal old-text (with-current-buffer first (buffer-string))))
            (save-current-buffer (hermes-list-profiles)
                                 (should-not (eq first (current-buffer))))))
      (dolist (buffer (seq-difference (buffer-list) before))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest hermes-buffer-provider-accounts-and-oauth-preserve-collisions ()
  "Public account browsing and row activation preserve colliding views."
  (let ((provider '((id . "example") (name . "Example")
                    (flow . "device_code")))
        (instance (hermes-instance-resolve)))
    (cl-letf (((symbol-function 'hermes-browser--run-on-client) #'ignore))
      (hermes-buffer-test--collision
       "*Hermes Provider Accounts*"
       (lambda ()
         (save-current-buffer (hermes-onboarding-oauth-connect)
                              (window-buffer (selected-window))))))
    (with-temp-buffer
      (hermes-provider-accounts-mode)
      (hermes-provider-accounts--render `((providers . (,provider))))
      (goto-char (point-min))
      (let ((accounts (current-buffer)))
        (cl-letf (((symbol-function 'hermes-browser--existing-client)
                   (lambda ()
                     (make-hermes-dashboard-transport-client
                      :base-url "https://fixture.invalid")))
                  ((symbol-function 'hermes-onboarding--oauth-start)
                   (lambda (&rest _) (hermes--promise-make))))
          (hermes-buffer-test--collision
           (hermes-onboarding--oauth-buffer-name instance)
           (lambda ()
             (save-current-buffer
               (with-current-buffer accounts
                 (call-interactively #'hermes-onboarding-provider-account-act))
               (window-buffer (selected-window))))))))))

(ert-deftest hermes-buffer-dashboard-reuse-keeps-one-timer ()
  "Successive public openings keep one owned dashboard timer."
  (let ((before (buffer-list))
        (hermes-dashboard-stale-refresh-interval 600)
        first timer)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-dashboard--check-auth) #'ignore)
                  ((symbol-function 'hermes-dashboard--warm-profile-cache) #'ignore)
                  ((symbol-function 'keymap-popup) #'ignore))
          (save-current-buffer
            (hermes)
            (setq first (current-buffer)
                  timer hermes-dashboard--stale-refresh-timer))
          (should (memq timer timer-list))
          (save-current-buffer
            (hermes)
            (should (eq first (current-buffer)))
            (should (eq timer hermes-dashboard--stale-refresh-timer)))
          (with-current-buffer first
            (set-visited-file-name "/nonexistent/hermes-dashboard.txt" t))
          (should-not (memq timer timer-list)))
      (dolist (buffer (seq-difference (buffer-list) before))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest hermes-buffer-queue-retired-target-is-not-refreshed ()
  "Chat activity ignores a queue panel reassociated with a user file."
  (let ((before (buffer-list)))
    (unwind-protect
        (with-temp-buffer
          (hermes-chat-mode)
          (let ((panel (hermes-chat-queue-panel)))
            (with-current-buffer panel
              (set-visited-file-name "/nonexistent/hermes-queue.txt" t)
              (set-visited-file-name nil t)
              (let ((inhibit-read-only t)) (erase-buffer) (insert "User text")))
            (hermes-chat--queue-panel-refresh-if-live)
            (should (equal "User text" (with-current-buffer panel (buffer-string))))
            (should-not (eq panel (hermes-chat-queue-panel)))))
      (dolist (buffer (seq-difference (buffer-list) before))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest hermes-buffer-exec-cleanup-preserves-collision ()
  "Approval cleanup kills only the owned buffer, not its name collision."
  (let ((before (buffer-list))
        (collision (generate-new-buffer hermes-exec--approval-buffer-name)))
    (unwind-protect
        (progn
          (with-current-buffer collision (insert "User approval notes"))
          (let ((approval (hermes-exec--approval-buffer '(:code "(+ 1 2)"))))
            (should-not (eq approval collision))
            (let ((hermes-exec--active (list :buffer approval)))
              (hermes-exec--close-approval-window))
            (should-not (buffer-live-p approval))
            (should (equal "User approval notes"
                           (with-current-buffer collision (buffer-string))))))
      (mapc #'kill-buffer (seq-difference (buffer-list) before)))))

(ert-deftest hermes-buffer-browser-mode-alone-is-not-ownership ()
  "The public browser does not claim an ordinary same-mode buffer."
  (cl-letf (((symbol-function 'hermes-browser--run-on-client) #'ignore))
    (hermes-buffer-test--collision
     "*Hermes Profiles*"
     (lambda () (save-current-buffer (hermes-list-profiles) (current-buffer)))
     #'hermes-profiles-mode t)))

(ert-deftest hermes-buffer-oauth-reassociation-retires-pending-result ()
  "An OAuth result cannot overwrite a status view adopted as a user file."
  (let ((before (buffer-list)) callback)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--existing-client)
                   (lambda ()
                     (make-hermes-dashboard-transport-client
                      :base-url "https://fixture.invalid")))
                  ((symbol-function 'hermes-onboarding--oauth-start)
                   (lambda (&rest _) (setq callback (hermes--promise-make)))))
          (with-temp-buffer
            (hermes-provider-accounts-mode)
            (hermes-provider-accounts--render
             '((providers . (((id . "example") (name . "Example")
                              (flow . "device_code"))))))
            (goto-char (point-min))
            (call-interactively #'hermes-onboarding-provider-account-act))
          (let ((view (window-buffer (selected-window))))
            (with-current-buffer view
              (set-visited-file-name "/nonexistent/hermes-oauth.txt" t)
              (set-visited-file-name nil t)
              (let ((inhibit-read-only t)) (erase-buffer) (insert "User text")))
            (hermes--promise-resolve
             callback '((status . "pending") (session_id . "old-flow")))
            (should (equal "User text" (with-current-buffer view (buffer-string))))))
      (dolist (buffer (seq-difference (buffer-list) before))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest hermes-buffer-memory-entry-preserves-same-mode-file ()
  "The custom memory entry cannot infer ownership from its current mode."
  (cl-letf (((symbol-function 'hermes-browser--run-on-client) #'ignore))
    (hermes-buffer-test--collision
     "*Hermes Memory*"
     (lambda () (save-current-buffer (hermes-memory-status) (current-buffer)))
     #'hermes-memory-status-mode)))

(ert-deftest hermes-buffer-exec-orphan-cannot-adopt-successor ()
  "A retired approval cannot repaint or close a namesake successor."
  (let ((before (buffer-list)) (hermes-exec--pending nil))
    (unwind-protect
        (let* ((old (hermes-exec--approval-buffer '(:code "old")))
               (hermes-exec--active (list :buffer old :code "old")))
          (kill-buffer old)
          (let* ((new (hermes-exec--approval-buffer '(:code "new")))
                 (text (with-current-buffer new (buffer-string))))
            (hermes-exec--refresh-active-buffer)
            (should (equal text (with-current-buffer new (buffer-string))))
            (hermes-exec--close-approval-window)
            (should (buffer-live-p new))))
      (mapc #'kill-buffer (seq-difference (buffer-list) before)))))

(defmacro hermes-buffer-test--with-wire (&rest body)
  "Run BODY through real client acquisition with only wire effects replaced."
  (declare (indent 0) (debug t))
  `(let* ((before (buffer-list))
          (instance '("Ownership" . "http://ownership.invalid"))
          (hermes-instances (list instance))
          (hermes-instance instance)
          (hermes-dashboard-transport-idle-close-delay 0)
          (hermes-dashboard-stale-refresh-interval nil)
          (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
          rpc rest)
     (save-window-excursion
       (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                  (lambda (&rest _)
                    (make-hermes-dashboard-transport-client
                     :base-url (cdr instance))))
                 ((symbol-function 'hermes-dashboard-transport-request)
                  (lambda (_client method params resolve reject)
                    (push (list method params resolve reject) rpc)))
                 ((symbol-function 'hermes-dashboard-transport-api-request-async)
                  (lambda (method path &rest args)
                    (let ((promise (hermes--promise-make)))
                      (push (list method path args promise) rest)
                      promise))))
         (unwind-protect (progn ,@body)
           (dolist (buffer (seq-difference (buffer-list) before))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))))))

(defun hermes-buffer-test--draft ()
  "Replace the current buffer with a local editable draft."
  (read-only-mode -1)
  (erase-buffer)
  (buffer-enable-undo)
  (setq buffer-undo-list nil)
  (insert "Local unsaved draft λ\n")
  (setq header-line-format "Local draft header"))

(defun hermes-buffer-test--view-snapshot (buffer)
  "Return BUFFER's editing and presentation state, including its lifetime."
  (should (buffer-live-p buffer))
  (with-current-buffer buffer
    (append (hermes-buffer-test--snapshot)
            (list buffer header-line-format buffer-read-only (point)))))

(ert-deftest hermes-buffer-session-actions-target-only-owned-detail ()
  "Public Rename/Delete follow owned details, not their display names."
  (dolist (state '(plain same-mode mode-reset associated detached))
    (hermes-buffer-test--with-wire
      (call-interactively #'hermes-list-sessions)
      (funcall (nth 2 (car rpc))
               '((sessions . (((id . "s1") (profile . "p1") (title . "Old"))))))
      (goto-char (point-min))
      (let* ((browser (current-buffer))
             (name "*Hermes Session: p1/s1*")
             (collision
              (if (memq state '(mode-reset associated detached))
                  (progn
                    (call-interactively #'hermes-sessions-view)
                    (hermes--promise-resolve (nth 3 (car rest))
                                             '((session_id . "s1") (messages . nil) (count . 0)
                                     (pagination . ((offset . 0) (limit . 500) (order . "oldest") (returned . 0)))))
                    (current-buffer))
                (generate-new-buffer name))))
        (with-current-buffer collision
          (pcase state
            ('same-mode (hermes-session-detail-mode))
            ('mode-reset (fundamental-mode) (hermes-session-detail-mode))
            ((or 'associated 'detached)
             (set-visited-file-name
              (expand-file-name "detail-draft" temporary-file-directory) t)
             (when (eq state 'detached) (set-visited-file-name nil t))
             (rename-buffer name)))
          (hermes-browser--own-instance instance)
          (hermes-buffer-test--draft))
        (let ((snapshot (hermes-buffer-test--view-snapshot collision)))
          (with-current-buffer browser (call-interactively #'hermes-sessions-view))
          (hermes--promise-resolve (nth 3 (car rest))
                                   '((session_id . "s1") (messages . nil) (count . 0)
                                     (pagination . ((offset . 0) (limit . 500) (order . "oldest") (returned . 0)))))
          (let ((detail (window-buffer (selected-window))))
            (should-not (eq detail collision))
            (should-not (equal (buffer-name detail) name))
            ;; A user rename also must not lose the logical view identity.
            (with-current-buffer detail (rename-buffer "Owned session detail" t))
            (with-current-buffer browser
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) "Renamed")))
                (call-interactively #'hermes-sessions-rename)))
            (should (equal (caar rest) "PATCH"))
            (hermes--promise-resolve (nth 3 (car rest)) '((ok . t)))
            (should (equal snapshot (hermes-buffer-test--view-snapshot collision)))
            (should (string-match-p
                     "Session: Renamed" (with-current-buffer detail (buffer-string))))
            (with-current-buffer browser
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (call-interactively #'hermes-sessions-delete)))
            (should (equal (caar rest) "DELETE"))
            (hermes--promise-resolve (nth 3 (car rest)) '((ok . t)))
            (should-not (buffer-live-p detail))
            (should (equal snapshot
                           (hermes-buffer-test--view-snapshot collision)))))))))

(ert-deftest hermes-buffer-dashboard-auth-requires-current-claim ()
  "Late authentication cannot publish into a retired view or its successor."
  (dolist (detach '(nil t))
    (hermes-buffer-test--with-wire
      (cl-letf (((symbol-function 'hermes-dashboard--warm-profile-cache) #'ignore)
                ((symbol-function 'keymap-popup) #'ignore))
        (call-interactively #'hermes)
        (hermes-dashboard--provider-auth-changed)
        (should (equal (caar rpc) "setup.runtime_check"))
        (let ((retired (current-buffer))
              (resolve (nth 2 (car rpc))))
          (set-visited-file-name
           (expand-file-name "dashboard-draft" temporary-file-directory) t)
          (when detach (set-visited-file-name nil t))
          (hermes-buffer-test--draft)
          (let ((snapshot (hermes-buffer-test--view-snapshot retired)))
            (call-interactively #'hermes)
            (let* ((successor (current-buffer))
                   (successor-snapshot (hermes-buffer-test--view-snapshot successor)))
              (should-not (eq successor retired))
              (funcall resolve '((ok . nil)))
              (should (equal snapshot
                             (hermes-buffer-test--view-snapshot retired)))
              (should (equal successor-snapshot
                             (hermes-buffer-test--view-snapshot successor)))
              (should-not hermes-dashboard--needs-onboarding)
              ;; The successor's own real auth-change path still renders.
              (hermes-dashboard--provider-auth-changed)
              (funcall (nth 2 (car rpc)) '((ok . nil)))
              (should (string-match-p "Connect a provider" (buffer-string)))
              (should (equal snapshot
                             (hermes-buffer-test--view-snapshot retired)))
              (call-interactively #'hermes)
              (should (eq successor (current-buffer))))))))))

(ert-deftest hermes-buffer-retired-cron-resize-preserves-draft ()
  (dolist (detach '(t nil))
    (hermes-buffer-test--with-wire
      (let ((hermes-cron-auto-refresh-interval nil))
        (delete-other-windows)
        (call-interactively #'hermes-list-crons)
        (let ((callback (car window-size-change-functions)))
          (should (eq callback 'hermes-cron--window-size-change))
          (set-visited-file-name
           (expand-file-name "cron-draft" temporary-file-directory) t)
          (when detach (set-visited-file-name nil t))
          (hermes-buffer-test--draft)
          (let ((snapshot (hermes-buffer-test--view-snapshot (current-buffer))))
            ;; Deliver the actual installed callback with actual resized geometry.
            (split-window-right)
            (funcall callback (selected-window))
            (should (equal snapshot
                           (hermes-buffer-test--view-snapshot (current-buffer))))))))))

(ert-deftest hermes-buffer-retired-kanban-resize-preserves-draft ()
  (dolist (detach '(t nil))
    (hermes-buffer-test--with-wire
      (delete-other-windows)
      (call-interactively #'hermes-kanban-boards)
      (set-buffer (window-buffer (selected-window)))
      (let ((callback (car window-size-change-functions)))
        (should (eq callback 'hermes-kanban--window-size-change))
        (set-visited-file-name
         (expand-file-name "kanban-draft" temporary-file-directory) t)
        (when detach (set-visited-file-name nil t))
        (hermes-buffer-test--draft)
        (let ((snapshot (hermes-buffer-test--view-snapshot (current-buffer))))
          (split-window-right)
          (funcall callback (selected-window))
          (should (equal snapshot
                         (hermes-buffer-test--view-snapshot (current-buffer)))))))))

(ert-deftest hermes-buffer-current-cron-resize-still-updates ()
  (hermes-buffer-test--with-wire
    (let ((hermes-cron-auto-refresh-interval nil))
      (delete-other-windows)
      (call-interactively #'hermes-list-crons)
      (let ((format-before tabulated-list-format)
            (callback (car window-size-change-functions)))
        (split-window-right)
        (funcall callback (selected-window))
        (should-not (equal format-before tabulated-list-format))))))

(ert-deftest hermes-buffer-cron-retirement-stops-owned-timer ()
  "File association cancels the timer and fences its retained delivery."
  (dolist (detach '(nil t))
    (hermes-buffer-test--with-wire
      (let ((hermes-cron-auto-refresh-interval 600))
        (call-interactively #'hermes-list-crons)
        (let ((timer hermes-cron--auto-refresh-timer))
          (should (memq timer timer-list))
          (set-visited-file-name
           (expand-file-name "cron-timer-draft" temporary-file-directory) t)
          (when detach (set-visited-file-name nil t))
          (should-not (memq timer timer-list))
          (should-not hermes-cron--auto-refresh-timer)
          (hermes-buffer-test--draft)
          (goto-char 4)
          (setq hermes-browser--status "Local draft status")
          (let ((count (length rest))
                (snapshot (hermes-buffer-test--view-snapshot (current-buffer))))
            (apply (timer--function timer) (timer--args timer))
            (should (equal snapshot (hermes-buffer-test--view-snapshot (current-buffer))))
            (should (equal hermes-browser--status "Local draft status"))
            (should (= count (length rest)))))))))

(ert-deftest hermes-buffer-close-preserves-unowned-same-mode-namesake ()
  (hermes-buffer-test--with-wire
    (let ((collision (generate-new-buffer "*Hermes Sessions*")))
      (with-current-buffer collision
        (hermes-sessions-mode)
        (hermes-buffer-test--draft))
      (call-interactively #'hermes-list-sessions)
      (should-not (eq collision (current-buffer)))
      (let ((view (current-buffer))
            (snapshot (hermes-buffer-test--view-snapshot collision)))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (call-interactively #'hermes-close))
        (should-not (buffer-live-p view))
        (should (equal snapshot (hermes-buffer-test--view-snapshot collision)))))))

(ert-deftest hermes-buffer-current-kanban-resize-still-updates ()
  "Current custom boards and task views still resize their actual columns."
  (hermes-buffer-test--with-wire
    (dolist (open (list #'hermes-kanban-boards
                       (lambda () (hermes-kanban-open-board-task "default" "task"))))
      (delete-other-windows)
      (funcall open)
      (set-buffer (window-buffer (selected-window)))
      (let ((format-before tabulated-list-format)
            (callback (car window-size-change-functions)))
        (split-window-right)
        (funcall callback (selected-window))
        (should-not (equal format-before tabulated-list-format))))))

(ert-deftest hermes-buffer-cron-timer-refresh-and-replacement ()
  "A current timer refreshes; a replaced timer cannot borrow its successor."
  (hermes-buffer-test--with-wire
    (let ((hermes-cron-auto-refresh-interval 600))
      (call-interactively #'hermes-list-crons)
      (let ((old hermes-cron--auto-refresh-timer)
            (buffer (current-buffer)))
        (apply (timer--function old) (timer--args old))
        (should (= (length rest) 2))
        (hermes--promise-resolve (nth 3 (car rest)) '(:jobs nil))
        (should (equal hermes-browser--status "Empty"))
        (should (eq buffer (hermes-buffer--get "*Hermes Cron*" #'hermes-cron-mode t)))
        (let ((new hermes-cron--auto-refresh-timer))
          (should-not (eq old new))
          (should-not (memq old timer-list))
          (should (memq new timer-list))
          (apply (timer--function old) (timer--args old))
          (should (= (length rest) 2))
          (apply (timer--function new) (timer--args new))
          (should (= (length rest) 3))
          (hermes--promise-reject (nth 3 (car rest)) "offline")
          (should-not (equal hermes-browser--status "Loading")))))))

(ert-deftest hermes-buffer-close-preserves-retirement-and-successors ()
  "Close rechecks exact claims after its recursive confirmation."
  (dolist (retire '(associated detached mode-reset successor))
    (hermes-buffer-test--with-wire
      (call-interactively #'hermes-list-sessions)
      (let ((old (current-buffer)) snapshot)
        ;; A renamed owned buffer is still included in close.
        (rename-buffer "Owned renamed sessions" t)
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _)
                     (with-current-buffer old
                       (pcase retire
                         ((or 'associated 'detached)
                          (set-visited-file-name
                           (expand-file-name "close-draft" temporary-file-directory) t)
                          (when (eq retire 'detached) (set-visited-file-name nil t)))
                         ('mode-reset (hermes-sessions-mode))
                         ('successor
                          (hermes-buffer--get "*Hermes Sessions*" #'hermes-sessions-mode t)))
                       (hermes-buffer-test--draft)
                       (setq snapshot (hermes-buffer-test--view-snapshot old)))
                     t)))
          (call-interactively #'hermes-close))
        (should (equal snapshot (hermes-buffer-test--view-snapshot old))))))
  (hermes-buffer-test--with-wire
    (call-interactively #'hermes-list-sessions)
    (let ((owned (current-buffer)))
      (rename-buffer "Owned renamed sessions" t)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (call-interactively #'hermes-close))
      (should-not (buffer-live-p owned)))))

(ert-deftest hermes-buffer-close-current-chats-and-unique-viewers ()
  "Fresh/resumed chats and constructed unique viewers retain close semantics."
  (hermes-buffer-test--with-wire
    (let* ((chat (hermes-chat--new-buffer))
           (resumed (hermes-chat-resume-session "saved-session"))
           (viewer (progn (call-interactively #'hermes-files) (current-buffer)))
           (unowned (generate-new-buffer " *unowned chat*")))
      (with-current-buffer unowned
        (hermes-chat-mode)
        (goto-char (point-max))
        (insert "Local unsaved chat draft"))
      (let ((snapshot (hermes-buffer-test--view-snapshot unowned)))
        (with-current-buffer viewer
          (hermes--promise-resolve (nth 3 (car rest))
                                  '(:path "/remote" :parent nil :entries
                                    ((:name "file" :path "/remote/file" :is_directory :false :size 1))))
          (goto-char (point-min))
          (call-interactively #'hermes-files-open))
        (let ((completed (window-buffer (selected-window))))
          (hermes--promise-resolve
           (nth 3 (car rest))
           '(:path "/remote/file" :size 1 :data_url "data:text/plain;base64,eA=="))
          (with-current-buffer viewer (call-interactively #'hermes-files-open))
          (let ((pending (window-buffer (selected-window))))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (call-interactively #'hermes-close))
            (dolist (buffer (list chat resumed viewer completed pending))
              (should-not (buffer-live-p buffer)))
            (should (equal snapshot (hermes-buffer-test--view-snapshot unowned)))))))))

(ert-deftest hermes-buffer-close-preserves-retired-chat-companions ()
  "Chat cleanup must not kill task drafts or repaint detached work drafts."
  (dolist (kind '(todos work))
    (dolist (detach '(nil t))
      (hermes-buffer-test--with-wire
        (let* ((chat (hermes-chat--new-buffer))
               (owner (list :buffer chat :current-p (lambda (_) t)
                            :view nil :view-valid-p nil :render nil))
               panel)
          (with-current-buffer chat
            (if (eq kind 'todos)
                (setq panel (hermes-chat-show-todos))
              (setq hermes-chat--work-owner owner)
              (call-interactively #'hermes-chat-work)
              (setq panel (plist-get owner :view))))
          (with-current-buffer panel
            (set-visited-file-name
             (expand-file-name "companion-draft" temporary-file-directory) t)
            (when detach (set-visited-file-name nil t))
            (let ((inhibit-read-only t)) (hermes-buffer-test--draft)))
          (let ((snapshot (hermes-buffer-test--view-snapshot panel)))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (call-interactively #'hermes-close))
            ;; Deliver the same detached renderer scheduled by chat teardown.
            (when (eq kind 'work) (funcall (plist-get owner :render) owner))
            (should-not (buffer-live-p chat))
            (should (equal snapshot (hermes-buffer-test--view-snapshot panel)))))))))

(ert-deftest hermes-buffer-help-views-preserve-namesakes ()
  "Native help renders into the owned object, including after user rename."
  (dolist (spec '((hermes-chat-session-details "*Hermes Session Details*")
                  (hermes-work-scope-details "*Hermes Work Scope*")))
    (dolist (state '(plain help file))
      (hermes-buffer-test--with-wire
        (let* ((chat (hermes-chat--new-buffer))
               (command (car spec)) (name (cadr spec))
               (collision (generate-new-buffer name)))
          (with-current-buffer collision
            (when (eq state 'help) (help-mode))
            (when (eq state 'file)
              (set-visited-file-name (expand-file-name "help-notes" temporary-file-directory) t)
              (rename-buffer name))
            (hermes-buffer-test--draft))
          (let ((snapshot (hermes-buffer-test--view-snapshot collision)))
            (with-current-buffer chat (call-interactively command))
            (let ((view (hermes-buffer--find name 'help-mode)))
              (should (buffer-live-p view))
              (should-not (eq view collision))
              (should (get-buffer-window view))
              (with-current-buffer chat (call-interactively command))
              (should (eq view (hermes-buffer--find name 'help-mode)))
              (with-current-buffer view
                (should (eq (key-binding (kbd "q")) #'quit-window))
                (when (eq command 'hermes-chat-session-details)
                  (should (commandp (key-binding (kbd "g"))))))
              (with-current-buffer view (rename-buffer "Renamed owned help" t))
              (with-current-buffer chat (call-interactively command))
              (should (eq view (hermes-buffer--find name 'help-mode)))
              (should (equal snapshot (hermes-buffer-test--view-snapshot collision))))))))))

(ert-deftest hermes-buffer-image-reopen-preserves-retired-records ()
  "Reopening recovery transfers exact records, never the retired draft surface."
  (dolist (state '(associated detached mode))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (hermes-buffer-test--with-wire
        (let* ((chat (hermes-chat--new-buffer))
               (bytes (unibyte-string 137 80 78 71 13 10 26 10 1 2 3))
               (panel (with-current-buffer chat
                        (hermes-chat--image-stage bytes)
                        (call-interactively #'hermes-chat-preview-images)
                        (buffer-local-value 'hermes-chat--image-recovery-buffer chat)))
               (records (buffer-local-value 'hermes-chat--image-records panel)))
          (with-current-buffer chat (call-interactively #'hermes-chat-preview-images))
          (should (eq panel (buffer-local-value 'hermes-chat--image-recovery-buffer chat)))
          (with-current-buffer panel
            (if (eq state 'mode) (fundamental-mode)
              (set-visited-file-name (expand-file-name "image-notes" temporary-file-directory) t)
              (when (eq state 'detached) (set-visited-file-name nil t)))
            (hermes-buffer-test--draft))
          (let ((snapshot (hermes-buffer-test--view-snapshot panel)))
            (with-current-buffer chat (call-interactively #'hermes-chat-preview-images))
            (let ((next (buffer-local-value 'hermes-chat--image-recovery-buffer chat)))
              (should-not (eq next panel))
              (with-current-buffer next
                (should (hermes-buffer--owned-p 'hermes-chat-image-recovery-mode))
                (should (eq records hermes-chat--image-records))
                (should (equal bytes (plist-get (car (plist-get (car records) :images)) :bytes)))
                (should (string-match-p "image/png" (buffer-string))))
              (with-current-buffer chat (call-interactively #'hermes-chat-preview-images))
              (should (eq next (buffer-local-value 'hermes-chat--image-recovery-buffer chat)))
              (with-current-buffer panel
                (should-error (hermes-chat-image-recovery-refresh) :type 'user-error))
              (should (equal snapshot (hermes-buffer-test--view-snapshot panel)))
              ;; Native global close may retain current recovery bytes; it must
              ;; never kill or repaint the retired notes companion.
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (call-interactively #'hermes-close))
              (should (equal snapshot (hermes-buffer-test--view-snapshot panel))))))))))

(defmacro hermes-buffer-test--local-views (&rest body)
  "Run BODY with isolated image locks, disposing only newly created buffers."
  (declare (indent 0) (debug t))
  `(let ((before (buffer-list))
         (hermes-chat--image-session-blocks (make-hash-table :test #'equal)))
     (save-window-excursion
       (unwind-protect (progn ,@body)
         (dolist (buffer (seq-difference (buffer-list) before))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setq hermes-chat--image-records nil
                     kill-buffer-query-functions nil)
               (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(defun hermes-buffer-test--retire-view (buffer state)
  "Retire BUFFER by native file association or mode reset according to STATE."
  (with-current-buffer buffer
    (if (eq state 'mode-reset) (funcall major-mode)
      (set-visited-file-name
       (expand-file-name "retired-companion-notes" temporary-file-directory) t)
      (when (eq state 'detached) (set-visited-file-name nil t)))
    (read-only-mode -1)
    (erase-buffer)
    (insert "Retired companion notes")))

(ert-deftest hermes-buffer-queue-public-actions-require-current-claim ()
  "Every queue action refuses retired views; all current actions still work."
  (dolist (state '(nil associated detached mode-reset))
    (dolist (key '("g" "e" "r" "u" "d" "D"))
      (hermes-buffer-test--local-views
        (let* ((chat (hermes-chat--new-buffer))
               (entries (list (list :id 'first :content "First" :rejected-p t)
                              (list :id 'second :content "Second")))
               (panel (with-current-buffer chat
                        (setq hermes-chat--queued-messages entries)
                        (hermes-chat-queue-panel)))
               (original (copy-tree entries))
               (reads 0) (submits 0)
               (hermes-chat--queue-drain-ready-function (lambda () t))
               (hermes-chat--submit-function (lambda (&rest _) (cl-incf submits))))
          (pop-to-buffer panel)
          (when (equal key "u") (forward-line))
          (let ((command (key-binding (kbd key))))
            (should (commandp command))
            (when state (hermes-buffer-test--retire-view panel state))
            (cl-letf (((symbol-function 'read-string-from-buffer)
                       (lambda (&rest _) (cl-incf reads) "Edited")))
              (if state
                  (should-error (call-interactively command) :type 'user-error)
                ;; Exercise g through the actual keyboard dispatcher too.
                (if (equal key "g") (execute-kbd-macro (kbd key))
                  (call-interactively command)))))
          (if state
              (progn
                (with-current-buffer chat (hermes-chat--queue-panel-refresh-if-live))
                (should (equal (buffer-string) "Retired companion notes"))
                (should (equal original (buffer-local-value 'hermes-chat--queued-messages chat)))
                (should (zerop reads)) (should (zerop submits)))
            (with-current-buffer chat
              (pcase key
                ("g" (should (equal original hermes-chat--queued-messages)))
                ("e" (should (equal "Edited" (plist-get (car hermes-chat--queued-messages) :content))))
                ("r" (should (= submits 1)))
                ((or "u" "d") (should (eq 'second (plist-get (car hermes-chat--queued-messages) :id))))
                ("D" (should (equal (cdr original) hermes-chat--queued-messages)))))))))))

(ert-deftest hermes-buffer-queue-quit-retains-view ()
  "The inherited window dismissal remains harmless for current and retired views."
  (dolist (state '(nil associated detached mode-reset))
    (hermes-buffer-test--local-views
      (let* ((chat (hermes-chat--new-buffer))
             (panel (with-current-buffer chat (hermes-chat-queue-panel))))
        (pop-to-buffer panel)
        (when state (hermes-buffer-test--retire-view panel state))
        (let ((text (buffer-string)))
          (call-interactively (key-binding (kbd "q")))
          (should (buffer-live-p panel))
          (should (equal text (with-current-buffer panel (buffer-string)))))))))

(ert-deftest hermes-buffer-queue-edit-retains-original-claim ()
  "An edit cannot outlive association, mode reset, or a same-buffer reopen."
  (dolist (state '(associated detached mode-reset reopen))
    (hermes-buffer-test--local-views
      (let* ((chat (hermes-chat--new-buffer))
             (entries (list (list :id 'first :content "Original")))
             (panel (with-current-buffer chat
                      (setq hermes-chat--queued-messages entries)
                      (hermes-chat-queue-panel)))
             (claim (buffer-local-value 'hermes-buffer--owner panel))
             (reads 0))
        (with-current-buffer panel
          (cl-letf (((symbol-function 'read-string-from-buffer)
                     (lambda (&rest _)
                       (cl-incf reads)
                       (if (eq state 'reopen)
                           (with-current-buffer chat (hermes-chat-queue-panel))
                         (hermes-buffer-test--retire-view panel state))
                       "Must not replace queued input")))
            (should-error (call-interactively (key-binding (kbd "e"))) :type 'user-error)))
        (should (= reads 1))
        (should (equal "Original" (plist-get (car (buffer-local-value 'hermes-chat--queued-messages chat)) :content)))
        (if (eq state 'reopen)
            (should-not (eq claim (buffer-local-value 'hermes-buffer--owner panel)))
          (should (equal "Retired companion notes" (with-current-buffer panel (buffer-string)))))))))

(defun hermes-buffer-test--recovery-fixture ()
  "Return a chat, its recovery panel and an unowned accepted record."
  (let ((chat (hermes-chat--new-buffer)))
    (with-current-buffer chat
      (hermes-chat--image-stage (unibyte-string 137 80 78 71 13 10 26 10 1 2 3))
      (let ((record hermes-chat--image-draft-record))
        (setq hermes-chat--draft-images nil hermes-chat--image-draft-record nil)
        (setf (plist-get record :state) 'accepted
              (plist-get record :content) "Recovered text"
              (plist-get record :session-key) 'test)
        (puthash 'test record hermes-chat--image-session-blocks)
        (list chat hermes-chat--image-recovery-buffer record)))))

(ert-deftest hermes-buffer-recovery-readers-retain-original-claim ()
  "Every restore/discard continuation refuses retirement and data transfer."
  (dolist (key '("r" "d"))
    (dolist (boundary (if (equal key "r") '(selection target confirmation)
                       '(selection confirmation)))
      (dolist (state '(associated detached mode-reset transfer))
        (hermes-buffer-test--local-views
          (pcase-let* ((`(,chat ,panel ,record) (hermes-buffer-test--recovery-fixture))
                       (target (hermes-chat--new-buffer))
                       (bytes (copy-sequence (plist-get (car (plist-get record :images)) :bytes)))
                       (seen nil) (successor nil))
            (cl-labels
                ((reader (stage answer)
                   (push stage seen)
                   (when (eq stage boundary)
                     (hermes-buffer-test--retire-view
                      panel (if (eq state 'transfer) 'detached state))
                     (when (eq state 'transfer)
                       (with-current-buffer chat
                         (setq successor (hermes-chat--image-recovery)))))
                   answer))
              (with-current-buffer panel
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (prompt &rest _)
                             (if (string-prefix-p "Image record" prompt)
                                 (reader 'selection "1")
                               (reader 'target (buffer-name target)))))
                          ((symbol-function 'yes-or-no-p)
                           (lambda (&rest _) (reader 'confirmation t))))
                  (should-error (call-interactively (key-binding (kbd key))) :type 'user-error))))
            (should (eq (car seen) boundary))
            (should (eq record (gethash 'test hermes-chat--image-session-blocks)))
            (should (equal bytes (plist-get (car (plist-get record :images)) :bytes)))
            (with-current-buffer target
              (should-not hermes-chat--draft-images)
              (should (string-empty-p (hermes-chat-input-string))))
            (with-current-buffer panel
              (should (equal "Retired companion notes" (buffer-string))))
            (if successor
                (progn
                  (with-current-buffer panel
                    (should-not hermes-chat--image-records)
                    (hermes-chat--image-recovery-killed))
                  (should (eq record (gethash 'test hermes-chat--image-session-blocks)))
                  (with-current-buffer successor
                    (should (eq record (car hermes-chat--image-records)))
                    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "1"))
                              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                      (call-interactively (key-binding (kbd "d"))))
                    (should-not hermes-chat--image-records))
                  (should (equal '(:state uncertain) (gethash 'test hermes-chat--image-session-blocks))))
              (should (eq record (car (buffer-local-value 'hermes-chat--image-records panel)))))))))))

(ert-deftest hermes-buffer-recovery-current-public-actions ()
  "Refresh, restore, discard and quit retain current native view semantics."
  (hermes-buffer-test--local-views
    (pcase-let* ((`(,_chat ,panel ,record) (hermes-buffer-test--recovery-fixture))
                 (target (hermes-chat--new-buffer)))
      (pop-to-buffer panel)
      (execute-kbd-macro (kbd "g"))
      (should (string-match-p "Recovered text" (buffer-string)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _)
                   (if (string-prefix-p "Image record" prompt) "1" (buffer-name target))))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (call-interactively (key-binding (kbd "r")))
        (with-current-buffer target
          (should (equal "Recovered text" (hermes-chat-input-string)))
          (should (equal (plist-get record :images) hermes-chat--draft-images)))
        (with-current-buffer panel
          (call-interactively (key-binding (kbd "d")))
          (should-not hermes-chat--image-records)))
      (should (equal '(:state uncertain) (gethash 'test hermes-chat--image-session-blocks)))
      (pop-to-buffer panel)
      (call-interactively (key-binding (kbd "q")))
      (should (buffer-live-p panel)))))

(ert-deftest hermes-buffer-recovery-kill-query-retains-original-claim ()
  "Kill confirmation cannot dispose bytes after retirement or source transfer."
  (dolist (state '(nil associated detached mode-reset transfer))
    (hermes-buffer-test--local-views
      (pcase-let* ((`(,chat ,panel ,record) (hermes-buffer-test--recovery-fixture))
                   (successor nil))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _)
                     (when state
                       (hermes-buffer-test--retire-view
                        panel (if (eq state 'transfer) 'detached state))
                       (when (eq state 'transfer)
                         (with-current-buffer chat (setq successor (hermes-chat--image-recovery)))))
                     t)))
          (if state (should-not (kill-buffer panel))
            (should (kill-buffer panel))))
        (if state
            (progn
              (should (buffer-live-p panel))
              (should (eq record (gethash 'test hermes-chat--image-session-blocks)))
              (should (eq record (car (buffer-local-value 'hermes-chat--image-records (or successor panel))))))
          (should (equal '(:state uncertain) (gethash 'test hermes-chat--image-session-blocks))))))))

(provide 'hermes-buffer-tests)
;;; hermes-buffer-tests.el ends here
