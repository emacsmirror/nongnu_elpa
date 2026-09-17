;;; hermes-gnosis-tests.el --- Optional practice completion tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Disposable origin and delayed wire tests; no learner database is opened.

;;; Code:

(require 'hermes-test-helpers)
(require 'sqlite)
(require 'websocket)

(ert-deftest hermes-gnosis-queued-wire-is-explicit-and-ordinary-unchanged ()
  (let ((client (hermes-test--dashboard-client)) frames)
    (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
              ((symbol-function 'websocket-send-text)
               (lambda (_socket text) (push (json-parse-string text :object-type 'alist) frames))))
      (unwind-protect
          (progn
            (hermes-dashboard-transport-prompt-submit
             client "ordinary" :session-id "runtime")
            (hermes-dashboard-transport-prompt-submit
             client "[Gnosis application event; not learner-authored]"
             :session-id "runtime" :queued t)
            (should (= 2 (length frames)))
            (should (eq t (alist-get 'queued (alist-get 'params (car frames)))))
            (should-not (assq 'queued (alist-get 'params (cadr frames)))))
        (hermes-dashboard-transport--reject-pending-requests client "Test cleanup")))))


(require 'hermes-gnosis)

(defmacro hermes-gnosis-test-with-origin (&rest body)
  "Run BODY with a real disposable SQLite origin and an attached chat."
  (declare (indent 0))
  `(let* ((file (make-temp-file "hermes-gnosis-"))
          (db (sqlite-open file))
          (hermes-gnosis-mode t)
          (hermes-gnosis--bindings nil)
          (hermes-chat--image-prior-submits (make-hash-table :test #'equal))
          (hermes-chat--image-session-blocks (make-hash-table :test #'equal))
          (status "completed") frames)
     (unwind-protect
         (cl-letf (((symbol-function 'gnosis-agent-status)
                    (lambda (id)
                      (list :api-version 1 :mode "practice" :session-id id
                            :database (nth 2 (assoc 0 (sqlite-select gnosis-db "PRAGMA database_list")))
                            :status status)))
                   ((symbol-function 'gnosis-agent-results)
                    (lambda (id) (append (gnosis-agent-status id)
                                         (list :batch id :connection gnosis-db))))
                   ((symbol-function 'websocket-openp) (lambda (_) t))
                   ((symbol-function 'websocket-close) #'ignore)
                   ((symbol-function 'websocket-send-text)
                    (lambda (_socket text)
                      (push (json-parse-string text :object-type 'alist) frames))))
           (hermes-test-with-chat-buffer
             (let ((client (hermes-test--dashboard-client)))
               (setf (hermes-dashboard-transport-client-ready-p client) t
                     (hermes-dashboard-transport-client-base-url client) "http://fixture.invalid")
               (setq hermes-chat--dashboard-client client
                     hermes-chat--dashboard-session-ready-p t
                     hermes-chat--dashboard-active-session-id "runtime"
                     hermes-chat--session-id "stored"
                     hermes-chat--profile (copy-sequence "study")
                     hermes-chat--resolved-start-mode 'remote)
               (unwind-protect
                   (progn ,@body)
                 (hermes-dashboard-transport--reject-pending-requests client "Test cleanup")))))
       (ignore-errors (sqlite-close db))
       (delete-file file))))

(defun hermes-gnosis-test-event (db)
  "Return the exact completion event from DB."
  (list :api-version 1 :mode "practice" :session-id "batch"
        :database (nth 2 (assoc 0 (sqlite-select db "PRAGMA database_list")))
        :connection db))

(ert-deftest hermes-gnosis-completion-real-pipeline-preserves-draft-and-once ()
  (hermes-gnosis-test-with-origin
    (buffer-enable-undo)
    (insert "private draft α\nsecond line")
    (goto-char (+ hermes-chat--input-marker 8))
    (let* ((images (list (list :bytes (unibyte-string 0 1 255))))
           (draft (hermes-chat-input-string))
           (offset (- (point) hermes-chat--input-marker))
           (undo buffer-undo-list)
           (handle (hermes-gnosis-bind "batch" db (current-buffer))))
      (setq hermes-chat--draft-images images)
      (hermes-gnosis--completed (hermes-gnosis-test-event db))
      (should (= 1 (length frames)))
      (let ((params (alist-get 'params (car frames))))
        (should (eq t (alist-get 'queued params)))
        (should (equal "runtime" (alist-get 'session_id params)))
        (should (string-search "not learner-authored" (alist-get 'text params)))
        (should (string-search handle (alist-get 'text params)))
        (should-not (string-search file (alist-get 'text params))))
      (should (eq 'application (plist-get (car (hermes-chat--entries)) :role)))
      (should-not (string-search "> [Gnosis" (buffer-string)))
      (should (equal draft (hermes-chat-input-string)))
      (should (= offset (- (point) hermes-chat--input-marker)))
      (should (eq undo buffer-undo-list))
      (should (eq images hermes-chat--draft-images))
      (should-not hermes-chat--queued-messages)
      (should (eq db (plist-get (hermes-gnosis-results handle) :connection)))
      ;; Lost receipts and recompletion do not grant another automatic attempt.
      (hermes-dashboard-transport--reject-pending-requests client "Lost receipt")
      (hermes-gnosis--completed (hermes-gnosis-test-event db))
      (should (= 1 (length frames))))))

(ert-deftest hermes-gnosis-busy-and-image-owners-have-bounded-manual-fallback ()
  (dolist (condition '(running fifo staged uncertain))
    (hermes-gnosis-test-with-origin
      (hermes-gnosis-bind "batch" db (current-buffer))
      (insert "keep")
      (pcase condition
        ('running (setq hermes-chat--dashboard-running-p t))
        ('fifo (setq hermes-chat--queued-messages (list (list :id 7 :content "user queue"))))
        ('staged (puthash (hermes-chat--image-session-key) '(:state uncertain)
                         hermes-chat--image-session-blocks))
        ('uncertain (puthash (hermes-chat--image-session-key) '(uncertain)
                            hermes-chat--image-prior-submits)))
      (let ((queue hermes-chat--queued-messages))
        (hermes-gnosis--completed (hermes-gnosis-test-event db))
        (hermes-gnosis--completed (hermes-gnosis-test-event db))
        (should-not frames)
        (should (eq queue hermes-chat--queued-messages))
        (should (equal "keep" (hermes-chat-input-string)))
        (should (= 1 (length (hermes-chat--entries))))
        (should (string-search "hermes-gnosis-copy-notice" (buffer-string)))))))

(ert-deftest hermes-gnosis-exact-database-not-current-or-same-batch ()
  (hermes-gnosis-test-with-origin
    (let* ((other-file (make-temp-file "hermes-other-"))
           (other (sqlite-open other-file))
           (handle (hermes-gnosis-bind "batch" db (current-buffer))))
      (unwind-protect
          (let ((gnosis-db other))
            (hermes-gnosis--completed (hermes-gnosis-test-event other))
            (should-not frames)
            (should (eq db (plist-get (hermes-gnosis-results handle) :connection)))
            (sqlite-close db)
            (should-error (hermes-gnosis-results handle))
            (should-error (hermes-gnosis-bind "batch" nil (current-buffer)))
            (should-not frames))
        (sqlite-close other)
        (delete-file other-file)))))

(ert-deftest hermes-gnosis-stale-owners-cannot-deliver-or-read ()
  (dolist (change '(stop disconnect client socket runtime stored profile endpoint claim file))
    (hermes-gnosis-test-with-origin
      (let ((handle (hermes-gnosis-bind "batch" db (current-buffer))))
        (pcase change
          ('stop (setq hermes-chat--interrupted-assistant-id "stopped")
                 (hermes-chat--notify-state-change)
                 (setq hermes-chat--interrupted-assistant-id nil))
          ('disconnect (hermes-chat--invalidate-transport-state))
          ('client (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)))
          ('socket (setf (hermes-dashboard-transport-client-websocket client) 'replacement))
          ('runtime (setq hermes-chat--dashboard-active-session-id "replacement"))
          ('stored (setq hermes-chat--session-id "replacement"))
          ('profile (aset hermes-chat--profile 0 ?X))
          ('endpoint (setf (hermes-dashboard-transport-client-base-url client) "http://other.invalid"))
          ('claim (setq hermes-buffer--owner (copy-tree hermes-buffer--owner)))
          ('file (set-visited-file-name (concat file ".notes") t)
                 (set-visited-file-name nil t)))
        (should-error (hermes-gnosis-results handle))
        (hermes-gnosis--completed (hermes-gnosis-test-event db))
        (should-not frames)))))

(ert-deftest hermes-gnosis-unfinished-cancelled-and-disabled-are-inert ()
  (hermes-gnosis-test-with-origin
    (let ((handle (hermes-gnosis-bind "batch" db (current-buffer))))
      (dolist (state '("unfinished" "cancelled" "running"))
        (setq status state)
        (hermes-gnosis--completed (hermes-gnosis-test-event db)))
      (should-not frames)
      (should-not (plist-get hermes-gnosis--binding :attempted))
      (hermes-gnosis-mode -1)
      (setq status "completed")
      (hermes-gnosis--completed (hermes-gnosis-test-event db))
      (should-not frames)
      (should-error (hermes-gnosis-results handle)))))

(ert-deftest hermes-gnosis-owner-retired-during-render-does-not-send ()
  (hermes-gnosis-test-with-origin
    (hermes-gnosis-bind "batch" db (current-buffer))
    (add-hook 'hermes-chat-state-change-hook #'hermes-gnosis--retire nil t)
    (hermes-gnosis--completed (hermes-gnosis-test-event db))
    (should-not frames)))

(ert-deftest hermes-gnosis-optional-enable-fails-cleanly-without-peer ()
  (let ((hermes-gnosis-mode nil)
        (gnosis-practice-completed-hook nil)
        (original (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest args)
                 (unless (eq feature 'gnosis-agent)
                   (apply original feature args)))))
      (should-error (hermes-gnosis-mode 1) :type 'user-error)
      (should-not hermes-gnosis-mode)
      (should-not gnosis-practice-completed-hook))))


(defun hermes-gnosis-test-reply (client frame result)
  "Deliver RESULT through CLIENT's real decoder for retained request FRAME."
  (hermes-dashboard-transport--handle-frame
   client (hermes-dashboard-transport--encode-frame
           `((jsonrpc . "2.0") (id . ,(alist-get 'id frame)) (result . ,result)))))

(ert-deftest hermes-gnosis-delayed-queued-receipt-retains-literal-application ()
  (hermes-gnosis-test-with-origin
    (hermes-gnosis-bind "batch" db (current-buffer))
    (insert "newer unsent input")
    (hermes-gnosis--completed (hermes-gnosis-test-event db))
    (let ((frame (car frames))
          (text (plist-get (car (hermes-chat--entries)) :content)))
      (should (equal "prompt.submit" (alist-get 'method frame)))
      ;; Backend policy has already queued this instead of redirecting a turn.
      (hermes-gnosis-test-reply client frame '((status . "queued")))
      (should-not hermes-chat--unsettled-submit-context)
      (should hermes-chat--server-queued-assistant-id)
      (should (equal text (plist-get (car (hermes-chat--entries)) :content)))
      (should (eq 'application (plist-get (car (hermes-chat--entries)) :role)))
      (should (equal "newer unsent input" (hermes-chat-input-string)))
      (hermes-gnosis--completed (hermes-gnosis-test-event db))
      (should (= 1 (length frames))))))

(ert-deftest hermes-gnosis-native-stop-retires-handle-before-completion ()
  (hermes-gnosis-test-with-origin
    (hermes-chat--submit-content "synthetic ordinary turn")
    (hermes-gnosis-test-reply client (car frames) '((status . "streaming")))
    (let ((handle (hermes-gnosis-bind "batch" db (current-buffer))))
      (hermes-chat-interrupt)
      (should (equal "session.interrupt" (alist-get 'method (car frames))))
      (hermes-gnosis--completed (hermes-gnosis-test-event db))
      (should (= 2 (length frames)))
      (should-error (hermes-gnosis-results handle)))))

(ert-deftest hermes-gnosis-replacement-and-manual-copy-never-reuse-old-handle ()
  (hermes-gnosis-test-with-origin
    (let ((old (hermes-gnosis-bind "batch" db (current-buffer)))
          (kill-ring nil))
      (insert "unsent")
      (hermes-gnosis-unbind)
      (hermes-gnosis-bind "batch" db (current-buffer))
      (should-error (hermes-gnosis-results old))
      (call-interactively #'hermes-gnosis-copy-notice)
      (should (string-search "not learner-authored" (car kill-ring)))
      (should-not (string-search old (car kill-ring)))
      (should (equal "unsent" (hermes-chat-input-string)))
      (should-not frames))))

(ert-deftest hermes-gnosis-closed-origin-during-render-is-not-submitted ()
  (hermes-gnosis-test-with-origin
    (hermes-gnosis-bind "batch" db (current-buffer))
    (let ((closer (lambda () (ignore-errors (sqlite-close db)))))
      (add-hook 'hermes-chat-state-change-hook closer nil t)
      (unwind-protect
          (hermes-gnosis--completed (hermes-gnosis-test-event db))
        (remove-hook 'hermes-chat-state-change-hook closer t)))
    (should-not frames)))

(ert-deftest hermes-gnosis-disable-does-not-strand-already-submitted-turn ()
  (hermes-gnosis-test-with-origin
    (hermes-gnosis-bind "batch" db (current-buffer))
    (hermes-gnosis--completed (hermes-gnosis-test-event db))
    (hermes-gnosis-mode -1)
    (hermes-gnosis-test-reply client (car frames) '((status . "streaming")))
    (hermes-dashboard-transport--dispatch-event
     client '(:type done :event "message.complete" :status done :session-id "runtime"))
    (should-not hermes-chat--unsettled-submit-context)
    (should-not hermes-chat--pending-assistant-id)
    (should-not hermes-gnosis--bindings)))

(ert-deftest hermes-gnosis-file-retirement-fences-late-receipt-and-stream ()
  (hermes-gnosis-test-with-origin
    (hermes-gnosis-bind "batch" db (current-buffer))
    (hermes-gnosis--completed (hermes-gnosis-test-event db))
    (let ((callback (plist-get
                     (gethash hermes-chat--dashboard-token
                              (hermes-dashboard-transport-client-subscribers client)) :fn)))
      (should (functionp callback))
      (set-visited-file-name (concat file ".notes") t)
      (set-visited-file-name nil t)
      (let ((before (buffer-string)))
        (hermes-gnosis-test-reply client (car frames) '((status . "queued")))
        (funcall callback '(:type text :content "late output" :session-id "runtime"))
        (should (equal before (buffer-string)))))))


(ert-deftest hermes-gnosis-narrowed-draft-and-restored-label-remain-literal ()
  (hermes-gnosis-test-with-origin
    (hermes-gnosis-bind "batch" db (current-buffer))
    (insert "prefix narrow draft suffix")
    (narrow-to-region (+ hermes-chat--input-marker 7) (- (point-max) 7))
    (goto-char (+ (point-min) 2))
    (let ((text (buffer-string))
          (offset (- (point) hermes-chat--input-marker)))
      (hermes-gnosis--completed (hermes-gnosis-test-event db))
      (should (= 1 (length frames)))
      (should (equal text (buffer-string)))
      (should (= offset (- (point) hermes-chat--input-marker)))
      (let* ((wire (alist-get 'text (alist-get 'params (car frames))))
             (entry (hermes-chat--history-entry `((role . "user") (text . ,wire)))))
        (should (eq 'user (plist-get entry :role)))
        (should (equal wire (plist-get entry :content)))))))

(ert-deftest hermes-gnosis-same-live-binding-preserves-completion-attempt ()
  (hermes-gnosis-test-with-origin
    (let* ((handle (hermes-gnosis-bind "batch" db (current-buffer)))
           (binding hermes-gnosis--binding))
      (should (equal handle (hermes-gnosis-bind "batch" db (current-buffer))))
      (should (eq binding hermes-gnosis--binding))
      (hermes-gnosis--completed (hermes-gnosis-test-event db))
      (hermes-gnosis-test-reply client (car frames) '((status . "streaming")))
      (hermes-dashboard-transport--dispatch-event
       client '(:type done :event "message.complete" :status done :session-id "runtime"))
      (should-not (hermes-chat--active-turn-p))
      (should (equal handle (hermes-gnosis-bind "batch" db (current-buffer))))
      (should (eq binding hermes-gnosis--binding))
      (should (plist-get binding :attempted))
      (hermes-gnosis--completed (hermes-gnosis-test-event db))
      (should (= 1 (length frames)))
      (should (eq db (plist-get (hermes-gnosis-results handle) :connection))))))

(ert-deftest hermes-gnosis-origin-or-destination-replacement-revokes-handle ()
  (dolist (change '(connection runtime))
    (hermes-gnosis-test-with-origin
      (let* ((old (hermes-gnosis-bind "batch" db (current-buffer)))
             (binding hermes-gnosis--binding)
             ;; The same filename does not make a new SQLite object the old origin.
             (other (and (eq change 'connection) (sqlite-open file))))
        (unwind-protect
            (progn
              (when (eq change 'runtime)
                (setq hermes-chat--dashboard-active-session-id "replacement"))
              (let ((new (hermes-gnosis-bind "batch" (or other db) (current-buffer))))
                (should-not (equal old new))
                (should (plist-get binding :retired))
                (should-error (hermes-gnosis-results old))
                (should (eq (or other db)
                            (plist-get (hermes-gnosis-results new) :connection)))))
          (when other (sqlite-close other)))))))

(ert-deftest hermes-gnosis-deferred-dispatch-rechecks-completed-origin ()
  (dolist (next-status '("unfinished" "cancelled" "completed"))
    (hermes-gnosis-test-with-origin
      (hermes-gnosis-bind "batch" db (current-buffer))
      ;; A failed create override can remain pending on an attached ready chat.
      ;; Its real config.set receipt defers prompt submission without retirement.
      (setq hermes-chat--create-overrides-retry-session-id "runtime"
            hermes-chat--dashboard-create-reasoning-effort "low")
      (insert "keep draft")
      (hermes-gnosis--completed (hermes-gnosis-test-event db))
      (should (= 1 (length frames)))
      (should (equal "config.set" (alist-get 'method (car frames))))
      (should (hermes-gnosis--current-p hermes-gnosis--binding))
      (setq status next-status)
      (hermes-gnosis-test-reply client (car frames) '((ok . t)))
      (if (equal next-status "completed")
          (progn
            (should (= 2 (length frames)))
            (should (equal "prompt.submit" (alist-get 'method (car frames))))
            (should (eq t (alist-get 'queued (alist-get 'params (car frames))))))
        (should (= 1 (length frames)))
        (should-not hermes-chat--unsettled-submit-context)
        (should-not (hermes-chat--active-turn-p)))
      (should (equal "keep draft" (hermes-chat-input-string)))
      (should (plist-get hermes-gnosis--binding :attempted))
      (setq status "completed")
      (hermes-gnosis--completed (hermes-gnosis-test-event db))
      (should (= (if (equal next-status "completed") 2 1) (length frames))))))

(ert-deftest hermes-gnosis-origin-read-quit-settles-application-prompt ()
  (dolist (deferred '(nil t))
    (hermes-gnosis-test-with-origin
      (hermes-gnosis-bind "batch" db (current-buffer))
      (insert "keep draft")
      (when deferred
        (setq hermes-chat--create-overrides-retry-session-id "runtime"
              hermes-chat--dashboard-create-reasoning-effort "low"))
      (let ((original (symbol-function 'gnosis-agent-status)) (reads 0))
        (cl-letf (((symbol-function 'gnosis-agent-status)
                   (lambda (id)
                     (if (and (not deferred) (= (cl-incf reads) 2))
                         (signal 'quit nil)
                       (funcall original id)))))
          (hermes-gnosis--completed (hermes-gnosis-test-event db))))
      (when deferred
        (should (equal "config.set" (alist-get 'method (car frames))))
        (cl-letf (((symbol-function 'gnosis-agent-status)
                   (lambda (_) (signal 'quit nil))))
          (condition-case nil
              (hermes-gnosis-test-reply client (car frames) '((ok . t)))
            (quit nil))))
      (should (= (if deferred 1 0) (length frames)))
      (should-not hermes-chat--unsettled-submit-context)
      (should-not (hermes-chat--active-turn-p))
      (should (equal "keep draft" (hermes-chat-input-string)))
      (should (plist-get hermes-gnosis--binding :attempted)))))

(provide 'hermes-gnosis-tests)
;;; hermes-gnosis-tests.el ends here
