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
          (status "completed") (mode "practice") study frames)
     (unwind-protect
         (cl-letf (((symbol-function 'gnosis-agent-status)
                    (lambda (id)
                      (list :api-version 1 :mode mode :session-id id
                            :database (nth 2 (assoc 0 (sqlite-select gnosis-db "PRAGMA database_list")))
                            :status status)))
                   ((symbol-function 'gnosis-agent-results)
                    (lambda (id) (append (gnosis-agent-status id)
                                         (list :batch id :connection gnosis-db :study study))))
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

(ert-deftest hermes-gnosis-application-observer-retains-terminal-before-receipt ()
  (hermes-gnosis-test-with-origin
    (let (observed)
      (insert "draft stays")
      (hermes-chat--submit-content
       "application" nil nil (lambda () t)
       (lambda (_context kind payload) (push (cons kind payload) observed)))
      (let ((frame (car frames)))
        (hermes-dashboard-transport--dispatch-event
         client '(:type done :event "message.complete" :status "completed"
                        :session-id "runtime" :final-text "exact α\n"))
        (should-not observed)
        (hermes-gnosis-test-reply client frame '((status . "streaming")))
        (hermes-test--wait-until (lambda () (= 2 (length observed))))
        (should (equal (mapcar #'car (reverse observed)) '(admitted terminal)))
        (should (equal (plist-get (cdar observed) :final-text) "exact α\n"))
        (should (equal "draft stays" (hermes-chat-input-string)))))))

(defun hermes-gnosis-test-work-frames (frames)
  "Return FRAMES excluding independent native session title refreshes."
  (seq-remove (lambda (frame) (equal (alist-get 'method frame) "session.title")) frames))

(defun hermes-gnosis-test-request (&optional phase)
  "Return an allocated contextual request for PHASE."
  (list :api-version 1 :session-id "batch" :request-id "request"
        :phase (or phase "evaluate") :occurrence-id "occurrence" :revision 3
        :goal "Finite synthetic review" :source "Synthetic source"
        :context nil :transcript [] :question '(:id "q" :question "Which quantity?")
        :response "idk" :accepted nil :remaining []))

(defun hermes-gnosis-test-tick (tutor)
  "Run TUTOR's scheduled application boundary deterministically."
  (hermes-test--event-loop-barrier)
  (when (timerp (plist-get tutor :timer)) (cancel-timer (plist-get tutor :timer)))
  (hermes-gnosis--tutor-pump tutor))

(defun hermes-gnosis-test-terminal (client request result &optional status)
  "Deliver a real normalized terminal from CLIENT for REQUEST and RESULT.
Use STATUS, or the installed backend's explicit successful status."
  (hermes-dashboard-transport--handle-frame
   client (hermes-dashboard-transport--encode-frame
           `((jsonrpc . "2.0") (method . "event")
             (params . ((type . "message.complete") (session_id . "runtime")
                        (payload . ((status . ,(or status "complete"))
                                    (text . ,(hermes-gnosis--json
                                              (hermes-gnosis--envelope request result)))))))))))

(ert-deftest hermes-gnosis-tutor-prewarm-early-response-and-accepted-adaptation ()
  (hermes-gnosis-test-with-origin
    (setq mode "agent-review" status "running")
    (let* ((tutor (hermes-gnosis--tutor (current-buffer)))
           (init (hermes-gnosis-test-request "initialize"))
           (request (hermes-gnosis-test-request)) resolved rejected)
      (hermes-gnosis--tutor-attach tutor "batch" db)
      (setf (plist-get tutor :initialize) (hermes-gnosis--tutor-op init nil nil))
      (insert "untouched tutor draft")
      (hermes-gnosis-test-tick tutor)
      (let ((init-frame (car (hermes-gnosis-test-work-frames frames))))
        (should (string-search "Initialize once" (alist-get 'text (alist-get 'params init-frame))))
        (should-error (hermes-chat-send) :type 'user-error)
        (hermes-gnosis--tutor-provider tutor request (lambda (x) (push x resolved))
                                      (lambda (x) (push x rejected)))
        (hermes-gnosis-test-tick tutor)
        (should (= 1 (length (hermes-gnosis-test-work-frames frames))))
        (hermes-gnosis-test-terminal client init '(:ready t))
        (hermes-gnosis-test-tick tutor)
        (should (= 1 (length (hermes-gnosis-test-work-frames frames))))
        (hermes-gnosis-test-reply client init-frame '((status . "streaming")))
        (hermes-gnosis-test-tick tutor)
        (should (= 2 (length (hermes-gnosis-test-work-frames frames))))
        (should-not (string-search "Initialize once" (alist-get 'text (alist-get 'params (car (hermes-gnosis-test-work-frames frames))))))
        (hermes-gnosis-test-reply client (car (hermes-gnosis-test-work-frames frames)) '((status . "streaming")))
        (hermes-gnosis-test-terminal client request '(:verdict "fail" :explanation "Define the missing quantity."))
        (hermes-gnosis-test-tick tutor)
        (should (= 1 (length resolved)))
        (should (= 2 (length (hermes-gnosis-test-work-frames frames))))
        (let ((adapt (hermes-gnosis-test-request "adapt")))
          (setf (plist-get adapt :request-id) "adapt"
                (plist-get adapt :accepted) '(:outcome "success" :override t))
          (hermes-gnosis--tutor-provider tutor adapt (lambda (x) (push x resolved))
                                        (lambda (x) (push x rejected)))
          (hermes-gnosis-test-tick tutor)
          (should (= 3 (length (hermes-gnosis-test-work-frames frames))))
          (should (string-search "\"override\":true" (alist-get 'text (alist-get 'params (car (hermes-gnosis-test-work-frames frames))))))
          (hermes-gnosis-test-reply client (car (hermes-gnosis-test-work-frames frames)) '((status . "streaming")))
          (hermes-gnosis-test-terminal client adapt '(:agent-note "Finite block done" :context nil :remaining [] :done t))
          (hermes-gnosis-test-tick tutor)
          (should (= 2 (length resolved))))
        (should-not rejected)
        (should (equal "stored" hermes-chat--session-id))
        (should (equal "untouched tutor draft" (hermes-chat-input-string)))))))

(ert-deftest hermes-gnosis-tutor-envelope-and-explicit-success-are-required ()
  (let* ((request (hermes-gnosis-test-request))
         (result '(:verdict "pass" :explanation "Correct"))
         (event (list :event "message.complete" :type 'done :status "complete"
                      :final-text (hermes-gnosis--json (hermes-gnosis--envelope request result)))))
    (should (equal result (hermes-gnosis--result request event)))
    (dolist (key '(:api-version :session-id :request-id :phase :occurrence-id :revision))
      (let ((wrong (copy-tree request)))
        (setf (plist-get wrong key) "wrong")
        (should-error (hermes-gnosis--result wrong event))))
    (dolist (status '(nil "done" "error" "interrupted" "incomplete"))
      (let ((bad (copy-tree event)))
        (setf (plist-get bad :status) status)
        (should-error (hermes-gnosis--result request bad))))
    (should-error (hermes-gnosis--parse "{\"result\":{},\"result\":{}}"))
    (should-error (hermes-gnosis--parse "{} trailing"))))

(ert-deftest hermes-gnosis-tutor-cancelled-inflight-retry-reuses-exact-result ()
  (hermes-gnosis-test-with-origin
    (setq mode "agent-review" status "running")
    (let* ((tutor (hermes-gnosis--tutor (current-buffer)))
           (request (hermes-gnosis-test-request)) resolved)
      (hermes-gnosis--tutor-attach tutor "batch" db)
      (setf (plist-get tutor :state) 'ready)
      (let ((cancel (hermes-gnosis--tutor-provider tutor request (lambda (x) (push x resolved)) #'ignore)))
        (hermes-gnosis-test-tick tutor)
        (funcall cancel)
        (funcall cancel)
        (hermes-gnosis-test-reply client (car (hermes-gnosis-test-work-frames frames)) '((status . "streaming")))
        (hermes-gnosis-test-terminal client request '(:verdict "fail" :explanation "Teaching"))
        (hermes-gnosis-test-tick tutor)
        (should-not resolved)
        (let ((retry (copy-tree request)))
          (setf (plist-get retry :request-id) "retry" (plist-get retry :revision) 5)
          (hermes-gnosis--tutor-provider tutor retry (lambda (x) (push x resolved)) #'ignore)
          (hermes-gnosis-test-tick tutor)
          (should (= 1 (length resolved)))
          (should (= 1 (length (hermes-gnosis-test-work-frames frames)))))))))

(ert-deftest hermes-gnosis-application-observer-queued-prior-terminal-is-not-result ()
  (hermes-gnosis-test-with-origin
    (let (events)
      (hermes-chat--submit-content "application" nil nil (lambda () t)
                                   (lambda (_context kind payload) (push (cons kind payload) events)))
      (let ((frame (car (hermes-gnosis-test-work-frames frames))))
        (hermes-dashboard-transport--dispatch-event
         client '(:type done :event "message.complete" :status "complete" :session-id "runtime" :final-text "prior"))
        (hermes-gnosis-test-reply client frame '((status . "queued")))
        (hermes-test--wait-until (lambda () events))
        (should (equal (mapcar #'car events) '(admitted)))
        (hermes-dashboard-transport--dispatch-event
         client '(:type status :event "session.info" :status-key "session.info" :session-id "runtime" :running nil))
        (hermes-dashboard-transport--dispatch-event
         client '(:type status :event "message.start" :status-key "message.start" :status "started" :session-id "runtime"))
        (hermes-dashboard-transport--dispatch-event
         client '(:type done :event "message.complete" :status "complete" :session-id "runtime" :final-text "owned"))
        (hermes-test--wait-until (lambda () (= 2 (length events))))
        (should (equal (mapcar #'car events) '(terminal admitted)))
        (should (equal (plist-get (cdar events) :final-text) "owned"))))))

(ert-deftest hermes-gnosis-tutor-replayed-terminal-recovers-without-inference ()
  (hermes-gnosis-test-with-origin
    (setq mode "agent-review" status "unfinished")
    (let* ((tutor (hermes-gnosis--tutor (current-buffer)))
           (request (hermes-gnosis-test-request)) resolved)
      (hermes-gnosis--tutor-attach tutor "batch" db)
      (setf (plist-get tutor :state) 'ready)
      (hermes-gnosis--recover-turn tutor request)
      (should (equal "session.events.since" (alist-get 'method (car (hermes-gnosis-test-work-frames frames)))))
      (hermes-gnosis-test-reply
       client (car (hermes-gnosis-test-work-frames frames))
       `((count . 1) (events . [((type . "message.complete") (session_id . "runtime")
                                (payload . ((status . "complete")
                                            (text . ,(hermes-gnosis--json (hermes-gnosis--envelope
                                                                         request '(:verdict "pass" :explanation "Correct")))))))])))
      (hermes-gnosis-test-tick tutor)
      (let ((retry (copy-tree request)))
        (setf (plist-get retry :request-id) "retry" (plist-get retry :revision) 8)
        (hermes-gnosis--tutor-provider tutor retry (lambda (x) (push x resolved)) #'ignore)
        (hermes-gnosis-test-tick tutor)
        (should (= 1 (length resolved)))
        (should (= 1 (length (hermes-gnosis-test-work-frames frames))))))))

(ert-deftest hermes-gnosis-tutor-missing-receipt-remains-explicit-not-resend ()
  (hermes-gnosis-test-with-origin
    (setq mode "agent-review" status "unfinished")
    (let ((tutor (hermes-gnosis--tutor (current-buffer))))
      (hermes-gnosis--tutor-attach tutor "batch" db)
      (setf (plist-get tutor :state) 'ready)
      (hermes-gnosis--recover-turn tutor (hermes-gnosis-test-request))
      (hermes-gnosis-test-reply client (car (hermes-gnosis-test-work-frames frames)) '((count . 0) (events . [])))
      (should (eq 'failed (plist-get tutor :state)))
      (should-error (hermes-gnosis--tutor-provider tutor (hermes-gnosis-test-request) #'ignore #'ignore))
      (should (= 1 (length (hermes-gnosis-test-work-frames frames)))))))

(ert-deftest hermes-gnosis-tutor-inflight-resume-observes-future-normal-terminal ()
  (hermes-gnosis-test-with-origin
    (setq mode "agent-review" status "unfinished")
    (let* ((tutor (hermes-gnosis--tutor (current-buffer)))
           (request (hermes-gnosis-test-request)))
      (hermes-gnosis--tutor-attach tutor "batch" db)
      (setf (plist-get tutor :state) 'ready)
      (hermes-chat--dashboard-restore-inflight-turn client)
      (hermes-chat--dashboard-bind-stream-callback client hermes-chat--pending-assistant-id)
      (hermes-gnosis--recover-turn tutor request)
      (hermes-gnosis-test-reply client (car (hermes-gnosis-test-work-frames frames)) '((count . 0) (events . [])))
      (should-not (eq 'failed (plist-get tutor :state)))
      (hermes-gnosis-test-terminal client request '(:verdict "pass" :explanation "Correct"))
      (hermes-gnosis-test-tick tutor)
      (should (plist-get tutor :recovery))
      (should (= 1 (length (hermes-gnosis-test-work-frames frames)))))))


(ert-deftest hermes-gnosis-resume-validates-normal-history-and-checkpoint ()
  (dolist (case '(settled recovered pending wrong-origin unknown-admission incomplete
                         completed completed-notice completed-wrong-origin))
    (hermes-gnosis-test-with-origin
      (setq mode "agent-review" status (if (memq case '(completed completed-notice completed-wrong-origin))
                                          "completed" "unfinished"))
      (let* ((real-require (symbol-function 'require))
             (request (hermes-gnosis-test-request))
             (init (hermes-gnosis-test-request "initialize"))
             (binding (hermes-gnosis--binding "batch" db)) bound resumed)
        (setf (plist-get init :request-id) "initialize"
              (plist-get init :questions) (vector (plist-get request :question)))
        (setq study (list :goal (plist-get init :goal) :source (plist-get init :source)
                          :questions (plist-get init :questions) :phase "question"
                          :current (list :id "q" :occurrence "occurrence" :response "idk")
                          :attempts (vector (list :request-id (if (eq case 'unknown-admission) "unseen" "request")
                                                 :evaluation (and (memq case '(settled completed)) '(:verdict "pass"))))))
        (when (eq case 'recovered)
          (setf (plist-get study :attempts)
                (vconcat (plist-get study :attempts)
                         (vector '(:request-id "local-retry" :evaluation (:verdict "pass"))))))
        (let* ((first (hermes-gnosis--tutor-prompt binding init))
               (last (hermes-gnosis--tutor-prompt binding request))
               (history `((session_id . "runtime") (stored_session_id . "stored")
                          (message_count . ,(if (eq case 'incomplete) 3 2))
                          (running . ,(and (eq case 'pending) t))
                          (messages . (((role . "user") (text . ,first))
                                       ((role . "user") (text . ,last)))))))
          (when (eq case 'completed-notice)
            (setf (alist-get 'message_count history) 3
                  (alist-get 'messages history)
                  (append (alist-get 'messages history)
                          (list `((role . "user") (text . ,(hermes-gnosis--notice binding)))))))
          (hermes-chat--restore-session-history client nil history)
          ;; Normal hydration may issue its independent goal/status read.
          (setq frames nil))
        (when (memq case '(wrong-origin completed-wrong-origin))
          (setq hermes-chat--session-id "different-durable-session"))
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &rest args)
                     (if (eq feature 'gnosis-agent-review) t
                       (apply real-require feature args))))
                  ((symbol-function 'gnosis-agent-review-bind-provider)
                   (lambda (id connection function)
                     (should (equal id "batch")) (should (eq connection db))
                     (should (functionp function)) (setq bound t) #'ignore))
                  ((symbol-function 'gnosis-agent-resume)
                   (lambda (id) (setq resumed t) (gnosis-agent-status id))))
          (if (memq case '(wrong-origin completed-wrong-origin unknown-admission incomplete))
              (progn
                (should-error (hermes-gnosis-resume-review "batch" db (current-buffer)))
                (should-not bound) (should-not resumed) (should-not frames))
            (hermes-gnosis-resume-review "batch" db (current-buffer))
            (if (memq case '(completed completed-notice))
                (progn (should-not bound) (should-not resumed)
                       (should-not hermes-gnosis--binding))
              (should bound) (should resumed))
            (if (eq case 'pending)
                (should (equal (mapcar (lambda (f) (alist-get 'method f)) frames) '("session.events.since")))
              (should-not frames))))))))

(ert-deftest hermes-gnosis-tutor-busy-and-native-interaction-keep-one-pending ()
  (hermes-gnosis-test-with-origin
    (setq mode "agent-review" status "unfinished")
    (let ((tutor (hermes-gnosis--tutor (current-buffer))))
      (hermes-gnosis--tutor-attach tutor "batch" db)
      (setf (plist-get tutor :state) 'ready)
      (setq hermes-chat--dashboard-running-p t)
      (hermes-gnosis--tutor-provider tutor (hermes-gnosis-test-request) #'ignore #'ignore)
      (hermes-gnosis-test-tick tutor)
      (should-not frames)
      (should-error (hermes-gnosis--tutor-provider tutor (hermes-gnosis-test-request) #'ignore #'ignore))
      (cl-letf (((symbol-function 'hermes-chat--pending-prompt-p) (lambda () t)))
        (should-not (hermes-gnosis--tutor-inhibit)))
      (hermes-gnosis-unbind)
      (should-not (plist-get tutor :pending))
      (should-not (plist-get tutor :timer))
      (should-not frames))))

(ert-deftest hermes-gnosis-tutor-retirement-fences-deferred-terminal ()
  (hermes-gnosis-test-with-origin
    (setq mode "agent-review" status "unfinished")
    (let* ((tutor (hermes-gnosis--tutor (current-buffer)))
           (request (hermes-gnosis-test-request)) resolved rejected)
      (hermes-gnosis--tutor-attach tutor "batch" db)
      (setf (plist-get tutor :state) 'ready)
      (hermes-gnosis--tutor-provider tutor request (lambda (_) (setq resolved t)) (lambda (_) (setq rejected t)))
      (hermes-gnosis-test-tick tutor)
      (hermes-gnosis-test-reply client (car frames) '((status . "streaming")))
      (hermes-gnosis-test-terminal client request '(:verdict "pass" :explanation "Correct"))
      (hermes-gnosis-unbind)
      (hermes-test--event-loop-barrier)
      (should-not resolved)
      (should rejected))))

(ert-deftest hermes-gnosis-queued-terminal-handoff-receipt-orderings ()
  (dolist (receipt '(before-prior after-prior after-start after-owned))
    (hermes-gnosis-test-with-origin
      (let (events)
        (insert "unchanged draft")
        (hermes-chat--submit-content
         "application" nil nil (lambda () t)
         (lambda (_context kind payload) (push (cons kind payload) events)))
        (let ((frame (car frames))
              (assistant-id (plist-get hermes-chat--application-context :assistant-id)))
          (cl-labels ((admit () (hermes-gnosis-test-reply client frame '((status . "queued"))))
                      (terminal (text)
                        (hermes-dashboard-transport--dispatch-event
                         client (list :type 'done :event "message.complete" :status "complete"
                                      :session-id "runtime" :content text :final-text text))))
            (when (eq receipt 'before-prior) (admit))
            (terminal "prior")
            (when (eq receipt 'after-prior) (admit))
            (hermes-test--event-loop-barrier)
            (should-not (assq 'terminal events))
            (hermes-dashboard-transport--dispatch-event
             client '(:type status :event "message.start" :status-key "message.start"
                            :status "started" :session-id "runtime"))
            (when (eq receipt 'after-start) (admit))
            (hermes-test--event-loop-barrier)
            (should-not (assq 'terminal events))
            (terminal "owned")
            (when (eq receipt 'after-owned) (admit))
            (hermes-test--event-loop-barrier)
            (should (equal (mapcar #'car events) '(terminal admitted)))
            (should (equal (plist-get (cdar events) :final-text) "owned"))
            (should (equal (hermes-chat--entry-content-by-id assistant-id) "owned"))
            (should-not hermes-chat--application-context)
            (should-not hermes-chat--server-queued-assistant-id)
            (should-not hermes-chat--queued-messages)
            (should (equal "unchanged draft" (hermes-chat-input-string)))))))))

(ert-deftest hermes-gnosis-cancelled-evaluation-edited-retry-is-new-turn ()
  (dolist (completion '(before-retry after-retry uncertain))
    (hermes-gnosis-test-with-origin
      (setq mode "agent-review" status "running")
      (let* ((tutor (hermes-gnosis--tutor (current-buffer)))
             (request (hermes-gnosis-test-request))
             (retry (copy-tree request)) resolved rejected)
        (hermes-gnosis--tutor-attach tutor "batch" db)
        (setf (plist-get tutor :state) 'ready
              (plist-get retry :request-id) "edited"
              (plist-get retry :revision) 5
              (plist-get retry :response) "Edited answer")
        (let ((cancel (hermes-gnosis--tutor-provider tutor request #'ignore #'ignore)))
          (hermes-gnosis-test-tick tutor)
          (let* ((old (plist-get tutor :active))
                 (frame (car frames))
                 (old-result '(:verdict "fail" :explanation "Old answer feedback")))
            (unless (eq completion 'uncertain)
              (hermes-gnosis-test-reply client frame '((status . "streaming"))))
            (funcall cancel)
            (when (eq completion 'before-retry)
              (hermes-gnosis-test-terminal client request old-result)
              (hermes-gnosis-test-tick tutor))
            (hermes-gnosis--tutor-provider tutor retry
                                          (lambda (x) (push x resolved))
                                          (lambda (x) (push x rejected)))
            (hermes-gnosis-test-tick tutor)
            (unless (eq completion 'before-retry)
              (should (= 1 (length (hermes-gnosis-test-work-frames frames))))
              (hermes-gnosis-test-terminal client request old-result)
              (hermes-gnosis-test-tick tutor))
            (if (eq completion 'uncertain)
                (progn
                  (should (= 1 (length (hermes-gnosis-test-work-frames frames))))
                  (should-not resolved))
              (should-not rejected)
              (should-not resolved)
              (should (= 2 (length (hermes-gnosis-test-work-frames frames))))
              (should (string-search "Edited answer" (alist-get 'text (alist-get 'params (car frames)))))
              ;; An already scheduled callback from the cancelled operation
              ;; cannot settle its successor or copy the old grade into it.
              (hermes-gnosis--tutor-observe tutor old (plist-get old :context) 'rejected "late")
              (should-not (plist-get (plist-get tutor :active) :error))
              (hermes-gnosis-test-reply client (car frames) '((status . "streaming")))
              (hermes-gnosis-test-terminal client retry '(:verdict "pass" :explanation "Edited feedback"))
              (hermes-gnosis-test-tick tutor)
              (should (equal resolved '((:verdict "pass" :explanation "Edited feedback"))))
              (should (equal hermes-chat--session-id "stored")))))))))

(provide 'hermes-gnosis-tests)
;;; hermes-gnosis-tests.el ends here
