;;; hermes-chat-dashboard-tests.el --- shared-socket isolation tests for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `hermes-chat-dashboard' shared-socket runtime isolation: two
;; chat buffers on one dashboard client must keep their session identity,
;; subscriber tokens, create-time overrides, and reconnect resumes strictly
;; buffer-local.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(require 'hermes-subagents)

(ert-deftest hermes-chat-reasoning-cleanup-rejects-reentrant-old-event ()
  (hermes-test-with-chat-buffer
    (hermes-chat--insert-entry '(:id "a1" :role assistant :content "" :status streaming))
    (setq hermes-chat--pending-assistant-id "a1")
    (let ((callback (hermes-chat--transport-callback (current-buffer) "a1" nil hermes-chat--transport-generation))
          called)
      (funcall callback '(:type thinking :event "thinking.delta" :content "first"))
      (let ((hermes-chat-state-change-hook
             (list (lambda ()
                     (unless called
                       (setq called t)
                       (funcall callback '(:type thinking :event "thinking.delta" :content "late")))))))
        (hermes-chat--cleanup-buffer))
      (should called)
      (should-not hermes-chat--pending-assistant-id)
      (should-not (gethash "a1:activity" hermes-chat--nodes)))))

;;; Observed delegates

(defvar hermes-test--work-processes-p nil
  "Non-nil to exercise both observation sources rather than delegates alone.")

(defmacro hermes-test--with-process-wire (&rest body)
  "Run BODY through both real typed inventory wrappers and raw frames."
  (declare (indent 0) (debug t))
  `(let ((hermes-test--work-processes-p t))
     (hermes-test--with-work-wire ,@body)))

(defun hermes-test--work-answer (client json)
  "Answer CLIENT's current work request with serialized JSON."
  (hermes-test--work-reply
   client (plist-get (plist-get hermes-chat--work-owner :request) :id) json))

(ert-deftest hermes-chat-work-process-wire-and-exits ()
  "Scoped wire requests preserve exit evidence and replace disappeared rows."
  (hermes-test--with-process-wire
    (hermes-chat-work-refresh)
    (hermes-test--work-answer client "{\"active\":[]}")
    (let* ((owner hermes-chat--work-owner)
           (request (plist-get owner :request))
           (frame (hermes-dashboard-transport--decode-frame (car frames))))
      (should (equal (hermes-transport--get frame 'method) "process.list"))
      (should (equal (hermes-transport--get
                      (hermes-transport--get frame 'params) 'session_id) "runtime"))
      (should (= (hash-table-count (hermes-dashboard-transport-client-pending client)) 1))
      (should (= (car (plist-get
                       (gethash (plist-get request :id)
                                (hermes-dashboard-transport-client-pending client)) :timer)) 10))
      (hermes-test--work-answer
       client "{\"processes\":[{\"session_id\":\"run\",\"status\":\"running\"},{\"session_id\":\"ok\",\"status\":\"exited\",\"exit_code\":0},{\"session_id\":\"bad\",\"status\":\"exited\",\"exit_code\":7},{\"session_id\":\"missing\",\"status\":\"exited\"},{\"session_id\":\"unknown\",\"status\":\"future\"}]}")
      (let ((rows (plist-get (plist-get owner :processes) :rows)))
        (dolist (case '(("run" running) ("ok" done) ("bad" failed)
                        ("missing" unknown) ("unknown" unknown)))
          (should (eq (plist-get (seq-find (lambda (row)
                                            (equal (plist-get row :id) (car case))) rows)
                                 :state) (cadr case)))))
      (should (equal (substring-no-properties (hermes-chat--work-label nil))
                     "1 process ?"))
      (should-not (plist-get owner :request))
      (hermes-chat-work-refresh)
      (hermes-test--work-answer client "{\"active\":[]}")
      (hermes-test--work-answer client "{\"processes\":[]}")
      (should-not (plist-get (plist-get owner :processes) :rows))
      (should-not (hermes-chat--work-label nil))
      (should-not events)
      (should-not hermes-chat--pending-assistant-id))))

(ert-deftest hermes-chat-work-process-runtime-without-durable-key ()
  "Missing delegate authority cannot suppress explicitly runtime-scoped processes."
  (hermes-test--with-process-wire
    (setf (hermes-dashboard-transport-client-session-id client) "foreign-default")
    (hermes-chat--work-bind client "runtime" nil)
    (hermes-chat-work-refresh)
    (let ((frame (hermes-dashboard-transport--decode-frame (car frames))))
      (should (equal (hermes-transport--get frame 'method) "process.list"))
      (should (equal (hermes-transport--get
                      (hermes-transport--get frame 'params) 'session_id) "runtime")))
    (hermes-test--work-answer client "{\"processes\":[{\"session_id\":\"p\",\"status\":\"running\"}]}")
    (should-not (plist-get hermes-chat--work-owner :delegates))
    (should (equal (substring-no-properties (hermes-chat--work-label nil))
                   "1 process ?"))))

(ert-deftest hermes-chat-work-new-busy-session-adopts-late-key ()
  "Cadence obtains a late durable key without duplicate reads or turn mutation."
  (hermes-test--with-process-wire
    (hermes-chat--work-bind client "runtime" nil)
    (setq hermes-chat--pending-assistant-id "busy-turn"
          hermes-chat--dashboard-running-p t)
    (let ((owner hermes-chat--work-owner))
      (cl-labels
          ((title-frames ()
             (seq-filter
              (lambda (frame) (equal (hermes-transport--get frame 'method) "session.title"))
              (mapcar #'hermes-dashboard-transport--decode-frame frames)))
           (tick ()
             (let ((timer (cdr (plist-get owner :timer))))
               (should (= (car timer) 5))
               (apply (cadr timer) (nth 2 timer)))))
        (hermes-chat--work-refresh owner)
        (should (= (length (title-frames)) 1))
        (let* ((frame (car (title-frames)))
               (id (hermes-transport--get frame 'id))
               (pending (gethash id (hermes-dashboard-transport-client-pending client))))
          (should (equal (hermes-transport--get frame 'params) '((session_id . "runtime"))))
          (should (= (car (plist-get pending :timer)) 10))
          (hermes-test--work-answer client "{\"processes\":[]}")
          (tick)
          (should (= (length (title-frames)) 1))
          (hermes-test--work-answer client "{\"processes\":[]}")
          (hermes-test--work-reply client id "{\"session_key\":null}")
          (should-not (plist-get owner :key))
          (tick)
          (should (= (length (title-frames)) 2))
          (hermes-test--work-reply
           client (hermes-transport--get (car (title-frames)) 'id)
           "{\"session_key\":\"A\"}")
          (hermes-test--work-answer client "{\"processes\":[]}")
          (tick)
          (should (= (length (title-frames)) 2))
          (should (eq (plist-get (plist-get owner :request) :source) :delegates))
          (hermes-test--work-answer client "{\"active\":[]}")
          (should (eq (plist-get (plist-get owner :delegates) :coverage) 'current))
          (should (equal (plist-get owner :key) "A"))
          (should (equal hermes-chat--pending-assistant-id "busy-turn"))
          (should hermes-chat--dashboard-running-p)
          (should (eq owner hermes-chat--work-owner)))))))

(ert-deftest hermes-chat-work-process-deadline-and-rebind ()
  "A held process request times out, and an old process cannot mutate a rebind."
  (hermes-test--with-process-wire
    (hermes-chat-work-refresh)
    (hermes-test--work-answer client "{\"active\":[]}")
    (let* ((owner hermes-chat--work-owner)
           (id (plist-get (plist-get owner :request) :id))
           (timer (plist-get (gethash id (hermes-dashboard-transport-client-pending client)) :timer)))
      (should (= (car timer) 10))
      (apply (cadr timer) (nth 2 timer))
      (should (plist-get (plist-get owner :processes) :paused))
      (should-not (plist-get (plist-get owner :delegates) :paused))
      (should (plist-get owner :timer))
      (hermes-chat--work-bind client "runtime" "B")
      (hermes-chat-work-refresh)
      (let ((request (plist-get hermes-chat--work-owner :request)))
        (hermes-test--work-reply client id "{\"processes\":[{\"session_id\":\"old\",\"status\":\"running\"}]}")
        (should (eq request (plist-get hermes-chat--work-owner :request)))
        (should-not (plist-get hermes-chat--work-owner :processes))))
    (should-not events)))

(ert-deftest hermes-chat-work-process-synchronous-failure-and-publication-reentry ()
  "Synchronous process failure settles locally; UI refresh cannot fork the cycle."
  (hermes-test--with-process-wire
    (hermes-chat-work-refresh)
    (cl-letf (((symbol-function 'hermes-chat--notify-state-change)
               (lambda () (hermes-chat-work-refresh))))
      (let ((hermes-dashboard-transport-websocket-send-function
             (lambda (&rest _) (error "process send failed"))))
        (hermes-test--work-answer client "{\"active\":[]}")))
    (should (= (length frames) 1))
    (should-not (plist-get hermes-chat--work-owner :request))
    (should-not (plist-get hermes-chat--work-owner :cycle))
    (should (plist-get (plist-get hermes-chat--work-owner :processes) :paused))
    (should (plist-get hermes-chat--work-owner :timer))
    (should-not events)))

(ert-deftest hermes-chat-work-process-publication-nonlocal-exit ()
  "Hook errors and quits release only the publishing cycle for explicit retry."
  (dolist (source '(:delegates :processes))
    (dolist (failure '(error quit))
      (dolist (replace '(nil t))
        (hermes-test--with-process-wire
          (hermes-chat-work-refresh)
          (when (eq source :processes)
            (hermes-test--work-answer client "{\"active\":[]}"))
          (let ((owner hermes-chat--work-owner)
                successor request cycle signaled)
            (let ((hermes-chat-state-change-hook
                   (list (lambda ()
                           (when replace
                             (hermes-chat--work-bind client "runtime" "B")
                             (hermes-chat-work-refresh)
                             (setq successor hermes-chat--work-owner
                                   request (plist-get successor :request)
                                   cycle (plist-get successor :cycle)))
                           (setq signaled failure)
                           (signal failure '("Publication failed"))))))
              (condition-case condition
                  (hermes-test--work-answer
                   client (if (eq source :delegates)
                              "{\"active\":[]}" "{\"processes\":[]}"))
                ((error quit) (setq signaled (car condition)))))
            (should (eq signaled failure))
            (should-not (plist-get owner :request))
            (unless replace
              (should-not (plist-get owner :cycle)))
            (when replace
              (should (eq successor hermes-chat--work-owner))
              (should (eq request (plist-get successor :request)))
              (should (eq cycle (plist-get successor :cycle)))
              (should-not (plist-get successor :timer)))
            (let ((before (length frames)))
              (hermes-chat-work-refresh)
              (should (= (length frames) (+ before (if replace 0 1))))
              (should (plist-get hermes-chat--work-owner :request))
              (should (= (hash-table-count
                          (hermes-dashboard-transport-client-pending client)) 1))
              (hermes-chat-work-refresh)
              (should (= (length frames) (+ before (if replace 0 1)))))
            (hermes-test--work-answer client "{\"active\":[]}")
            (should (= (hash-table-count
                        (hermes-dashboard-transport-client-pending client)) 1))
            (hermes-test--work-answer client "{\"processes\":[]}")
            (should-not (plist-get hermes-chat--work-owner :cycle))
            (should-not (plist-get hermes-chat--work-owner :request))))))))

(ert-deftest hermes-chat-work-process-wire-shapes ()
  "Only actual process arrays establish observations; malformed rows pause."
  (hermes-test--with-process-wire
    (dolist (case '(("{}" stale) ("{\"processes\":null}" stale)
                    ("{\"processes\":false}" stale) ("{\"processes\":{}}" stale)
                    ("{\"processes\":1}" stale) ("{\"processes\":[null]}" partial)
                    ("{\"processes\":[{\"session_id\":\"dup\",\"status\":\"running\"},{\"session_id\":\"dup\",\"status\":\"running\"}]}" partial)))
      (hermes-chat-work-refresh)
      (hermes-test--work-answer client "{\"active\":[]}")
      (hermes-test--work-answer client (car case))
      (let ((source (plist-get hermes-chat--work-owner :processes)))
        (should (eq (plist-get source :coverage) (cadr case)))
        (should (plist-get source :paused))
        (should-not (plist-get source :rows)))
      (should (plist-get hermes-chat--work-owner :timer)))
    (should-not events)))

(ert-deftest hermes-chat-work-process-independent-errors ()
  "Either source may fail while the healthy stage keeps its cadence."
  (dolist (source '(:delegates :processes))
    (dolist (code '(-32601 4001 5010 -32603))
      (hermes-test--with-process-wire
        (hermes-chat-work-refresh)
        (when (eq source :processes)
          (hermes-test--work-answer client "{\"active\":[]}"))
        (let* ((owner hermes-chat--work-owner)
               (id (plist-get (plist-get owner :request) :id)))
          (hermes-dashboard-transport--handle-frame
           client (format "{\"jsonrpc\":\"2.0\",\"id\":%S,\"error\":{\"code\":%s,\"message\":\"private error\"}}" id code))
          (should (plist-get (plist-get owner source) :paused))
          (when (eq source :delegates)
            (hermes-test--work-answer client "{\"processes\":[]}"))
          (should (plist-get owner :timer))
          (hermes-chat--work-refresh owner)
          (should (eq (plist-get (plist-get owner :request) :source)
                      (if (eq source :delegates) :processes :delegates)))
          (hermes-test--work-answer client
                                   (if (eq source :delegates)
                                       "{\"processes\":[]}" "{\"active\":[]}"))
          (should-not (plist-get owner :request))
          (hermes-chat-work-refresh)
          (should (eq (plist-get (plist-get owner :request) :source) :delegates))
          (should-not (string-match-p "private error" (hermes-chat--work-details)))
          (should-not events))))))

(ert-deftest hermes-chat-work-process-timeout-coalesces ()
  "Delegate deadline advances exactly once; busy g and late replies cannot retry."
  (hermes-test--with-process-wire
    (hermes-chat-work-refresh)
    (let* ((owner hermes-chat--work-owner)
           (id (plist-get (plist-get owner :request) :id))
           (timer (plist-get (gethash id (hermes-dashboard-transport-client-pending client)) :timer)))
      (hermes-chat-work-refresh)
      (apply (cadr timer) (nth 2 timer))
      (should (eq (plist-get (plist-get owner :request) :source) :processes))
      (let ((request (plist-get owner :request)))
        (hermes-test--work-reply client id "{\"active\":[]}")
        (hermes-chat-work-refresh)
        (should (eq request (plist-get owner :request))))
      (hermes-test--work-answer client "{\"processes\":[]}")
      (should (plist-get (plist-get owner :delegates) :paused))
      (hermes-chat-work-refresh)
      (let ((request (plist-get owner :request)))
        (hermes-test--work-reply client id "{\"active\":[]}")
        (should (eq request (plist-get owner :request))))
      (should-not events))))

(ert-deftest hermes-chat-work-process-hidden-between-stages ()
  "Hiding on delegate publication suppresses the process send and cadence."
  (hermes-test--with-process-wire
    (hermes-chat-work-refresh)
    (cl-letf (((symbol-function 'hermes-chat--notify-state-change)
               (lambda () (switch-to-buffer (get-buffer-create " *hidden work*")))))
      (unwind-protect
          (hermes-test--work-answer client "{\"active\":[]}")
        (kill-buffer " *hidden work*")))
    (should (= (length frames) 1))
    (should-not (plist-get hermes-chat--work-owner :request))
    (should-not (plist-get hermes-chat--work-owner :timer))))

(defmacro hermes-test--with-work (&rest body)
  "Run BODY with a visible, bound chat and captured delegate completions."
  (declare (indent 0) (debug t))
  `(let ((client (hermes-test--dashboard-client))
         (source-eligible (symbol-function 'hermes-chat--work-source-eligible-p))
         calls timers)
     (setf (hermes-dashboard-transport-client-ready-p client) t)
     (cl-letf (((symbol-function 'hermes-chat--work-source-eligible-p)
                (lambda (owner source)
                  (and (or hermes-test--work-processes-p (eq source :delegates))
                       (funcall source-eligible owner source))))
               ((symbol-function 'hermes-dashboard-transport-delegation-status)
                (lambda (_client &rest args)
                  (push args calls)
                  (should (= hermes-dashboard-transport-request-timeout 10))
                  (should hermes-dashboard-transport-request-lossless-result)
                  (length calls)))
               ((symbol-function 'run-at-time)
                (lambda (delay _repeat fn &rest args)
                  (let ((timer (list delay fn args)))
                    (push timer timers) timer)))
               ((symbol-function 'cancel-timer) #'ignore))
       (save-window-excursion
         (hermes-test-with-chat-buffer
          (switch-to-buffer (current-buffer))
          (setq hermes-chat--dashboard-client client
                hermes-chat--dashboard-active-session-id "runtime"
                hermes-chat--dashboard-session-ready-p t)
          (hermes-chat--work-bind client "runtime" "A")
          ,@body)))))

(ert-deftest hermes-chat-work-refinement-popup-workers ()
  "Inspect renders current delegate counts without treating processes as workers."
  (hermes-test--with-work
    (let* ((groups (apply #'append (keymap-popup--meta hermes-chat-actions-map 'descriptions)))
           (inspect (seq-find (lambda (g) (equal (plist-get g :name) "Inspect")) groups))
           (entry (seq-find (lambda (e) (equal (plist-get e :key) "W"))
                            (plist-get inspect :entries)))
           (description (plist-get entry :description)))
      (should entry)
      (should (functionp description))
      (should (= 1 (seq-count
                    (lambda (e) (equal (plist-get e :key) "W"))
                    (apply #'append (mapcar (lambda (g) (plist-get g :entries)) groups)))))
      (should (equal (funcall description) "Workers 0 · unknown"))
      (setf (plist-get hermes-chat--work-owner :delegates)
            '(:coverage current :rows ((:kind delegate :state running)
                                      (:kind delegate :state running)))
            (plist-get hermes-chat--work-owner :processes)
            '(:coverage current :rows ((:kind process :state running))))
      (should (equal (funcall description) "Workers 2"))
      (let* ((rendered (keymap-popup--render-entry entry))
             (number (string-match "2" rendered)))
        (should number)
        (should (eq (get-text-property number 'face rendered) 'keymap-popup-value))
        (should-not (get-text-property 0 'face (funcall description))))
      (let ((chat (current-buffer)) (owner hermes-chat--work-owner))
        (unwind-protect
            (progn
              (call-interactively (keymap-lookup hermes-chat-actions-map "W"))
              (should (eq owner hermes-work--owner))
              (should (equal (funcall description) "Workers 2"))
              (hermes-chat-work)
              (should (eq (current-buffer) (plist-get owner :view)))
              (setf (plist-get (plist-get owner :delegates) :coverage) 'stale)
              (should (equal (funcall description) "Workers 2 · stale"))
              (with-current-buffer chat
                (hermes-chat--work-bind client "runtime" "B"))
              (should-error (hermes-chat-work) :type 'user-error)
              (should (equal (funcall description) "Workers 2 · stale")))
          (when (buffer-live-p (plist-get owner :view))
            (kill-buffer (plist-get owner :view)))
          (set-buffer chat)))
      (setf (plist-get hermes-chat--work-owner :delegates) '(:coverage current :rows nil))
      (should (equal (funcall description) "Workers 0")))))

(ert-deftest hermes-chat-work-refinement-header ()
  "Current empty work is silent; running counts distinguish their kinds."
  (hermes-test--with-work
    (setf (plist-get hermes-chat--work-owner :delegates)
          '(:coverage current :rows nil)
          (plist-get hermes-chat--work-owner :processes)
          '(:coverage current :rows nil))
    (should-not (hermes-chat--work-label nil))
    (should-not (hermes-chat--work-label t))
    (setf (plist-get hermes-chat--work-owner :delegates)
          '(:coverage current :rows ((:kind delegate :state running)
                                    (:kind delegate :state running)))
          (plist-get hermes-chat--work-owner :processes)
          '(:coverage current :rows ((:kind process :state running))))
    (dolist (displayable '(t nil))
      (cl-letf (((symbol-function 'char-displayable-p)
                 (lambda (character)
                   (should (= character ?🤖))
                   displayable)))
        (should (equal (substring-no-properties (hermes-chat--work-label nil))
                       (if displayable "🤖 2 / 1 process" "Agents 2 / 1 process")))
        (should (equal (substring-no-properties (hermes-chat--work-label t)) "2a / 1p"))
        (dolist (width '(1 8 12 20 30 40 50 80 120))
          (should (<= (string-width (hermes-chat--header-line width)) width)))))
    (setf (plist-get hermes-chat--work-owner :delegates) '(:coverage current :rows nil)
          (plist-get hermes-chat--work-owner :processes)
          '(:coverage current :rows ((:kind process :state running)
                                    (:kind process :state running))))
    (should (equal (substring-no-properties (hermes-chat--work-label nil)) "2 processes"))
    (should (equal (substring-no-properties (hermes-chat--work-label t)) "2p"))
    (setf (plist-get (plist-get hermes-chat--work-owner :processes) :coverage) 'stale)
    (should (equal (substring-no-properties (hermes-chat--work-label nil)) "Work ?"))
    (should (equal (substring-no-properties (hermes-chat--work-label t)) "W ?"))))

(ert-deftest hermes-chat-work-refinement-reload-wire-and-cleanup ()
  "Metadata reads use exact runtime scope and cancel independently on teardown."
  (hermes-test--with-process-wire
    (hermes-chat--work-stop)
    (setf (hermes-dashboard-transport-client-session-id client) "foreign-default")
    (hermes-chat--work-activate)
    (let* ((owner hermes-chat--work-owner)
           (frame (hermes-dashboard-transport--decode-frame (car frames)))
           (id (hermes-transport--get frame 'id))
           (pending (gethash id (hermes-dashboard-transport-client-pending client))))
      (should (equal (hermes-transport--get frame 'method) "session.title"))
      (should (equal (hermes-transport--get frame 'params) '((session_id . "runtime"))))
      (should (eq (plist-get pending :owner) (plist-get owner :adoption)))
      (should (= (car (plist-get pending :timer)) 10))
      (hermes-chat--work-activate)
      (should (= (length frames) 1))
      (hermes-test--work-reply client id "{\"session_key\":\"A\",\"title\":\"Unchanged\"}")
      (should (equal (plist-get owner :key) "A"))
      (should-not (plist-get owner :adoption))
      (should-not events))
    (hermes-chat--work-stop)
    (hermes-chat--work-activate)
    (let* ((owner hermes-chat--work-owner)
           (frame (hermes-dashboard-transport--decode-frame (car frames)))
           (id (hermes-transport--get frame 'id)))
      (hermes-chat--work-stop)
      (should-not (gethash id (hermes-dashboard-transport-client-pending client)))
      (should-not (plist-get owner :adoption))
      (hermes-test--work-reply client id "{\"session_key\":\"late\"}")
      (should-not hermes-chat--work-owner))))

(ert-deftest hermes-chat-work-refinement-reload-failures ()
  "Metadata error, timeout and synchronous quit leave no request resources."
  (dolist (failure '(error timeout quit))
    (hermes-test--with-process-wire
      (hermes-chat--work-stop)
      (let (escaped)
        (condition-case nil
            (if (eq failure 'quit)
                (let ((hermes-dashboard-transport-websocket-send-function
                       (lambda (&rest _) (signal 'quit nil))))
                  (hermes-chat--work-activate))
              (hermes-chat--work-activate)
              (let* ((frame (hermes-dashboard-transport--decode-frame (car frames)))
                     (id (hermes-transport--get frame 'id))
                     (pending (gethash id (hermes-dashboard-transport-client-pending client))))
                (if (eq failure 'timeout)
                    (let ((timer (plist-get pending :timer)))
                      (apply (cadr timer) (nth 2 timer)))
                  (hermes-dashboard-transport--handle-frame
                   client (format "{\"jsonrpc\":\"2.0\",\"id\":%S,\"error\":{\"code\":-32601,\"message\":\"Unavailable\"}}" id)))))
          (quit (setq escaped t)))
        (should-not escaped)
        (should-not (plist-get hermes-chat--work-owner :key))
        (should-not (plist-get hermes-chat--work-owner :adoption))
        (should (= 0 (hash-table-count (hermes-dashboard-transport-client-pending client))))))))

(ert-deftest hermes-chat-work-refinement-reload-adopts-once ()
  "Reload reads authoritative metadata once without touching the draft or turn."
  (hermes-test--with-work
    (hermes-chat--work-stop)
    (setq hermes-chat--session-id "unverified-fallback")
    (insert "unsent draft")
    (let ((text (buffer-string)) (position (point))
          (tick (buffer-chars-modified-tick)) (lifetime hermes-chat--lifecycle-generation)
          requests)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-session-title-fetch)
                 (lambda (_client &rest args) (push args requests))))
        (load (locate-library "hermes-chat-dashboard.el") nil t t)
        (hermes-chat--work-activate)
        (let ((owner hermes-chat--work-owner))
          (should owner)
          (should-not (plist-get owner :key))
          (load (locate-library "hermes-chat-dashboard.el") nil t t)
          (hermes-chat--work-activate)
          (should (eq owner hermes-chat--work-owner))
          (should (= (length requests) 1))
          (should (equal (plist-get (car requests) :session-id) "runtime"))
          (funcall (plist-get (car requests) :resolve) '((session_key . "A")))
          (should (equal (plist-get owner :key) "A"))
          (hermes-chat--work-activate)
          (should (= (length requests) 1))
          (should (eq owner hermes-chat--work-owner))))
      (should (equal text (buffer-string)))
      (should (= position (point)))
      (should (= tick (buffer-chars-modified-tick)))
      (should (eq lifetime hermes-chat--lifecycle-generation))
      (should (equal hermes-chat--session-id "unverified-fallback")))))

(ert-deftest hermes-chat-work-refinement-reload-rejects-replacement ()
  "Late metadata cannot adopt a replaced attachment or changed connection."
  (dolist (change '(owner runtime lifetime connection client profile instance mode))
    (hermes-test--with-work
      (hermes-chat--work-stop)
      (let (request)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-session-title-fetch)
                   (lambda (_client &rest args) (setq request args))))
          (hermes-chat--work-activate))
        (let ((owner hermes-chat--work-owner))
          (pcase change
            ('owner (hermes-chat--work-bind client "runtime" "B"))
            ('runtime (setq hermes-chat--dashboard-active-session-id "other"))
            ('lifetime (setq hermes-chat--lifecycle-generation (list 'replacement)))
            ('connection (cl-incf (hermes-dashboard-transport-client-generation client)))
            ('client (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)))
            ('profile (setq hermes-chat--profile "other"))
            ('instance (setq hermes-instance nil))
            ('mode (fundamental-mode)))
          (funcall (plist-get request :resolve) '((session_key . "late")))
          (should-not (plist-get owner :key))
          (when (eq change 'owner)
            (should (equal (plist-get hermes-chat--work-owner :key) "B"))))))))

(ert-deftest hermes-chat-work-refinement-reload-unknown-and-existing ()
  "Missing metadata stays unknown; reload does not recreate existing pollers."
  (hermes-test--with-work
    (let ((owner hermes-chat--work-owner) (timer (plist-get hermes-chat--work-owner :timer)))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-session-title-fetch)
                 (lambda (&rest _) (ert-fail "Existing owner fetched again"))))
        (hermes-chat--work-activate)
        (should (eq owner hermes-chat--work-owner))
        (should (eq timer (plist-get owner :timer)))))
    (hermes-chat--work-stop)
    (let (request)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-session-title-fetch)
                 (lambda (_client &rest args) (setq request args))))
        (hermes-chat--work-activate))
      (funcall (plist-get request :resolve) '((title . "No key")))
      (should-not (plist-get hermes-chat--work-owner :key))
      (should (string-match-p "unbound" (hermes-chat--work-details))))))

(ert-deftest hermes-chat-work-refinement-rows-first ()
  "The work list puts rows before scope prose; help retains the full caveat."
  (hermes-test--with-work
    (setf (plist-get hermes-chat--work-owner :delegates)
          '(:coverage current :rows ((:kind delegate :id "a" :key (delegate . "a")
                                     :goal "Find the bug" :state running))))
    (let ((owner hermes-chat--work-owner))
      (unwind-protect
          (progn
            (hermes-chat-work)
            (goto-char (point-min))
            (search-forward "Find the bug")
            (should (<= (line-number-at-pos) 4))
            (should-not (string-match-p "full work ledger" (buffer-string)))
            (should (eq (key-binding (kbd "i")) #'hermes-work-instance-subagents))
            (should (eq (key-binding (kbd "h")) #'hermes-work-scope-details)))
        (when (buffer-live-p (plist-get owner :view))
          (kill-buffer (plist-get owner :view)))))))

(ert-deftest hermes-chat-work-bounded-pause-refresh ()
  "A failed source pauses, busy refresh coalesces, normal turns retain ownership."
  (hermes-test--with-work
    (let ((owner hermes-chat--work-owner))
      (hermes-chat--work-refresh owner)
      (should (= (length calls) 1))
      (hermes-chat-work-refresh)
      (cl-incf hermes-chat--transport-generation)
      (should (hermes-chat--work-current-p owner))
      (funcall (plist-get (car calls) :reject) "arbitrary failure")
      (should-not (plist-get owner :request))
      (should (plist-get (plist-get owner :delegates) :paused))
      (should-not (plist-get owner :timer))
      (hermes-chat--work-refresh owner)
      (should (= (length calls) 1))
      (hermes-chat-work-refresh)
      (should (= (length calls) 2))
      (funcall (plist-get (car calls) :resolve)
               (hermes-transport-json-parse-lossless "{\"active\":[]}"))
      (should (eq (plist-get (plist-get owner :delegates) :coverage) 'current))
      (should (equal (caar timers) 5))
      ;; This fixture observes delegates alone; processes remain unknown.
      (should (equal (substring-no-properties (hermes-chat--work-label nil)) "Work ?"))
      (should-not hermes-chat--pending-assistant-id))))

(ert-deftest hermes-chat-work-rebind-stale-completion ()
  "A stale completion cannot settle or publish into a new attachment."
  (hermes-test--with-work
    (hermes-chat--work-refresh hermes-chat--work-owner)
    (let ((old (car calls)) (owner hermes-chat--work-owner))
      (hermes-chat--work-bind client "runtime" "B")
      (hermes-chat--work-refresh hermes-chat--work-owner)
      (let ((request (plist-get hermes-chat--work-owner :request)))
        (funcall (plist-get old :resolve)
                 (hermes-transport-json-parse-lossless "{\"active\":[]}"))
        (should-not (hermes-chat--work-current-p owner))
        (should (eq request (plist-get hermes-chat--work-owner :request)))
        (should-not (plist-get hermes-chat--work-owner :delegates))))))

(defmacro hermes-test--with-work-wire (&rest body)
  "Run BODY with real typed requests/raw responses and fake socket/timers."
  (declare (indent 0) (debug t))
  `(let ((rpc (symbol-function 'hermes-dashboard-transport-delegation-status))
         (hermes-dashboard-transport-request-timeout nil)
         frames events)
     (hermes-test--with-work
       (let ((hermes-dashboard-transport-websocket-send-function
              (lambda (_socket text) (push text frames))))
         (setf (hermes-dashboard-transport-client-callback client)
               (lambda (event) (push event events)))
         (cl-letf (((symbol-function 'hermes-dashboard-transport-delegation-status) rpc))
           ,@body)))))

(ert-deftest hermes-chat-reasoning-mode-exit-releases-exact-work ()
  "Error and quit in presentation cannot strand work or damage a successor."
  (dolist (condition '(error quit))
    (hermes-test--with-process-wire
      ;; Another subscriber keeps the shared client available for a successor.
      (setf (hermes-dashboard-transport-client-refcount client) 2)
      (hermes-chat--insert-entry
       '(:id "a1" :role assistant :content "" :status streaming))
      (setq hermes-chat--pending-assistant-id "a1")
      (hermes-chat--reasoning-row "a1" t)
      (hermes-chat-work-refresh)
      (let* ((owner hermes-chat--work-owner)
             (generation hermes-chat--transport-generation)
             (callback (hermes-chat--transport-callback
                        (current-buffer) "a1" nil generation))
             (id (plist-get (plist-get owner :request) :id))
             (pending (hermes-dashboard-transport-client-pending client))
             (timer (plist-get (gethash id pending) :timer))
             (cadence (list 'cadence))
             cancelled caught)
        (setf (plist-get owner :timer) (cons (list 'token) cadence))
        (cl-letf (((symbol-function 'cancel-timer)
                   (lambda (target) (push target cancelled))))
          (let ((hermes-chat-state-change-hook
                 (list (lambda () (signal condition '("Display hook failed"))))))
            (condition-case err (fundamental-mode)
              ((error quit) (setq caught err))))
          (should (eq (car caught) condition))
          (should hermes-chat--cleanup-done-p)
          (should (> hermes-chat--transport-generation generation))
          (should-not (hermes-chat--work-current-p owner))
          (should-not (gethash "a1:activity" hermes-chat--nodes))
          (should-not hermes-chat--dashboard-client)
          (should (= (hash-table-count pending) 0))
          (should (= (cl-count timer cancelled :test #'eq) 1))
          (should (= (cl-count cadence cancelled :test #'eq) 1))
          (hermes-chat--cleanup-buffer)
          (should (= (cl-count timer cancelled :test #'eq) 1))
          (should (= (cl-count cadence cancelled :test #'eq) 1))
          ;; Rebind in the same buffer; late old events and deadline callbacks
          ;; must neither recreate the row nor settle the successor request.
          (setq hermes-chat--dashboard-client client
                hermes-chat--dashboard-active-session-id "runtime"
                hermes-chat--dashboard-session-ready-p t)
          (hermes-chat--work-bind client "runtime" "A")
          (hermes-chat-work-refresh)
          (let* ((successor hermes-chat--work-owner)
                 (request (plist-get successor :request))
                 (next-id (plist-get request :id))
                 (next-timer (plist-get (gethash next-id pending) :timer)))
            (should request)
            (funcall callback '(:type thinking :event "thinking.delta" :content "late"))
            (hermes-test--work-reply client id "{\"active\":[]}")
            (apply (cadr timer) (nth 2 timer))
            (hermes-chat--cleanup-buffer)
            (should (eq successor hermes-chat--work-owner))
            (should (eq request (plist-get successor :request)))
            (should (eq next-timer (plist-get (gethash next-id pending) :timer)))
            (should (= (hash-table-count pending) 1))
            (should-not (memq next-timer cancelled))
            (should-not (gethash "a1:activity" hermes-chat--nodes))
            (hermes-chat--work-stop)))))))

(ert-deftest hermes-work-activity-mutation-is-local ()
  "Activity insertion/deletion is inert; ordinary input still runs change hooks."
  (dolist (condition '(error quit replace nil))
    (hermes-test--with-process-wire
      (setf (hermes-dashboard-transport-client-refcount client) 2)
      (setq hermes-chat--dashboard-token
            (hermes-dashboard-transport-subscribe client #'ignore))
      (hermes-chat--insert-entry
       '(:id "a1" :role assistant :content "" :status streaming))
      (setq hermes-chat--pending-assistant-id "a1")
      (let* ((owner hermes-chat--work-owner)
             (token hermes-chat--dashboard-token)
             (nodes hermes-chat--nodes)
             (pending (hermes-dashboard-transport-client-pending client))
             fired caught cancelled)
        (hermes-chat-work-refresh)
        (hermes-test--work-answer client "{\"active\":[]}")
        (hermes-test--work-answer client "{\"processes\":[]}")
        (let ((cadence (cdr (plist-get owner :timer))))
          (hermes-chat-work-refresh)
          (let* ((id (plist-get (plist-get owner :request) :id))
                 (deadline (plist-get (gethash id pending) :timer))
                 (hook (lambda (&rest _)
                         (setq fired t)
                         (pcase condition
                           ((or 'error 'quit) (signal condition '("Chat change")))
                           ('replace (fundamental-mode))))))
            (setf (plist-get owner :timer) (cons (list 'queued) cadence))
            (add-hook 'after-change-functions hook nil t)
            (unwind-protect
                (cl-letf (((symbol-function 'cancel-timer)
                           (lambda (timer) (push timer cancelled))))
                  (condition-case err
                      (progn (hermes-chat--reasoning-row "a1" t)
                             (fundamental-mode))
                    ((error quit) (setq caught (car err))))
                  (should-not caught)
                  (should-not fired)
                  (should-not (gethash "a1:activity" nodes))
                  (should-not hermes-chat--dashboard-client)
                  (should-not (gethash token (hermes-dashboard-transport-client-subscribers client)))
                  (should (= (hermes-dashboard-transport-client-refcount client) 1))
                  (should (= (hash-table-count pending) 0))
                  (should-not (hermes-chat--work-current-p owner))
                  (should-not (gethash (plist-get (plist-get owner :request) :id) pending))
                  (should-not (plist-get owner :timer))
                  (should (= (cl-count deadline cancelled :test #'eq) 1))
                  (should (= (cl-count cadence cancelled :test #'eq) 1))
                  (hermes-chat--cleanup-buffer)
                  (hermes-chat--cleanup-buffer)
                  (should (= (hermes-dashboard-transport-client-refcount client) 1))
                  (should (= (cl-count deadline cancelled :test #'eq) 1))
                  (should (= (cl-count cadence cancelled :test #'eq) 1))
                  ;; The binding ended: ordinary buffer edits retain native hooks.
                  (add-hook 'after-change-functions hook nil t)
                  (condition-case err
                      (let ((inhibit-read-only t)) (goto-char (point-max)) (insert "Input"))
                    ((error quit) (setq caught (car err))))
                  (should fired)
                  (should (eq caught (and (memq condition '(error quit)) condition))))
              (setq after-change-functions nil))))))))

(ert-deftest hermes-work-terminal-mutation-settles ()
  "List change hooks cannot interrupt terminal settlement or replace its owner."
  (dolist (event '((:type done :content "Finished") (:type error :content "Failed")))
    (dolist (condition '(error quit replace nil))
      (save-window-excursion
        (hermes-test--with-process-wire
          (hermes-chat--insert-entry
           '(:id "a1" :role assistant :content "" :status streaming))
          (setq hermes-chat--pending-assistant-id "a1"
                hermes-chat--dashboard-stream-assistant-id "a1")
          (hermes-chat--insert-entry
           '(:id "comment" :role commentary :content "Real commentary" :status done))
          (hermes-chat--reasoning-row "a1" t)
          (let ((chat (current-buffer)) (owner hermes-chat--work-owner)
                (refs (hermes-dashboard-transport-client-refcount client))
                view fired caught)
            (unwind-protect
                (progn
                  (hermes-chat-work)
                  (setq view (current-buffer))
                  (add-hook 'after-change-functions
                            (lambda (&rest _)
                              (setq fired t)
                              (pcase condition
                                ((or 'error 'quit) (signal condition '("List change")))
                                ('replace
                                 (with-current-buffer chat
                                   (hermes-chat--work-bind client "successor" "B"))))) nil t)
                  (with-current-buffer chat
                    (condition-case err (hermes-chat--handle-transport-event "a1" event)
                      ((error quit) (setq caught (car err))))
                    (should-not caught)
                    (should-not fired)
                    (should-not hermes-chat--pending-assistant-id)
                    (should-not hermes-chat--dashboard-stream-assistant-id)
                    (should-not (gethash "a1:activity" hermes-chat--nodes))
                    (should (equal (plist-get (ewoc-data (gethash "comment" hermes-chat--nodes)) :content)
                                   "Real commentary"))
                    (should (eq owner hermes-chat--work-owner))
                    (should (eq client hermes-chat--dashboard-client))
                    (should (= refs (hermes-dashboard-transport-client-refcount client))))
                  (should (eq owner (buffer-local-value 'hermes-work--owner view))))
              (when (buffer-live-p view)
                (with-current-buffer view (setq after-change-functions nil))
                (kill-buffer view)))))))))

(ert-deftest hermes-work-deferred-mutation-protects-successor ()
  "A detached render cannot run change hooks mid-print or write a replaced view."
  (dolist (condition '(error quit replace nil))
    (save-window-excursion
      (hermes-test--with-process-wire
        (setf (hermes-dashboard-transport-client-refcount client) 2)
        (let ((chat (current-buffer)) (owner hermes-chat--work-owner)
              view fired caught)
          (unwind-protect
              (progn
                (hermes-chat-work)
                (setq view (current-buffer))
                (with-current-buffer chat (hermes-chat--stop-dashboard-client))
                (let ((publication
                       (seq-find (lambda (timer)
                                   (and (eq (cadr timer) #'hermes-work--render)
                                        (eq (car (nth 2 timer)) owner))) timers)))
                  (should publication)
                  (with-current-buffer chat
                    (setq hermes-chat--dashboard-client client
                          hermes-chat--dashboard-active-session-id "successor"
                          hermes-chat--dashboard-session-ready-p t)
                    (hermes-chat--work-bind client "successor" "B")
                    (hermes-chat-work-refresh))
                  (let* ((successor (buffer-local-value 'hermes-chat--work-owner chat))
                         (request (plist-get successor :request))
                         (pending (hermes-dashboard-transport-client-pending client))
                         (deadline (plist-get (gethash (plist-get request :id) pending) :timer))
                         (transcript (with-current-buffer chat (buffer-string))))
                    (with-current-buffer view
                      (add-hook 'after-change-functions
                                (lambda (&rest _)
                                  (setq fired t)
                                  (pcase condition
                                    ((or 'error 'quit) (signal condition '("List change")))
                                    ('replace
                                     (setq after-change-functions nil)
                                     (fundamental-mode)
                                     (let ((inhibit-read-only t))
                                       (erase-buffer) (insert "Successor view content"))))) nil t))
                    (condition-case err (apply (cadr publication) (nth 2 publication))
                      ((error quit) (setq caught (car err))))
                    (should-not caught)
                    (should-not fired)
                    (should (hermes-work--view-p owner))
                    (should (string-match-p "Agents stale"
                                            (with-current-buffer view (buffer-string))))
                    ;; Prove the hook is live outside package-owned rendering.
                    (with-current-buffer view
                      (condition-case err
                          (let ((inhibit-read-only t)) (goto-char (point-max)) (insert "User edit"))
                        ((error quit) (setq caught (car err))))
                      (should fired)
                      (should (eq caught (and (memq condition '(error quit)) condition)))
                      (setq after-change-functions nil)
                      (unless (eq condition 'replace)
                        (fundamental-mode)
                        (let ((inhibit-read-only t))
                          (erase-buffer) (insert "Successor view content"))))
                    (apply (cadr publication) (nth 2 publication))
                    (should (eq (buffer-local-value 'major-mode view) 'fundamental-mode))
                    (should-not (buffer-local-value 'hermes-work--owner view))
                    (should (equal (with-current-buffer view (buffer-string)) "Successor view content"))
                    (should (eq successor (buffer-local-value 'hermes-chat--work-owner chat)))
                    (should (eq request (plist-get successor :request)))
                    (should (eq deadline (plist-get (gethash (plist-get request :id) pending) :timer)))
                    (should (= (hash-table-count pending) 1))
                    (should (= (hermes-dashboard-transport-client-refcount client) 1))
                    (should (equal transcript (with-current-buffer chat (buffer-string)))))))
            (when (buffer-live-p view)
              (with-current-buffer view (setq after-change-functions nil))
              (kill-buffer view))))))))

(defun hermes-test--work-view-teardown (action condition)
  "Exercise ACTION with an actual work list signalling CONDITION on repaint."
  (save-window-excursion
    (hermes-test--with-process-wire
      (setf (hermes-dashboard-transport-client-refcount client) 2)
      (setq hermes-chat--dashboard-token
            (hermes-dashboard-transport-subscribe client #'ignore))
      (hermes-chat--insert-entry
       '(:id "a1" :role assistant :content "" :status streaming))
      (setq hermes-chat--pending-assistant-id "a1")
      (hermes-chat--reasoning-row "a1" t)
      (hermes-chat-work-refresh)
      (let* ((chat (current-buffer))
             (owner hermes-chat--work-owner)
             (nodes hermes-chat--nodes)
             (token hermes-chat--dashboard-token)
             (callback (hermes-chat--transport-callback
                        chat "a1" nil hermes-chat--transport-generation))
             (id (plist-get (plist-get owner :request) :id))
             (pending (hermes-dashboard-transport-client-pending client))
             (deadline (plist-get (gethash id pending) :timer))
             (cadence (cdr (plist-get owner :timer)))
             view publication cancelled caught)
        ;; A cadence can already be queued when a manual refresh starts.
        (hermes-test--work-answer client "{\"active\":[]}")
        (hermes-test--work-answer client "{\"processes\":[]}")
        (setq cadence (cdr (plist-get owner :timer)))
        (hermes-chat-work-refresh)
        (setq id (plist-get (plist-get owner :request) :id)
              deadline (plist-get (gethash id pending) :timer))
        (setf (plist-get owner :timer) (cons (list 'queued) cadence))
        (unwind-protect
            (progn
              (hermes-chat-work)
              (setq view (current-buffer))
              (when condition
                (add-hook 'after-change-functions
                          (lambda (&rest _) (signal condition '("Work list display hook")))
                          nil t))
              (with-current-buffer chat
                (cl-letf (((symbol-function 'cancel-timer)
                           (lambda (timer) (push timer cancelled)))
                          ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                  (condition-case err (funcall action)
                    ((error quit) (setq caught (car err))))
                  (should-not hermes-chat--dashboard-client)
                  (should-not caught)
                  (should-not (gethash token (hermes-dashboard-transport-client-subscribers client)))
                  (should (= (hermes-dashboard-transport-client-refcount client) 1))
                  (should-not (gethash "a1:activity" nodes))
                  (should-not (hermes-chat--work-current-p owner))
                  (should (= (hash-table-count pending) 0))
                  (should (= (cl-count deadline cancelled :test #'eq) 1))
                  (should (= (cl-count cadence cancelled :test #'eq) 1))
                  (setq publication
                        (seq-find (lambda (timer)
                                    (and (eq (cadr timer) #'hermes-work--render)
                                         (eq (car (nth 2 timer)) owner))) timers))
                  (should publication)
                  ;; Deferred presentation may fail, but exact release is complete.
                  (condition-case err
                      (apply (cadr publication) (nth 2 publication))
                    ((error quit) (setq caught (car err))))
                  (should-not caught)
                  (with-current-buffer view
                    (setq after-change-functions nil)
                    (unless condition
                      (should (string-match-p "Agents stale" (buffer-string)))))
                  (hermes-chat--cleanup-buffer)
                  (hermes-chat--cleanup-buffer)
                  (should (= (hermes-dashboard-transport-client-refcount client) 1))
                  (should (= (cl-count deadline cancelled :test #'eq) 1))
                  (should (= (cl-count cadence cancelled :test #'eq) 1))
                  ;; Reuse this buffer, then deliver every captured old continuation.
                  (unless (derived-mode-p 'hermes-chat-mode) (hermes-chat-mode))
                  (setq hermes-chat--dashboard-client client
                        hermes-chat--dashboard-active-session-id "runtime"
                        hermes-chat--dashboard-session-ready-p t)
                  (hermes-chat--work-bind client "runtime" "A")
                  (hermes-chat-work-refresh)
                  (let* ((successor hermes-chat--work-owner)
                         (request (plist-get successor :request))
                         (next-id (plist-get request :id))
                         (next-timer (plist-get (gethash next-id pending) :timer))
                         (transcript (buffer-string)))
                    (should request)
                    (funcall callback '(:type thinking :event "thinking.delta" :content "late"))
                    (hermes-test--work-reply client id "{\"active\":[]}")
                    (apply (cadr deadline) (nth 2 deadline))
                    (apply (cadr cadence) (nth 2 cadence))
                    (apply (cadr publication) (nth 2 publication))
                    (should (eq successor hermes-chat--work-owner))
                    (should (eq request (plist-get successor :request)))
                    (should (eq next-timer (plist-get (gethash next-id pending) :timer)))
                    (should (= (hash-table-count pending) 1))
                    (should-not (memq next-timer cancelled))
                    (should (equal transcript (buffer-string)))
                    ;; A delayed repaint cannot write into a reused list buffer.
                    (with-current-buffer view
                      (fundamental-mode)
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert "Successor view content")))
                    (apply (cadr publication) (nth 2 publication))
                    (should (equal (with-current-buffer view (buffer-string))
                                   "Successor view content"))))))
          (when (buffer-live-p view)
            (with-current-buffer view (setq after-change-functions nil))
            (kill-buffer view))
          (when (buffer-live-p chat)
            (with-current-buffer chat (hermes-chat--stop-dashboard-client))))))))

(ert-deftest hermes-work-list-mode-exit-teardown ()
  "Mode exit releases resources before error, quit, or successful list repaint."
  (dolist (condition '(error quit nil))
    (hermes-test--work-view-teardown #'fundamental-mode condition)))

(ert-deftest hermes-work-list-disconnect-teardown ()
  "Disconnect releases resources before arbitrary work-list display hooks."
  (dolist (condition '(error quit nil))
    (hermes-test--work-view-teardown #'hermes-chat-disconnect condition)))

(ert-deftest hermes-work-list-clear-teardown ()
  "Public clear resets resources before arbitrary work-list display hooks."
  (dolist (condition '(error quit nil))
    (hermes-test--work-view-teardown #'hermes-chat-clear condition)))

(ert-deftest hermes-work-list-reconnect-teardown ()
  "Reconnect forgets the old owner before work-list display hooks can fail."
  (dolist (condition '(error quit nil))
    (save-window-excursion
      (hermes-test--with-process-wire
        (hermes-chat--insert-entry
         '(:id "a1" :role assistant :content "" :status streaming))
        (setq hermes-chat--pending-assistant-id "a1")
        (hermes-chat--reasoning-row "a1" t)
        (hermes-chat-work-refresh)
        (let ((chat (current-buffer))
              (owner hermes-chat--work-owner)
              view caught)
          (unwind-protect
              (progn
                (hermes-chat-work)
                (setq view (current-buffer))
                (when condition
                  (add-hook 'after-change-functions
                            (lambda (&rest _) (signal condition '("Work list display hook")))
                            nil t))
                (with-current-buffer chat
                  (condition-case err
                      (hermes-chat--handle-transport-event
                       "a1" '(:type status :status "reconnecting"))
                    ((error quit) (setq caught (car err))))
                  (should-not caught)
                  (should-not hermes-chat--work-owner)
                  (should-not hermes-chat--dashboard-active-session-id)
                  (should-not (gethash "a1:activity" hermes-chat--nodes))
                  (should (= (hash-table-count
                              (hermes-dashboard-transport-client-pending client)) 0))
                  ;; Reconnect retains the shared client, unlike terminal detach.
                  (should (eq client hermes-chat--dashboard-client))
                  (let ((publication
                         (seq-find (lambda (timer)
                                     (and (eq (cadr timer) #'hermes-work--render)
                                          (eq (car (nth 2 timer)) owner))) timers)))
                    (should publication)
                    (condition-case err
                        (apply (cadr publication) (nth 2 publication))
                      ((error quit) (setq caught (car err))))
                    (should-not caught))))
            (when (buffer-live-p view)
              (with-current-buffer view (setq after-change-functions nil))
              (kill-buffer view))))))))

(defun hermes-test--work-reply (client id json)
  "Deliver JSON result for request ID through CLIENT's raw frame handler."
  (hermes-dashboard-transport--handle-frame
   client (format "{\"jsonrpc\":\"2.0\",\"id\":%S,\"result\":%s}" id json)))

(ert-deftest hermes-chat-work-real-wire-validation ()
  "Serialized shapes establish current, partial, or stale, never invented idle."
  (hermes-test--with-work-wire
    (dolist (case '(("{\"active\":[]}" current nil)
                    ("{}" stale t) ("{\"active\":null}" stale t)
                    ("{\"active\":false}" stale t) ("{\"active\":{}}" stale t)
                    ("{\"active\":7}" stale t) ("{\"active\":[null]}" partial t)
                    ("{\"active\":[{\"subagent_id\":\"x\",\"owner_agent_session_id\":\"A\",\"status\":\"running\"},false]}" partial t)))
      (hermes-chat-work-refresh)
      (let* ((owner hermes-chat--work-owner)
             (id (plist-get (plist-get owner :request) :id))
             (pending (gethash id (hermes-dashboard-transport-client-pending client))))
        (should (equal (hermes-transport--get
                        (hermes-dashboard-transport--decode-frame (car frames)) 'method)
                       "delegation.status"))
        (should (eq (plist-get pending :owner) owner))
        (should (plist-get pending :lossless-result))
        (should (= (car (plist-get pending :timer)) 10))
        (hermes-test--work-reply client id (car case))
        (let ((source (plist-get owner :delegates)))
          (should (eq (plist-get source :coverage) (cadr case)))
          (should (eq (plist-get source :paused) (nth 2 case)))
          (should-not (plist-get owner :request))
          (when (nth 2 case)
            (should-not (plist-get owner :timer))
            (should (string-match-p "?" (hermes-chat--work-label nil)))))))
    (should (equal (substring-no-properties (hermes-chat--work-label nil))
                   (if (char-displayable-p ?🤖) "🤖 1 ?" "Agents 1 ?")))
    (should-not events)
    (should-not hermes-chat--pending-assistant-id)))

(ert-deftest hermes-chat-work-real-errors-pause ()
  "All message-only RPC failures pause locally without settling the parent."
  (hermes-test--with-work-wire
    (dolist (code '(-32601 4001 5010 -32603))
      (hermes-chat-work-refresh)
      (let ((id (plist-get (plist-get hermes-chat--work-owner :request) :id)))
        (hermes-dashboard-transport--handle-frame
         client (format "{\"jsonrpc\":\"2.0\",\"id\":%S,\"error\":{\"code\":%s,\"message\":\"foreign secret\"}}"
                        id code)))
      (should (plist-get (plist-get hermes-chat--work-owner :delegates) :paused))
      (should-not (plist-get hermes-chat--work-owner :timer))
      (should-not (string-match-p "foreign secret" (hermes-chat--work-details))))
    (should-not events)))

(ert-deftest hermes-chat-work-real-timeout-and-late-response ()
  "Global nil still yields ten seconds, and late success cannot affect a new cycle."
  (hermes-test--with-work-wire
    (hermes-chat-work-refresh)
    (let* ((owner hermes-chat--work-owner)
           (id (plist-get (plist-get owner :request) :id))
           (timer (plist-get (gethash id (hermes-dashboard-transport-client-pending client))
                             :timer)))
      (should (= (car timer) 10))
      (apply (cadr timer) (nth 2 timer))
      (should-not (gethash id (hermes-dashboard-transport-client-pending client)))
      (should (plist-get (plist-get owner :delegates) :paused))
      (hermes-chat-work-refresh)
      (let ((request (plist-get owner :request)))
        (hermes-test--work-reply client id "{\"active\":[]}")
        (should (eq request (plist-get owner :request)))
        (should (eq (plist-get (plist-get owner :delegates) :coverage) 'stale))))
    (should-not events)))

(ert-deftest hermes-chat-work-readiness-deadline-and-drop ()
  "Held readiness remains bounded; dropping an owner before ready suppresses send."
  (dolist (drop '(nil t))
    (hermes-test--with-work-wire
      (let (ready)
        (cl-letf (((symbol-function 'hermes-dashboard-transport--when-ready)
                   (lambda (_client resolve _reject) (setq ready resolve))))
          (hermes-chat-work-refresh))
        (let* ((owner hermes-chat--work-owner)
               (id (plist-get (plist-get owner :request) :id))
               (timer (plist-get (gethash id (hermes-dashboard-transport-client-pending client))
                                 :timer)))
          (should (= (car timer) 10))
          (if drop (hermes-chat--work-stop)
            (apply (cadr timer) (nth 2 timer)))
          (funcall ready)
          (should-not frames)
          (should-not (gethash id (hermes-dashboard-transport-client-pending client))))))))

(ert-deftest hermes-chat-work-synchronous-failures ()
  "Synchronous readiness rejection and send errors leave no wedged context."
  (dolist (failure '(ready send signal))
    (hermes-test--with-work-wire
      (cl-letf (((symbol-function 'hermes-dashboard-transport--when-ready)
                 (lambda (_client resolve reject)
                   (pcase failure
                     ('ready (funcall reject "not ready"))
                     ('signal (error "registration failed"))
                     (_ (funcall resolve))))))
        (let ((hermes-dashboard-transport-websocket-send-function
               (lambda (&rest _) (error "send failed"))))
          (hermes-chat-work-refresh)))
      (should-not (plist-get hermes-chat--work-owner :request))
      (should (plist-get (plist-get hermes-chat--work-owner :delegates) :paused))
      (should-not (plist-get hermes-chat--work-owner :timer))
      (should-not events))))

(ert-deftest hermes-chat-work-ui-reentry-and-generation ()
  "An old finalizer cannot arm cadence or publish after a UI hook replaces owner."
  (hermes-test--with-work
    (hermes-chat-work-refresh)
    (let ((old hermes-chat--work-owner) (resolve (plist-get (car calls) :resolve)))
      (cl-letf (((symbol-function 'hermes-chat--notify-state-change)
                 (lambda ()
                   (hermes-chat--work-bind client "runtime" "B")
                   (hermes-chat-work-refresh))))
        (funcall resolve (hermes-transport-json-parse-lossless "{\"active\":[]}")))
      (should-not (plist-get old :timer))
      (should (plist-get hermes-chat--work-owner :request))
      (should-not (plist-get hermes-chat--work-owner :delegates))
      (cl-incf hermes-chat--work-generation)
      (funcall (plist-get (car calls) :resolve)
               (hermes-transport-json-parse-lossless "{\"active\":[]}"))
      (should-not (plist-get hermes-chat--work-owner :delegates)))))

(ert-deftest hermes-chat-work-synchronous-ready-drops-owner ()
  "A readiness callback can destroy the owner before the wrapper returns its ID."
  (hermes-test--with-work-wire
    (let ((owner hermes-chat--work-owner))
      (cl-letf (((symbol-function 'hermes-dashboard-transport--when-ready)
                 (lambda (_client resolve _reject)
                   (hermes-chat--work-stop)
                   (funcall resolve))))
        (hermes-chat--work-refresh owner))
      (should-not frames)
      (should-not hermes-chat--work-owner)
      (should-not (plist-get (plist-get owner :request) :id))
      (should (= (hash-table-count (hermes-dashboard-transport-client-pending client)) 0)))))

(ert-deftest hermes-chat-work-binding-is-durable-not-runtime ()
  "Session binding records only session_key, never the runtime fallback."
  (hermes-test--with-work
    (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
      (hermes-chat--dashboard-record-session client '((session_id . "runtime")))
      (should-not (plist-get hermes-chat--work-owner :key))
      (hermes-chat-work-refresh)
      (should-not calls)
      (hermes-chat--dashboard-record-session
       client '((session_id . "runtime") (session_key . "A")))
      (should (equal (plist-get hermes-chat--work-owner :key) "A"))
      (hermes-chat-work-refresh)
      (should (= (length calls) 1))
      (hermes-chat--dashboard-record-session
       client '((session_id . "runtime") (stored_session_id . "A")))
      (should-not (plist-get hermes-chat--work-owner :key))
      (should-not (plist-get hermes-chat--work-owner :delegates)))))

(ert-deftest hermes-chat-work-two-chats-one-client ()
  "A and B receive the same global wire inventory but keep exact owned rows."
  (hermes-test--with-work-wire
    (let ((a (current-buffer)) (owner-a hermes-chat--work-owner)
          (b (generate-new-buffer " *work B*"))
          (wire "{\"active\":[{\"subagent_id\":\"a\",\"owner_agent_session_id\":\"A\",\"status\":\"running\"},{\"subagent_id\":\"b\",\"owner_agent_session_id\":\"B\",\"status\":\"running\",\"goal\":\"private B\"}]}"))
      (unwind-protect
          (progn
            (hermes-chat-work-refresh)
            (set-window-buffer (split-window-right) b)
            (with-current-buffer b
              (hermes-chat-mode)
              (setq hermes-chat--dashboard-client client
                    hermes-chat--dashboard-active-session-id "runtime-B"
                    hermes-chat--dashboard-session-ready-p t)
              (hermes-chat--work-bind client "runtime-B" "B")
              (hermes-chat-work-refresh)
              (let ((owner-b hermes-chat--work-owner))
                (hermes-test--work-reply client
                                        (plist-get (plist-get owner-a :request) :id) wire)
                (hermes-test--work-reply client
                                        (plist-get (plist-get owner-b :request) :id) wire)
                (should (equal (mapcar (lambda (row) (plist-get row :id))
                                      (plist-get (plist-get owner-b :delegates) :rows)) '("b")))
                (with-current-buffer a
                  (should (equal (mapcar (lambda (row) (plist-get row :id))
                                        (plist-get (plist-get owner-a :delegates) :rows)) '("a")))
                  (should-not (string-match-p "private B"
                                              (prin1-to-string (plist-get owner-a :delegates))))))))
        (kill-buffer b)))))

(ert-deftest hermes-chat-work-window-removal-and-reentry ()
  "Actual last-window removal cancels cadence; reentry schedules without sending."
  (hermes-test--with-work
    (let ((chat (current-buffer)) (owner hermes-chat--work-owner))
      (hermes-chat-work-refresh)
      (funcall (plist-get (car calls) :resolve)
               (hermes-transport-json-parse-lossless "{\"active\":[]}"))
      (let ((old-timer (cdr (plist-get owner :timer)))
            (other (split-window-right)))
        (set-window-buffer other chat)
        (set-window-buffer (selected-window) (get-buffer-create " *work hidden*"))
        (delete-window other)
        ;; Run the same default hook dispatched by window-state redisplay.
        (run-hook-with-args 'pre-redisplay-functions (selected-window))
        (should-not (plist-get owner :timer))
        (should (eq (plist-get (plist-get owner :delegates) :coverage) 'stale))
        (apply (cadr old-timer) (nth 2 old-timer))
        (should (= (length calls) 1))
        (set-window-buffer (selected-window) chat)
        (run-hook-with-args 'pre-redisplay-functions (selected-window))
        (should (= (length calls) 1))
        (let ((timer (cdr (plist-get owner :timer))))
          (should (= (car timer) 0))
          (apply (cadr timer) (nth 2 timer)))
        (should (= (length calls) 2))
        (set-window-buffer (selected-window) (get-buffer-create " *work hidden*"))
        (run-hook-with-args 'pre-redisplay-functions (selected-window))
        (funcall (plist-get (car calls) :reject) "failed while hidden")
        (set-window-buffer (selected-window) chat)
        (run-hook-with-args 'pre-redisplay-functions (selected-window))
        (should-not (plist-get owner :timer))
        (should (= (length calls) 2)))
      (kill-buffer " *work hidden*"))))

(ert-deftest hermes-chat-work-frame-visibility-notifications ()
  "Focus and redisplay notifications reconcile visibility without sending RPCs."
  (dolist (notification '(focus redisplay))
    (hermes-test--with-work
      (let ((owner hermes-chat--work-owner)
            (visible t)
            (notify (if (eq notification 'focus)
                        (lambda () (funcall after-focus-change-function))
                      (lambda ()
                        (run-hook-with-args 'pre-redisplay-functions
                                            (selected-window))))))
        (hermes-chat-work-refresh)
        (funcall (plist-get (car calls) :resolve)
                 (hermes-transport-json-parse-lossless "{\"active\":[]}"))
        (let ((old-timer (cdr (plist-get owner :timer))))
          (cl-letf (((symbol-function 'frame-visible-p) (lambda (&rest _) visible)))
            (setq visible 'icon)
            (funcall notify)
            (should-not (plist-get owner :timer))
            (should (eq (plist-get (plist-get owner :delegates) :coverage) 'stale))
            (apply (cadr old-timer) (nth 2 old-timer))
            (should (= (length calls) 1))
            (setq visible t)
            (funcall notify)
            (should (= (length calls) 1))
            (let ((timer (cdr (plist-get owner :timer))))
              (should (= (car timer) 0))
              (funcall notify)
              (should (eq timer (cdr (plist-get owner :timer))))
              (apply (cadr timer) (nth 2 timer)))
            (should (= (length calls) 2))))))))

(ert-deftest hermes-chat-work-connection-turn-and-cleanup ()
  "Normal turns preserve snapshots, but disconnect/reset/mode exit release ownership."
  (dolist (end '(disconnect reset mode kill))
    (hermes-test--with-work-wire
      (hermes-chat-work-refresh)
      (let* ((owner hermes-chat--work-owner)
             (id (plist-get (plist-get owner :request) :id))
             (foreign (list 'unrelated))
             (other-id (let ((hermes-dashboard-transport-request-owner foreign))
                         (hermes-dashboard-transport-request client "other" nil #'ignore #'ignore))))
        (rename-buffer (generate-new-buffer-name " *renamed owner*"))
        (cl-incf hermes-chat--transport-generation)
        (should (hermes-chat--work-current-p owner))
        (pcase end
          ('disconnect (hermes-chat--forget-live-dashboard-session))
          ('reset (hermes-chat--reset-transcript))
          ('mode (fundamental-mode))
          ('kill (kill-buffer (current-buffer))))
        (should-not (hermes-chat--work-current-p owner))
        (should-not (gethash id (hermes-dashboard-transport-client-pending client)))
        ;; Work cleanup itself never cancels requests owned by another feature.
        ;; Full chat teardown may close an unshared client, so check disconnect here.
        (when (eq end 'disconnect)
          (should (gethash other-id (hermes-dashboard-transport-client-pending client))))
        (hermes-test--work-reply client id "{\"active\":[]}")
        (should-not (plist-get owner :timer))))))

(ert-deftest hermes-chat-work-connection-change-makes-header-unknown ()
  "Connection generation mismatch suppresses even previously positive counts."
  (hermes-test--with-work
    (hermes-chat-work-refresh)
    (funcall (plist-get (car calls) :resolve)
             (hermes-transport-json-parse-lossless
              "{\"active\":[{\"subagent_id\":\"a\",\"owner_agent_session_id\":\"A\",\"status\":\"running\"}]}"))
    (should (equal (substring-no-properties (hermes-chat--work-label t)) "1a ?"))
    (cl-incf (hermes-dashboard-transport-client-generation client))
    (should (equal (substring-no-properties (hermes-chat--work-label t)) "W ?"))
    (hermes-chat--work-visibility)
    (should-not (plist-get hermes-chat--work-owner :timer))))

(ert-deftest hermes-chat-work-header-and-details-preserve-draft ()
  "Ready and work coexist without render I/O, point movement or draft changes."
  (hermes-test--with-work
    (hermes-chat--set-header-state :status 'ready)
    (insert "unsent draft")
    (let ((text (buffer-string)) (point (point))
          (chat (current-buffer))
          (window (selected-window))
          (help-map (copy-keymap help-mode-map)))
      (hermes-chat-work-refresh)
      (funcall (plist-get (car calls) :resolve)
               (hermes-transport-json-parse-lossless
                "{\"active\":[{\"subagent_id\":\"a\",\"owner_agent_session_id\":\"A\",\"status\":\"running\"}]}"))
      (dolist (width '(12 20 30 40 50 80 120))
        (let ((header (hermes-chat--header-line width)))
          (should (<= (string-width header) width))
          (should-not (string-match-p "Ready" header))
          (when (>= width 30)
            (should (string-match-p
                     (if (< width 50) "1a"
                       (if (char-displayable-p ?🤖) "🤖 1" "Agents 1")) header)))))
      (should (eq (get-text-property 0 'face (hermes-chat--work-label nil))
                  'hermes-work-running))
      (should (string-match-p "not a full work ledger" (hermes-chat--work-details)))
      (should (equal text (buffer-string)))
      (should (= point (point)))
      (should (eq window (selected-window)))
      (should (= (length calls) 1))
      (unwind-protect
          (progn
            (hermes-chat-session-details)
            (should (equal help-map help-mode-map))
            ;; Help display may replace the sole chat window in batch Emacs.
            ;; Details alone must not authorize a request, even explicit g.
            (delete-other-windows window)
            (set-window-buffer window "*Hermes Session Details*")
            (should-not (hermes-chat--work-visible-p hermes-chat--work-owner))
            (with-current-buffer "*Hermes Session Details*"
              (call-interactively (key-binding (kbd "g"))))
            (should (= (length calls) 1))
            (set-window-buffer (split-window-right) chat)
            (should (hermes-chat--work-visible-p hermes-chat--work-owner))
            (with-current-buffer "*Hermes Session Details*"
              (call-interactively (key-binding (kbd "g"))))
            (should (= (length calls) 2))
            (with-current-buffer chat
              (should (equal text (buffer-string)))
              (should (= point (point)))))
        (when (get-buffer "*Hermes Session Details*")
          (kill-buffer "*Hermes Session Details*"))))))

;;; Shared-socket runtime isolation

(defun hermes-test--resolve-new-dashboard-session (_client &rest args)
  "Resolve ARGS as a fresh dashboard session."
  (funcall (plist-get args :resolve) '((session_id . "sid-new"))))

(defun hermes-test--confirming-config-set (record)
  "Return a `config.set' stub that calls RECORD and requires confirmation."
  (lambda (_client key value &rest args)
    (funcall record key value args)
    (funcall (plist-get args :resolve)
             (cond
              ((not (equal key "model")) `((key . ,key)))
              ((plist-get args :confirm-expensive-model) '((key . "model")))
              (t '((confirm_required . t) (confirm_message . "Expensive model")))))))

(ert-deftest hermes-chat-dashboard-acquires-client-for-buffer-instance ()
  "Each chat acquires its dashboard client under its pinned instance URL."
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test")))
        urls buffers)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
               (lambda (&rest _)
                 (push hermes-dashboard-transport-url urls)
                 (make-hermes-dashboard-transport-client))))
      (unwind-protect
          (dolist (instance '(("local" . "http://127.0.0.1:9119")
                              ("remote" . "https://hermes.example.test")))
            (let ((buffer (generate-new-buffer (hermes-test--chat-buffer-name))))
              (push buffer buffers)
              (with-current-buffer buffer
                (hermes-chat-mode)
                (setq hermes-instance instance)
                (hermes-chat--dashboard-ensure-client))))
        (mapc (lambda (buffer)
                (when (buffer-live-p buffer) (kill-buffer buffer)))
              buffers)))
    (should (equal (nreverse urls)
                   '("http://127.0.0.1:9119"
                     "https://hermes.example.test")))))

(defmacro hermes-test--with-submit-wire (&rest body)
  "Run BODY with an attached chat and captured production RPC frames."
  (declare (indent 0) (debug t))
  `(let* ((client (hermes-test--dashboard-client))
          (hermes-transport-send-function #'hermes-transport-send)
          (hermes-dashboard-transport-request-timeout nil)
          frames
          (hermes-dashboard-transport-websocket-send-function
           (lambda (_socket text)
             (push (hermes-dashboard-transport--decode-frame text) frames))))
     (setf (hermes-dashboard-transport-client-ready-p client) t)
     (cl-letf (((symbol-function 'hermes-chat--maybe-refresh-session-title) #'ignore)
               ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
       (hermes-test-with-chat-buffer
        (setq hermes-chat--dashboard-client client
              hermes-chat--dashboard-active-session-id "sid"
              hermes-chat--dashboard-session-ready-p t)
        ,@body))))

(defun hermes-test--submit-wire-event (client type &optional payload)
  "Deliver TYPE and PAYLOAD through CLIENT's production JSON parser."
  (hermes-dashboard-transport--handle-frame
   client (hermes-dashboard-transport--encode-frame
           `((jsonrpc . "2.0") (method . "event")
             (params . ((type . ,type) (session_id . "sid")
                        (payload . ,payload)))))))

(defun hermes-test--submit-wire-reply (client request &optional reject status)
  "Deliver CLIENT's response to REQUEST, with optional REJECT or STATUS."
  (hermes-dashboard-transport--handle-frame
   client (hermes-dashboard-transport--encode-frame
           `((jsonrpc . "2.0") (id . ,(alist-get 'id request))
             ,(if reject
                  '(error . ((code . -32000) (message . "submit rejected")))
                `(result . ((status . ,(or status "accepted")))))))))

(defun hermes-test--terminal-before-submit-ack (queued)
  "Prove terminal-before-ack settlement for a normal or QUEUED submission."
  (dolist (terminal '("message.complete" "error"))
    (hermes-test--with-submit-wire
     (if queued
         (progn
           (hermes-chat--queue-content "first\nfull body")
           (hermes-chat--drain-queued-message))
       (insert "first\nfull body")
       (hermes-chat-send))
     (let ((request (car frames))
           (context hermes-chat--unsettled-submit-context))
       (should (equal (alist-get 'method request) "prompt.submit"))
       (should (eq (not (null (plist-get context :queue-id))) queued))
       (hermes-chat-queue-message "second\nfull body")
       (hermes-chat-queue-message "third\nfull body")
       (let ((suffix (last hermes-chat--queued-messages 2)))
         (hermes-test--submit-wire-event client "message.start")
         (hermes-test--submit-wire-event client terminal '((text . "finished")))
         (hermes-test--submit-wire-event client terminal '((text . "duplicate")))
         (should-not hermes-chat--pending-assistant-id)
         (should (eq context hermes-chat--unsettled-submit-context))
         (hermes-test--submit-wire-reply client request)
         (should-not (eq context hermes-chat--unsettled-submit-context))
         (hermes-test--submit-wire-event client "session.info" '((running . :false)))
         (should (eq suffix hermes-chat--queued-messages))
         (should (equal (hermes-test--queued-contents)
                        '("second\nfull body" "third\nfull body")))
         (should (= (length frames) 2))
         (let ((successor hermes-chat--unsettled-submit-context))
           (should successor)
           (should (equal (plist-get successor :content) "second\nfull body"))
           (hermes-test--submit-wire-reply client request)
           (should (eq successor hermes-chat--unsettled-submit-context))
           (should (eq suffix hermes-chat--queued-messages)))
         (hermes-test--submit-wire-reply client (car frames))
         (should (equal (hermes-test--queued-contents) '("third\nfull body")))
         (hermes-test--submit-wire-event client "message.complete")
         (hermes-test--submit-wire-event client "session.info" '((running . :false)))
         (should (= (length frames) 3))
         (should (equal (mapcar (lambda (frame)
                                  (alist-get 'text (alist-get 'params frame)))
                                (reverse frames))
                        '("first\nfull body" "second\nfull body" "third\nfull body"))))))))

(ert-deftest hermes-chat-dashboard-terminal-before-submit-ack-normal ()
  "Normal terminal-before-ack releases the request and advances FIFO once."
  (hermes-test--terminal-before-submit-ack nil))

(ert-deftest hermes-chat-dashboard-terminal-before-submit-ack-queued ()
  "Queued terminal-before-ack accepts the exact FIFO head once."
  (hermes-test--terminal-before-submit-ack t))

(ert-deftest hermes-chat-dashboard-submit-terminal-orderings ()
  "Acceptance, terminal and idle settle once in each valid finite ordering."
  (dolist (order '((ack terminal idle) (terminal ack idle)
                   (terminal idle ack)))
    (dolist (status '("accepted" "queued"))
      (hermes-test--with-submit-wire
       (hermes-chat--queue-content "first")
       (hermes-chat--drain-queued-message)
       (let ((request (car frames))
             (assistant hermes-chat--pending-assistant-id))
         ;; Idle then start is the existing authoritative boundary separating a
         ;; server-queued turn from the prior turn's output.
         (hermes-test--submit-wire-event client "session.info" '((running . :false)))
         (hermes-test--submit-wire-event client "message.start")
         (dolist (step order)
           (pcase step
             ('ack (hermes-test--submit-wire-reply client request nil status))
             ('terminal (hermes-test--submit-wire-event client "message.complete"))
             ('idle (hermes-test--submit-wire-event
                     client "session.info" '((running . :false))))))
         (should-not hermes-chat--unsettled-submit-context)
         (should-not hermes-chat--queued-messages)
         (should-not hermes-chat--queued-submit-id)
         (should-not hermes-chat--pending-assistant-id)
         (should-not hermes-chat--server-queued-assistant-id)
         (should (eq (plist-get (ewoc-data (gethash assistant hermes-chat--nodes))
                               :status) 'done))
         (hermes-test--submit-wire-reply client request nil status)
         (should-not (hermes-chat--active-turn-p))
         (should (= (length frames) 1)))))))

(ert-deftest hermes-chat-dashboard-terminal-before-submit-reject ()
  "Late rejection settles its request without sending or losing retained input."
  (dolist (queued '(nil t))
    (hermes-test--with-submit-wire
     (if queued
         (progn
           (hermes-chat--queue-content "first")
           (hermes-chat--drain-queued-message))
       (insert "first") (hermes-chat-send))
     (let* ((request (car frames))
            (reject (plist-get
                     (gethash (alist-get 'id request)
                              (hermes-dashboard-transport-client-pending client))
                     :reject))
            (context hermes-chat--unsettled-submit-context))
       (hermes-chat-queue-message "second")
       (hermes-test--submit-wire-event client "message.complete")
       (hermes-test--submit-wire-event client "session.info" '((running . :false)))
       (hermes-test--submit-wire-reply client request t)
       (should-not (eq context hermes-chat--unsettled-submit-context))
       (if queued
           (progn
             (should (equal (hermes-test--queued-contents) '("first" "second")))
             (should-not hermes-chat--queued-submit-id)
             (should-not hermes-chat--pending-assistant-id)
             (should (= (length frames) 1)))
         ;; The next accepted input may progress, but old rejection must not
         ;; clear the new turn's running state after dispatching it.
         (should (= (length frames) 2))
         (should hermes-chat--dashboard-running-p)
         (should hermes-chat--pending-assistant-id)
         (should (equal (plist-get hermes-chat--unsettled-submit-context :content)
                        "second")))
       (let ((before (copy-tree (hermes-chat--entries)))
             (owner hermes-chat--unsettled-submit-context))
         (funcall reject "late duplicate rejection")
         (should (equal before (hermes-chat--entries)))
         (should (eq owner hermes-chat--unsettled-submit-context)))))))

(defun hermes-test--rejected-head-retry (terminal)
  "Prove explicit retry after rejection, with optional preceding TERMINAL."
    (hermes-test--with-submit-wire
     (hermes-chat--queue-content "first\n  exact body  " nil "compact")
     (hermes-chat--drain-queued-message)
     (hermes-chat-queue-message "second\nbody")
     (insert "newer draft\n  intact  ")
     (let ((request (car frames))
           (head (car hermes-chat--queued-messages))
           (suffix (cdr hermes-chat--queued-messages))
           (draft (buffer-substring-no-properties
                   hermes-chat--input-marker (point-max))))
       (when terminal
         (hermes-test--submit-wire-event client "message.complete"))
       (hermes-test--submit-wire-reply client request t)
       (dotimes (_ 3)
         (hermes-test--submit-wire-event client "session.info" '((running . :false))))
       (should (= (length frames) 1))
       (should (eq head (car hermes-chat--queued-messages)))
       (should (eq suffix (cdr hermes-chat--queued-messages)))
       (should (plist-get head :rejected-p))
       (should-not hermes-chat--unsettled-submit-context)
       (let ((panel (save-window-excursion (hermes-chat-queue-panel))))
         (unwind-protect
             (progn
               (with-current-buffer panel
                 (should (string-match-p "Retry queued message" (buffer-string)))
                 (call-interactively (keymap-lookup hermes-chat-queue-panel-mode-map "r")))
               (should (= (length frames) 2))
               (should-not (plist-get head :rejected-p))
               (should (equal (alist-get 'text (alist-get 'params (car frames)))
                              "first\n  exact body  "))
               (with-current-buffer panel
                 (should-error (hermes-chat-queue-panel-retry) :type 'user-error))
               (should (= (length frames) 2))
               (should (equal draft (buffer-substring-no-properties
                                     hermes-chat--input-marker (point-max))))
               (hermes-test--submit-wire-reply client (car frames))
               (should (eq suffix hermes-chat--queued-messages)))
           (kill-buffer panel))))))

(ert-deftest hermes-chat-dashboard-rejected-head-requires-explicit-retry ()
  "Reject then idle never sends again until an explicit panel retry."
  (hermes-test--rejected-head-retry nil))

(ert-deftest hermes-chat-dashboard-terminal-rejected-head-requires-explicit-retry ()
  "Terminal, reject then idle never sends again until an explicit panel retry."
  (hermes-test--rejected-head-retry t))

(ert-deftest hermes-chat-dashboard-rejected-retry-refuses-invalid-owner ()
  "Busy, detached and stale panels preserve the rejected entry and draft."
  (dolist (change '(busy detached lifetime client session panel mode))
    (hermes-test--with-submit-wire
     (hermes-chat--queue-content "first\nexact")
     (hermes-chat--drain-queued-message)
     (hermes-test--submit-wire-reply client (car frames) t)
     (insert "draft")
     (let ((panel (save-window-excursion (hermes-chat-queue-panel)))
           (head (car hermes-chat--queued-messages)))
       (unwind-protect
           (progn
             (pcase change
               ('busy (setq hermes-chat--dashboard-running-p t))
               ('detached (setq hermes-chat--dashboard-session-ready-p nil))
               ('lifetime (setq hermes-chat--lifecycle-generation 'changed))
               ('client (setq hermes-chat--dashboard-client
                              (hermes-test--dashboard-client)))
               ('session (setq hermes-chat--dashboard-active-session-id "other"))
               ('panel (setq hermes-chat--queue-panel-buffer nil))
               ('mode (setq major-mode 'fundamental-mode)))
             (with-current-buffer panel
               (should-error (call-interactively
                              (keymap-lookup hermes-chat-queue-panel-mode-map "r"))
                             :type 'user-error))
             (should (eq head (car hermes-chat--queued-messages)))
             (should (plist-get head :rejected-p))
             (should (equal "first\nexact" (plist-get head :content)))
             (should (equal "draft" (hermes-chat-input-string)))
             (should (= (length frames) 1)))
         (kill-buffer panel))))))

(ert-deftest hermes-chat-dashboard-rejected-edit-and-remove-preserve-pause ()
  "Editing and removing a suffix cannot silently resend the rejected head."
  (hermes-test--with-submit-wire
   (hermes-chat--queue-content "first")
   (hermes-chat--drain-queued-message)
   (hermes-chat-queue-message "second")
   (hermes-test--submit-wire-reply client (car frames) t)
   (let ((panel (save-window-excursion (hermes-chat-queue-panel))))
     (unwind-protect
         (progn
           (with-current-buffer panel
             (cl-letf (((symbol-function 'read-string-from-buffer)
                        (lambda (&rest _) "edited\nbody")))
               (call-interactively #'hermes-chat-queue-panel-edit))
             (forward-line 1)
             (call-interactively #'hermes-chat-queue-panel-remove))
           (hermes-test--submit-wire-event client "session.info" '((running . :false)))
           (should (= (length frames) 1))
           (should (equal (hermes-test--queued-contents) '("edited\nbody")))
           (should (plist-get (car hermes-chat--queued-messages) :rejected-p))
           (with-current-buffer panel
             (call-interactively #'hermes-chat-queue-panel-remove))
           (hermes-test--submit-wire-event client "session.info" '((running . :false)))
           (should-not hermes-chat--queued-messages)
           (should (= (length frames) 1)))
       (kill-buffer panel)))))

(ert-deftest hermes-chat-dashboard-submit-replaced-owner ()
  "Old wire responses cannot settle changed context, lifetime or connection."
  (dolist (replacement '(context lifetime generation client session))
    (dolist (reject '(nil t))
      (hermes-test--with-submit-wire
       (hermes-chat--queue-content "owned head")
       (hermes-chat--drain-queued-message)
       (hermes-chat-queue-message "untouched suffix")
       (let ((request (car frames)))
         (hermes-test--submit-wire-event client "message.complete")
         (pcase replacement
           ('context (setq hermes-chat--unsettled-submit-context
                           (copy-sequence hermes-chat--unsettled-submit-context)))
           ('lifetime (setq hermes-chat--lifecycle-generation (list 'replacement)))
           ('generation (hermes-chat--next-transport-generation))
           ('client (setq hermes-chat--dashboard-client
                          (hermes-test--dashboard-client)))
           ('session (setq hermes-chat--dashboard-active-session-id "replacement")))
         (let ((owner hermes-chat--unsettled-submit-context)
               (queue hermes-chat--queued-messages)
               (before (copy-tree (hermes-chat--entries)))
               (header (copy-tree hermes-chat--status-state)))
           (hermes-test--submit-wire-reply client request reject "queued")
           (should (eq owner hermes-chat--unsettled-submit-context))
           (should (eq queue hermes-chat--queued-messages))
           (should (equal (hermes-test--queued-contents)
                          '("owned head" "untouched suffix")))
           (should (equal before (hermes-chat--entries)))
           (should (equal header hermes-chat--status-state))
           (should-not hermes-chat--pending-assistant-id)
           (should (= (length frames) 1))))))))

(ert-deftest hermes-chat-dashboard-parses-vanilla-goal-status ()
  "Vanilla `/goal status' output becomes compact header state."
  (should
   (equal (hermes-chat--dashboard-goal-status-projection
           "⊙ Goal (active, 3/20 turns): Ship it")
          '(:goal (:status "active" :running t :turns-used 3 :max-turns 20))))
  (should
   (equal (hermes-chat--dashboard-goal-status-projection
           "⏳ Goal (parked 8s — timer, 3/20 turns): Ship it")
          '(:goal (:status "inactive" :running nil :turns-used 3 :max-turns 20))))
  (should
   (equal (hermes-chat--dashboard-goal-status-projection
           "No active goal. Set one with /goal <text>.")
          '(:goal nil)))
  (should-not
   (hermes-chat--dashboard-goal-status-projection "Unrecognized response")))

(ert-deftest hermes-chat-dashboard-refreshes-goal-from-vanilla-hermes ()
  "Goal refresh queries the owning vanilla session and updates its header state."
  (let ((client (hermes-test--dashboard-client)) dispatch)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client name arg &rest args)
                 (setq dispatch (list name arg args)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-live")
       (hermes-chat--dashboard-refresh-goal)
       (should (equal (car dispatch) "goal"))
       (should (equal (cadr dispatch) "status"))
       (should (equal (plist-get (caddr dispatch) :session-id) "sid-live"))
       (funcall (plist-get (caddr dispatch) :resolve)
                '((type . "exec")
                  (output . "⊙ Goal (active, 4/20 turns): Ship it")))
       (should (equal hermes-chat--goal
                      '(:status "active" :running t
                        :turns-used 4 :max-turns 20)))))))

(ert-deftest hermes-chat-dashboard-ignores-stale-goal-refresh ()
  "A goal status response cannot update a successor session."
  (let ((client (hermes-test--dashboard-client)) resolve)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client _name _arg &rest args)
                 (setq resolve (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--goal '(:running nil))
       (hermes-chat--dashboard-refresh-goal)
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve
                '((type . "exec")
                  (output . "⊙ Goal (active, 4/20 turns): Ship it")))
       (should (equal hermes-chat--goal '(:running nil)))))))

(ert-deftest hermes-chat-dashboard-record-session-refreshes-vanilla-goal ()
  "Attaching a session requests vanilla Hermes goal state once."
  (let ((client (hermes-test--dashboard-client)) (refreshes 0))
    (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal)
               (lambda () (setq refreshes (1+ refreshes)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client)
       (hermes-chat--dashboard-record-session
        client '((session_id . "sid-live")))
       (should (= refreshes 1))))))

(ert-deftest hermes-chat-dashboard-record-session-does-not-mutate-shared-client ()
  "Recording a session result updates buffer-local vars only, never the client."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client)))
    (hermes-test-with-chat-buffer
     (hermes-chat--dashboard-set-subscriber
      client (lambda (_event)))
     (setq hermes-chat--dashboard-token
           (hermes-dashboard-transport-subscribe
            client (lambda (_event))))
     (hermes-chat--dashboard-record-session
      client '((session_id . "sid-live")
               (stored_session_id . "sid-stored")))
     (should (equal hermes-chat--dashboard-active-session-id "sid-live"))
     (should (equal hermes-chat--session-id "sid-stored"))
     (should hermes-chat--dashboard-session-ready-p)
     (should-not (hermes-dashboard-transport-client-session-id client))
     (should-not (hermes-dashboard-transport-client-stored-session-id client)))))

(ert-deftest hermes-chat-dashboard-record-session-binds-subscriber-token ()
  "Recording a session binds the buffer's subscriber token to its session id."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client)))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-token
           (hermes-dashboard-transport-subscribe
            client (lambda (_event))))
     (hermes-chat--dashboard-record-session
      client '((session_id . "sid-route")))
     (should (eq (gethash "sid-route"
                          (hermes-dashboard-transport-client-session-index
                           client))
                 hermes-chat--dashboard-token)))))

(ert-deftest hermes-chat-dashboard-shared-client-keeps-buffer-local-session-ids ()
  "Two buffers sharing one client keep independent buffer-local session ids."
  :tags '(shared-socket-isolation)
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        shared buf-a buf-b)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :callback (plist-get args :callback)))))
      (unwind-protect
          (progn
            (setq buf-a (generate-new-buffer (hermes-test--chat-buffer-name))
                  buf-b (generate-new-buffer (hermes-test--chat-buffer-name)))
            (with-current-buffer buf-a
              (hermes-chat-mode)
              (setq shared (hermes-chat--dashboard-start
                            (lambda (_event)))
                    hermes-chat--dashboard-token
                    (hermes-dashboard-transport-subscribe
                     shared (lambda (_event))))
              (hermes-chat--dashboard-record-session
               shared '((session_id . "sid-a"))))
            (with-current-buffer buf-b
              (hermes-chat-mode)
              (hermes-chat--dashboard-start (lambda (_event)))
              (setq hermes-chat--dashboard-token
                    (hermes-dashboard-transport-subscribe
                     shared (lambda (_event))))
              (hermes-chat--dashboard-record-session
               shared '((session_id . "sid-b"))))
            ;; Buffer B's later record must not clobber buffer A.
            (with-current-buffer buf-a
              (should (equal hermes-chat--dashboard-active-session-id
                             "sid-a")))
            (with-current-buffer buf-b
              (should (equal hermes-chat--dashboard-active-session-id
                             "sid-b")))
            ;; The shared client holds no ambient session identity.
            (should-not (hermes-dashboard-transport-client-session-id
                         shared)))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest hermes-chat-dashboard-model-switch-targets-current-buffer-session ()
  "A model switch from buffer A targets A's session, not the last recorded one."
  :tags '(shared-socket-isolation)
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        config-session shared buf-a buf-b)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :callback (plist-get args :callback))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq config-session (plist-get args :session-id))
                 (funcall (plist-get args :resolve) '((ok . t)))))
              ((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((providers .
                             (((slug . "p")
                               (authenticated . t)
                               (models . ("m")))))))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (unwind-protect
          (progn
            (setq buf-a (generate-new-buffer (hermes-test--chat-buffer-name))
                  buf-b (generate-new-buffer (hermes-test--chat-buffer-name)))
            (with-current-buffer buf-a
              (hermes-chat-mode)
              (setq shared (hermes-chat--dashboard-start
                            (lambda (_event)))
                    hermes-chat--dashboard-client shared
                    hermes-chat--dashboard-active-session-id "sid-a"
                    hermes-chat--dashboard-session-ready-p t))
            (with-current-buffer buf-b
              (hermes-chat-mode)
              (hermes-chat--dashboard-start (lambda (_event)))
              (setq hermes-chat--dashboard-client shared
                    hermes-chat--dashboard-active-session-id "sid-b"
                    hermes-chat--dashboard-session-ready-p t))
            (with-current-buffer buf-a
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_items &rest _) "p · m")))
                (hermes-chat-switch-model)))
            ;; Buffer A's switch must carry A's session id.
            (should (equal config-session "sid-a")))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest hermes-chat-dashboard-create-seeds-runtime-before-config-gate ()
  "Fresh creation seeds customized runtime before the post-create gate."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client))
        create-args config-calls order)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-args args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (push (list key value (plist-get args :session-id))
                       config-calls)
                 (push 'config order)
                 (funcall (plist-get args :resolve) `((key . ,key)))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) (push 'submit order) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-model "gpt-5"
               hermes-chat--dashboard-create-provider "openai"
               hermes-chat--dashboard-create-reasoning-effort "high")
         (insert "hi")
         (hermes-chat-send)
         (should (equal (plist-get create-args :model) "gpt-5"))
         (should (equal (plist-get create-args :provider) "openai"))
         (should (equal (plist-get create-args :reasoning-effort) "high"))
         (should (equal config-calls
                        '(("reasoning" "high" "sid-new")
                          ("model" "gpt-5 --provider openai" "sid-new"))))
         (should (equal (reverse order) '(config config submit)))
         (should-not hermes-chat--dashboard-create-model)
         (should-not hermes-chat--dashboard-create-provider))))))

(ert-deftest hermes-chat-dashboard-first-build-uses-draft-model ()
  "The deferred agent build snapshots the draft model during `session.create'."
  (let ((client (hermes-test--dashboard-client)) built-model prompt-model)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq built-model (or (plist-get args :model) "profile-default"))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key _value &rest args)
                 (funcall (plist-get args :resolve) `((key . ,key)))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) (setq prompt-model built-model))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-model "grok-4.6"
               hermes-chat--dashboard-create-provider "xai-oauth")
         (insert "hello")
         (hermes-chat-send)
         (should (equal prompt-model "grok-4.6")))))))


(ert-deftest hermes-chat-dashboard-create-confirms-model-before-first-prompt ()
  "An expensive pre-session model is confirmed before `prompt.submit'."
  (let ((client (hermes-test--dashboard-client)) calls order)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               #'hermes-test--resolve-new-dashboard-session)
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (hermes-test--confirming-config-set
                (lambda (key value args)
                  (push (list key value (plist-get args :session-id)
                              (plist-get args :confirm-expensive-model)) calls)
                  (push 'config order))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) (push 'submit order))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-model "gpt-expensive"
               hermes-chat--dashboard-create-provider "openai"
               hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (insert "hi")
         (hermes-chat-send)
         (should (equal (reverse calls)
                        '(("model" "gpt-expensive --provider openai" "sid-new" nil)
                          ("model" "gpt-expensive --provider openai" "sid-new" t)
                          ("reasoning" "high" "sid-new" nil)
                          ("fast" "fast" "sid-new" nil))))
         (should (equal (reverse order)
                        '(config config config config submit))))))))

(ert-deftest hermes-chat-dashboard-control-decline-retries-on-next-action ()
  "A control-action decline preserves the model for an owned retry."
  (let ((client (hermes-test--dashboard-client))
        (answers '(nil t)) action rejected calls create-model)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-model (plist-get args :model))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (pop answers)))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (hermes-test--confirming-config-set
                (lambda (_key _value args)
                  (push (plist-get args :confirm-expensive-model) calls)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq action t))
        (lambda (message) (setq rejected message)))
       (should-not action)
       (should rejected)
       (should (equal create-model "gpt-expensive"))
       (should hermes-chat--dashboard-create-model)
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq action t)))
       (should action)
       (should (equal (reverse calls) '(nil nil t)))
       (should-not hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-retry-overrides-are-non-replacing ()
  "Concurrent retry callers preserve the first owner and send one batch."
  (let ((client (hermes-test--dashboard-client))
        calls resolve first-action second-action first-reject second-reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0))
                       resolve (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid"
             hermes-chat--dashboard-create-fast-p t
             hermes-chat--create-overrides-retry-session-id "sid")
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq first-action t))
        (lambda (message) (setq first-reject message)))
       (let ((owner hermes-chat--create-override-owner))
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq second-action t))
          (lambda (message) (setq second-reject message)))
         (should (eq hermes-chat--create-override-owner owner)))
       (should (= calls 1))
       (should-not first-action)
       (should-not first-reject)
       (should-not second-action)
       (should (string-match-p "configuration is in progress" second-reject))
       (funcall resolve '((key . "fast")))
       (should first-action)
       (should-not second-action)
       (should-not hermes-chat--create-override-owner)
       (should-not hermes-chat--dashboard-create-fast-p)
       (should-not hermes-chat--create-overrides-retry-session-id)))))

(ert-deftest hermes-chat-dashboard-fresh-overrides-reject-late-control ()
  "A control caller cannot bypass an attached fresh-session override batch."
  (let ((client (hermes-test--dashboard-client))
        calls resolve first-action second-action first-reject second-reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve) '((session_id . "sid")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0))
                       resolve (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-create-fast-p t)
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq first-action t))
        (lambda (message) (setq first-reject message)))
       (let ((owner hermes-chat--create-override-owner))
         (should owner)
         (should-not hermes-chat--create-overrides-retry-session-id)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq second-action t))
          (lambda (message) (setq second-reject message)))
         (should (eq hermes-chat--create-override-owner owner)))
       (should (= calls 1))
       (should hermes-chat--dashboard-create-fast-p)
       (should-not first-action)
       (should-not first-reject)
       (should-not second-action)
       (should (equal second-reject "Session setup is in progress"))
       (funcall resolve '((key . "fast")))
       (should first-action)
       (should-not first-reject)
       (should-not second-action)
       (should-not hermes-chat--create-override-owner)
       (should-not hermes-chat--dashboard-create-fast-p)))))

(ert-deftest hermes-chat-dashboard-remote-create-uses-optional-default-cwd ()
  "Prompt and control creation use the gateway default when it is available."
  (dolist (path '(prompt control))
    (dolist (outcome '(success reject signal malformed))
      (let ((client (hermes-test--dashboard-client))
            api-calls create-cwd action)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                   (lambda (&rest _) client))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (&rest _)
                     (setq api-calls (1+ (or api-calls 0)))
                     (pcase outcome
                       ('success (hermes--promise-resolved
                                  '((cwd . "/srv/default"))))
                       ('reject (hermes--promise-rejected "missing"))
                       ('signal (error "missing"))
                       ('malformed (hermes--promise-resolved '((cwd . "")))))))
                  ((symbol-function 'hermes-dashboard-transport-session-create)
                   (lambda (_client &rest args)
                     (setq create-cwd (plist-get args :cwd))
                     (funcall (plist-get args :resolve) '((session_id . "sid")))))
                  ((symbol-function 'hermes-dashboard-transport-prompt-submit)
                   (lambda (&rest _) (setq action t)))
                  ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
          (let ((hermes-transport-send-function #'hermes-transport-send))
            (hermes-test-with-chat-buffer
             (setq-local hermes-chat--resolved-start-mode 'remote)
             (setq default-directory "/tmp/editor/"
                   hermes-chat--working-directory nil)
             (pcase path
               ('prompt (insert "hi") (hermes-chat-send))
               ('control
                (setq hermes-chat--dashboard-client client)
                (hermes-chat--dashboard-ensure-session-action
                 client (current-buffer) (lambda (_client) (setq action t)))))
             (should (= api-calls 1))
             (should (equal create-cwd
                            (and (eq outcome 'success) "/srv/default")))
             (should (equal hermes-chat--working-directory create-cwd))
             (should (equal default-directory "/tmp/editor/"))
             (should action))))))))

(ert-deftest hermes-chat-dashboard-fresh-reservation-rejects-each-phase ()
  "One control owner spans preflight, create, and override settlement."
  (let ((client (hermes-test--dashboard-client))
        (preflight (hermes--promise-make))
        api-calls create-calls config-calls create-resolve config-resolve
        actions rejections)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (setq api-calls (1+ (or api-calls 0)))
                 preflight))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-calls (1+ (or create-calls 0))
                       create-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq config-calls (1+ (or config-calls 0))
                       config-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
      (hermes-test-with-chat-buffer
       (setq-local hermes-chat--resolved-start-mode 'remote)
       (setq hermes-chat--dashboard-client client
             hermes-chat--working-directory nil
             hermes-chat--dashboard-create-fast-p t)
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer)
        (lambda (_client)
          (setq actions (1+ (or actions 0)))
          (error "continuation boom"))
        (lambda (message) (push message rejections)))
       (should (= api-calls 1))
       (hermes-chat--dashboard-ensure-session
        client "late prompt" (current-buffer) nil
        (lambda (message) (push message rejections)))
       (hermes--promise-resolve preflight '((cwd . "/srv/default")))
       (should (= create-calls 1))
       (hermes-chat--dashboard-ensure-session
        client "late queued" (current-buffer) nil
        (lambda (message) (push message rejections)) t)
       (funcall create-resolve '((session_id . "sid")))
       (should (= config-calls 1))
       (hermes-chat--dashboard-ensure-session
        client "late attached prompt" (current-buffer) nil
        (lambda (message) (push message rejections)))
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) #'ignore (lambda (message) (push message rejections)))
       (should (= (length rejections) 4))
       (should (cl-every (lambda (message)
                           (equal message "Session setup is in progress"))
                         rejections))
       (should (= create-calls 1))
       (should (= config-calls 1))
       (should-not actions)
       (funcall config-resolve '((key . "fast")))
       (should (= actions 1))
       (should (equal (car rejections) "continuation boom"))
       (should-not (bound-and-true-p hermes-chat--session-bootstrap))
       (should-not hermes-chat--create-override-owner)))))

(ert-deftest hermes-chat-dashboard-create-failure-settles-origin ()
  "Sync and async create failures settle prompt and control origins once."
  (dolist (path '(prompt control))
    (dolist (preflight '(resolve reject))
      (dolist (failure '(sync async))
        (let ((client (hermes-test--dashboard-client))
              action rejected create-reject)
          (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                     (lambda (&rest _) client))
                    ((symbol-function 'hermes-dashboard-transport-api-request-async)
                     (lambda (&rest _)
                       (if (eq preflight 'resolve)
                           (hermes--promise-resolved '((cwd . "/srv/default")))
                         (hermes--promise-rejected "missing"))))
                    ((symbol-function 'hermes-dashboard-transport-session-create)
                     (lambda (_client &rest args)
                       (if (eq failure 'sync)
                           (error "create boom")
                         (setq create-reject (plist-get args :reject)))))
                    ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
            (let ((hermes-transport-send-function #'hermes-transport-send))
              (hermes-test-with-chat-buffer
               (setq-local hermes-chat--resolved-start-mode 'remote)
               (setq hermes-chat--dashboard-client client
                     hermes-chat--working-directory nil)
               (pcase path
                 ('prompt (insert "recover me") (hermes-chat-send))
                 ('control
                  (hermes-chat--dashboard-ensure-session-action
                   client (current-buffer) (lambda (_client) (setq action t))
                   (lambda (message) (push message rejected)))))
               (when (eq failure 'async)
                 (funcall create-reject "create boom")
                 (funcall create-reject "duplicate boom"))
               (pcase path
                 ('prompt
                  (should (string-match-p "create boom" (buffer-string)))
                  (should-not hermes-chat--pending-assistant-id)
                  (should-not hermes-chat--unsettled-submit-context))
                 ('control
                  (should (equal rejected '("create boom")))
                  (should-not action)))
               (should-not (bound-and-true-p hermes-chat--session-bootstrap))))))))))

(ert-deftest hermes-chat-dashboard-reset-cancels-each-bootstrap-phase ()
  "Reset cancels preflight, create, and override owners before late callbacks."
  (dolist (phase '(preflight create override))
    (let ((client (hermes-test--dashboard-client))
          (preflight (hermes--promise-make))
          create-resolve config-resolve action rejected)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (&rest _) preflight))
                ((symbol-function 'hermes-dashboard-transport-session-create)
                 (lambda (_client &rest args)
                   (setq create-resolve (plist-get args :resolve))))
                ((symbol-function 'hermes-dashboard-transport-config-set)
                 (lambda (_client _key _value &rest args)
                   (setq config-resolve (plist-get args :resolve))))
                ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
        (hermes-test-with-chat-buffer
         (setq-local hermes-chat--resolved-start-mode 'remote)
         (setq hermes-chat--dashboard-client client
               hermes-chat--working-directory nil
               hermes-chat--dashboard-create-fast-p t)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t))
          (lambda (message) (setq rejected message)))
         (when (memq phase '(create override))
           (hermes--promise-resolve preflight '((cwd . "/srv/default"))))
         (when (eq phase 'override)
           (funcall create-resolve '((session_id . "sid"))))
         (should (bound-and-true-p hermes-chat--session-bootstrap))
         (should (eq (plist-get hermes-chat--session-bootstrap :phase) phase))
         (hermes-chat--reset-transcript)
         (pcase phase
           ('preflight
            (hermes--promise-resolve preflight '((cwd . "/srv/late"))))
           ('create (funcall create-resolve '((session_id . "late"))))
           ('override (funcall config-resolve '((key . "fast")))))
         (should-not action)
         (should-not rejected)
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not (bound-and-true-p hermes-chat--session-bootstrap))
         (should-not hermes-chat--create-override-owner))))))

(ert-deftest hermes-chat-dashboard-stale-model-result-does-not-prompt ()
  "A model result for a replaced session rejects without prompting."
  (let ((client (hermes-test--dashboard-client)) resolve prompted continued rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq resolve (plist-get args :resolve))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (setq prompted t) t)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-apply-create-overrides
        client (lambda () (setq continued t))
        hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve '((confirm_required . t)))
       (should-not prompted)
       (should-not continued)
       (should rejected)
       (should-not hermes-chat--create-override-owner)
       (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
       (should hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-model-confirmation-revalidates-after-prompt ()
  "Replacing the session during confirmation prevents the confirmed retry."
  (let ((client (hermes-test--dashboard-client)) calls continued rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0)))
                 (funcall (plist-get args :resolve) '((confirm_required . t)))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (setq hermes-chat--dashboard-active-session-id "sid-new")
                 t)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-apply-create-overrides
        client (lambda () (setq continued t))
        hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
       (should (= calls 1))
       (should-not continued)
       (should rejected)
       (should-not hermes-chat--create-override-owner)
       (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
       (should hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-repeated-model-confirmation-rejects ()
  "A confirmed retry that still requires confirmation rejects without continuing."
  (let ((client (hermes-test--dashboard-client)) calls continued rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0)))
                 (funcall (plist-get args :resolve) '((confirm_required . t)))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-apply-create-overrides
        client (lambda () (setq continued t))
        hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
       (should (= calls 2))
       (should-not continued)
       (should rejected)
       (should-not hermes-chat--create-override-owner)
       (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
       (should hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-confirmed-retry-sync-failure-settles-origin ()
  "A synchronous confirmed-retry error or quit releases exact ownership."
  (dolist (signal '(error quit))
    (let ((client (hermes-test--dashboard-client)) calls resolve rejected)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
                 (lambda (_client _key _value &rest args)
                   (setq calls (1+ (or calls 0)))
                   (if (= calls 1)
                       (setq resolve (plist-get args :resolve))
                     (signal signal nil))))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-session-ready-p t
               hermes-chat--dashboard-active-session-id "sid-old"
               hermes-chat--dashboard-create-model "gpt-expensive")
         (hermes-chat--dashboard-apply-create-overrides
          client (lambda () (ert-fail "failure must not continue"))
          hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
         (funcall resolve '((confirm_required . t)))
         (should (= calls 2))
         (should rejected)
         (should-not hermes-chat--create-override-owner)
         (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
         (should hermes-chat--dashboard-create-model))))))

(ert-deftest hermes-chat-dashboard-create-uses-chat-working-directory ()
  "A fresh session starts in its gateway working directory."
  (let ((client (hermes-test--dashboard-client)) create-cwd)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-cwd (plist-get args :cwd))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq default-directory "/tmp/local-editor/"
               hermes-chat--working-directory "/srv/remote-project")
         (insert "hi")
         (hermes-chat-send)
         (should (equal create-cwd "/srv/remote-project")))))))

(ert-deftest hermes-chat-dashboard-remote-create-omits-unknown-cwd ()
  "A detached remote chat never submits its editor directory as gateway cwd."
  (let ((client (hermes-test--dashboard-client)) create-args)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) (hermes--promise-rejected "unavailable")))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-args args)
                 (funcall (plist-get args :resolve) '((session_id . "sid")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq-local hermes-chat--resolved-start-mode 'remote)
         (setq default-directory "/tmp/local-editor/"
               hermes-chat--working-directory nil)
         (insert "hi")
         (hermes-chat-send)
         (should create-args)
         (should-not (plist-get create-args :cwd))
         (should (equal default-directory "/tmp/local-editor/")))))))

(ert-deftest hermes-chat-set-directory-converges-after-session-info ()
  "The current response adopts cwd after passive gateway projection."
  (dolist (project-root '(nil "/tmp/project-a/"))
    (let* ((client (hermes-test--dashboard-client))
           (editor-directory (or project-root "/tmp/local-editor/"))
           resolve request)
      (hermes-test-with-chat-buffer
       (setq-local hermes-chat--resolved-start-mode 'remote)
       (setq default-directory editor-directory
             hermes-chat--launch-project-root project-root
             hermes-chat--working-directory "/srv/old"
             hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid")
       (hermes-chat--refresh-buffer-name)
       (let ((initial-name (buffer-name))
             (event-callback
              (hermes-chat--transport-callback
               (current-buffer) nil t hermes-chat--transport-generation)))
         (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                    (lambda (_client cwd &rest args)
                      (setq request (list cwd (plist-get args :session-id))
                            resolve (plist-get args :resolve)))))
           (hermes-chat-set-directory "C:/project")
           (should (equal request '("C:/project" "sid")))
           (funcall event-callback
                    '(:type status :event "session.info" :status "ready"
                            :session-id "sid" :cwd "/mnt/c/translated"))
           (should (equal hermes-chat--working-directory
                          "/mnt/c/translated"))
           (should (equal default-directory editor-directory))
           (should (string-match-p
                    "translated" (hermes-test--header-line-string)))
           (if project-root
               (should (equal (buffer-name) initial-name))
             (should (string-match-p "\\[translated\\]" (buffer-name))))
           (funcall resolve '((cwd . "/mnt/c/translated")))
           (should (equal default-directory "/mnt/c/translated/"))
           (should (equal hermes-chat--working-directory
                          "/mnt/c/translated"))
           (if project-root
               (progn
                 (should (equal (buffer-name) initial-name))
                 (should (equal hermes-chat--launch-project-root project-root))
                 (should (equal
                          (hermes-chat--project-buffers
                           project-root (list (current-buffer)))
                          (list (current-buffer)))))
             (should (string-match-p "\\[translated\\]" (buffer-name))))))))))

(ert-deftest hermes-chat-set-directory-separates-event-response-ownership ()
  "A replacement blocks stale event or response effects at its own boundary."
  (dolist (schedule '(before-event before-response))
    (let ((client (hermes-test--dashboard-client)) resolve)
      (hermes-test-with-chat-buffer
       (setq-local hermes-chat--resolved-start-mode 'remote)
       (setq default-directory "/tmp/local-editor/"
             hermes-chat--working-directory "/srv/old"
             hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old")
       (hermes-chat--refresh-buffer-name)
       (let ((initial-name (buffer-name))
             (event-callback
              (hermes-chat--transport-callback
               (current-buffer) nil t hermes-chat--transport-generation)))
         (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                    (lambda (_client _cwd &rest args)
                      (setq resolve (plist-get args :resolve)))))
           (hermes-chat-set-directory "C:/project")
           (when (eq schedule 'before-event)
             (setq hermes-chat--dashboard-active-session-id "sid-new"))
           (funcall event-callback
                    '(:type status :event "session.info" :status "ready"
                            :session-id "sid-old" :cwd "/mnt/c/translated"))
           (when (eq schedule 'before-response)
             (setq hermes-chat--dashboard-active-session-id "sid-new"))
           (funcall resolve '((cwd . "/mnt/c/translated")))
           (should (equal default-directory "/tmp/local-editor/"))
           (pcase schedule
             ('before-event
              (should (equal hermes-chat--working-directory "/srv/old"))
              (should (equal (buffer-name) initial-name)))
             ('before-response
              (should (equal hermes-chat--working-directory
                             "/mnt/c/translated"))
              (should (string-match-p
                       "\\[translated\\]" (buffer-name)))))))))))

(ert-deftest hermes-chat-unknown-remote-directory-starts-with-manual-entry ()
  "A detached remote chat asks for a gateway path without listing a local path."
  (let ((client (hermes-test--dashboard-client)) prompt-default set-cwd)
    (hermes-test-with-chat-buffer
     (setq-local hermes-chat--resolved-start-mode 'remote)
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory nil
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _) (ert-fail "unknown cwd must not be listed")))
               ((symbol-function 'read-string)
                (lambda (_prompt initial &rest _)
                  (setq prompt-default initial)
                  "/srv/manual"))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client cwd &rest args)
                  (setq set-cwd cwd)
                  (funcall (plist-get args :resolve) `((cwd . ,cwd))))))
       (hermes-chat-set-directory)
       (should (equal prompt-default ""))
       (should (equal set-cwd "/srv/manual"))
       (should (equal default-directory "/srv/manual/"))))))

(ert-deftest hermes-chat-set-directory-uses-authoritative-backend-path ()
  "Changing directory applies the authoritative backend path to the chat."
  (let ((client (hermes-test--dashboard-client))
        request)
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory "/srv/old"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client cwd &rest args)
                  (setq request (list cwd (plist-get args :session-id)))
                  (funcall (plist-get args :resolve)
                           '((cwd . "/mnt/c/translated"))))))
       (hermes-chat-set-directory "C:/project")
       (should (equal request '("C:/project" "sid")))
       (should (equal hermes-chat--working-directory "/mnt/c/translated"))
       (should (equal default-directory "/mnt/c/translated/"))
       (should (string-match-p "\[translated\]" (buffer-name)))))))

(ert-deftest hermes-chat-directory-parent-handles-instance-path-syntax ()
  "Parent navigation is lexical for Unix and Windows instance paths."
  (should (equal (hermes-chat--directory-parent "/srv/project/") "/srv"))
  (should (equal (hermes-chat--directory-parent "/srv") "/"))
  (should (equal (hermes-chat--directory-parent "/") "/"))
  (should (equal (hermes-chat--directory-parent "C:\\Users\\Thanos")
                 "C:\\Users"))
  (should (equal (hermes-chat--directory-parent "C:\\Users") "C:\\"))
  (should (equal (hermes-chat--directory-parent "C:\\") "C:\\")))

(ert-deftest hermes-chat-set-directory-ignores-stale-rejection ()
  "A directory rejection cannot surface in a successor session."
  (let ((client (hermes-test--dashboard-client)) reject surfaced)
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client _cwd &rest args)
                  (setq reject (plist-get args :reject))))
               ((symbol-function 'hermes-chat--command-error)
                (lambda (message) (setq surfaced message))))
       (hermes-chat-set-directory "/srv/new")
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall reject "old session rejected")
       (should-not surfaced)
       (should (equal hermes-chat--working-directory "/srv"))
       (should (equal default-directory "/tmp/local-editor/"))))))

(ert-deftest hermes-chat-set-directory-ignores-stale-resolution ()
  "A directory response cannot change a successor session's local context."
  (let ((client (hermes-test--dashboard-client)) resolve)
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client _cwd &rest args)
                  (setq resolve (plist-get args :resolve)))))
       (hermes-chat-set-directory "/srv/new")
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve '((cwd . "/srv/new")))
       (should (equal hermes-chat--working-directory "/srv"))
       (should (equal default-directory "/tmp/local-editor/"))))))

(ert-deftest hermes-chat-dashboard-record-session-records-authoritative-cwd ()
  "Session creation records nested backend cwd without changing editor cwd."
  (let ((client (hermes-test--dashboard-client)))
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/")
     (hermes-chat--dashboard-record-session
      client '((session_id . "sid") (info . ((cwd . "/srv/project")))))
     (should (equal hermes-chat--working-directory "/srv/project"))
     (should (equal default-directory "/tmp/local-editor/")))))

(ert-deftest hermes-chat-set-directory-browses-instance-filesystem ()
  "Interactive directory selection walks `/api/fs/list' on the owning instance."
  (let ((client (hermes-test--dashboard-client)) requests set-cwd)
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (method path &rest args)
                  (let ((directory (cdr (assq 'path (plist-get args :query)))))
                    (push (list method path directory (plist-get args :client))
                          requests)
                    (hermes--promise-resolved
                     (if (equal directory "/srv")
                         '((entries . (((name . "project")
                                        (path . "/srv/project")
                                        (isDirectory . t))
                                       ((name . "README")
                                        (path . "/srv/README")
                                        (isDirectory . :false)))))
                       '((entries . ())))))))
               ((symbol-function 'completing-read)
                (lambda (_prompt candidates &rest _)
                  (car (seq-find
                        (lambda (candidate)
                          (let ((choice (cdr candidate)))
                            (if (= (length requests) 1)
                                (and (eq (plist-get choice :action) 'browse)
                                     (equal (plist-get choice :path)
                                            "/srv/project"))
                              (eq (plist-get choice :action) 'select))))
                        candidates))))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client cwd &rest args)
                  (setq set-cwd (list cwd (plist-get args :session-id)))
                  (funcall (plist-get args :resolve)
                           '((cwd . "/srv/project"))))))
       (hermes-chat-set-directory)
       (should (equal (reverse requests)
                      (list (list "GET" "/api/fs/list" "/srv" client)
                            (list "GET" "/api/fs/list" "/srv/project" client))))
       (should (equal set-cwd '("/srv/project" "sid")))
       (should (equal hermes-chat--working-directory "/srv/project"))
       (should (equal default-directory "/srv/project/"))))))

(ert-deftest hermes-chat-directory-browser-falls-back-to-manual-path ()
  "An unavailable listing endpoint still accepts an instance-native path."
  (let ((client (hermes-test--dashboard-client)) prompt-default set-cwd)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _) (hermes--promise-rejected "404 not found")))
               ((symbol-function 'read-string)
                (lambda (_prompt initial &rest _)
                  (setq prompt-default initial)
                  "/opt/manual"))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client cwd &rest args)
                  (setq set-cwd (list cwd (plist-get args :session-id)))
                  (funcall (plist-get args :resolve)
                           '((cwd . "/opt/manual"))))))
       (hermes-chat-set-directory)
       (should (equal prompt-default "/srv"))
       (should (equal set-cwd '("/opt/manual" "sid")))
       (should (equal hermes-chat--working-directory "/opt/manual"))))))

(ert-deftest hermes-chat-directory-browser-ignores-stale-response ()
  "A directory listing cannot prompt or mutate a successor session."
  (let ((client (hermes-test--dashboard-client))
        (promise (hermes--promise-make)) prompted)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _) promise))
               ((symbol-function 'completing-read)
                (lambda (&rest _) (setq prompted t))))
       (hermes-chat-set-directory)
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (hermes--promise-resolve promise '((entries . ())))
       (should-not prompted)
       (should (equal hermes-chat--working-directory "/srv"))))))

(ert-deftest hermes-chat-directory-browser-ignores-session-change-in-completion ()
  "A completion owned by an old session cannot browse or set its successor."
  (let ((client (hermes-test--dashboard-client)) requests set-cwd)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (_method _path &rest args)
                  (push (cdr (assq 'path (plist-get args :query))) requests)
                  (hermes--promise-resolved
                   '((entries . (((name . "project")
                                  (path . "/srv/project")
                                  (isDirectory . t))))))))
               ((symbol-function 'completing-read)
                (lambda (_prompt candidates &rest _)
                  (setq hermes-chat--dashboard-active-session-id "sid-new")
                  (car (seq-find
                        (lambda (candidate)
                          (eq (plist-get (cdr candidate) :action) 'browse))
                        candidates))))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (&rest _) (setq set-cwd t))))
       (hermes-chat-set-directory)
       (should (equal requests '("/srv")))
       (should-not set-cwd)
       (should (equal hermes-chat--working-directory "/srv"))))))

(ert-deftest hermes-chat-directory-browser-ignores-session-change-in-manual-read ()
  "A manual path owned by an old session cannot set its successor."
  (let ((client (hermes-test--dashboard-client)) requests set-cwd)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _)
                  (setq requests (1+ (or requests 0)))
                  (hermes--promise-rejected "listing unavailable")))
               ((symbol-function 'read-string)
                (lambda (&rest _)
                  (setq hermes-chat--dashboard-active-session-id "sid-new")
                  "/opt/project"))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (&rest _) (setq set-cwd t))))
       (hermes-chat-set-directory)
       (should (= requests 1))
       (should-not set-cwd)
       (should (equal hermes-chat--working-directory "/srv"))))))

(ert-deftest hermes-chat-set-directory-requires-chat-buffer ()
  "The globally autoloaded directory command rejects non-chat buffers."
  (with-temp-buffer
    (should-error (hermes-chat-set-directory temporary-file-directory)
                  :type 'user-error)))

(ert-deftest hermes-chat-set-directory-rejects-active-turn ()
  "Changing directory is unavailable while the current turn is active."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-running-p t)
   (should-error (hermes-chat-set-directory "/tmp/") :type 'user-error)))

(ert-deftest hermes-chat-dashboard-create-seeds-reasoning-fast-before-config-set ()
  "Pre-session reasoning and fast picks seed creation before their config gate."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client))
        create-args config-calls)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-args args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (push (list key value (plist-get args :session-id))
                       config-calls)
                 (funcall (plist-get args :resolve) `((key . ,key)))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (insert "hi")
         (hermes-chat-send)
         (should (equal (plist-get create-args :reasoning-effort) "high"))
         (should (eq (plist-get create-args :fast) t))
         (should (equal (reverse config-calls)
                        '(("reasoning" "high" "sid-new")
                          ("fast" "fast" "sid-new"))))
         (should-not hermes-chat--dashboard-create-reasoning-effort)
         (should-not hermes-chat--dashboard-create-fast-p))))))

(ert-deftest hermes-chat-dashboard-sync-override-failure-settles-origin ()
  "Synchronous model and non-model failures release exact override ownership."
  (dolist (spec '((model error) (reasoning quit)))
    (let ((client (hermes-test--dashboard-client)) rejected)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
                 (lambda (&rest _) (signal (cadr spec) nil))))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-session-ready-p t
               hermes-chat--dashboard-active-session-id "sid-old")
         (pcase (car spec)
           ('model (setq hermes-chat--dashboard-create-model "gpt"))
           ('reasoning
            (setq hermes-chat--dashboard-create-reasoning-effort "high")))
         (hermes-chat--dashboard-apply-create-overrides
          client (lambda () (ert-fail "failure must not continue"))
          hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
         (should rejected)
         (should-not hermes-chat--create-override-owner)
         (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
         (should (hermes-chat--dashboard-create-config-cells)))))))

(ert-deftest hermes-chat-dashboard-resume-discards-unowned-create-overrides ()
  "A stored session never receives create overrides without retry provenance."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client))
        resume-model resume-provider resume-reasoning resume-fast configs action)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (setq resume-model (plist-get args :model)
                       resume-provider (plist-get args :provider)
                       resume-reasoning (plist-get args :reasoning-effort)
                       resume-fast (plist-get args :fast))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live")
                            (resumed . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq configs (1+ (or configs 0)))
                 (funcall (plist-get args :resolve) '((key . "unexpected")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "sid-stored"
               hermes-chat--dashboard-create-model "gpt-5"
               hermes-chat--dashboard-create-provider "openai"
               hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (insert "resume me")
         (hermes-chat-send)
         (should-not resume-model)
         (should-not resume-provider)
         (should-not resume-reasoning)
         (should-not resume-fast)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (should-not configs)
         (should-not (hermes-chat--dashboard-create-config-cells)))
        (setq action nil configs nil)
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "sid-stored"
               hermes-chat--dashboard-client client
               hermes-chat--dashboard-create-model "gpt-5"
               hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (setq action nil)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (should-not configs)
         (should-not (hermes-chat--dashboard-create-config-cells)))))))

(ert-deftest hermes-chat-dashboard-resume-restores-pending-batch-clarify ()
  "Session resume renders locked answers and sends only the unanswered question."
  (let ((client (hermes-test--dashboard-client)) resume sent reads)
    (cl-letf (((symbol-function 'hermes-chat--dashboard-start)
               (lambda (&rest _)
                 (setq hermes-chat--dashboard-client client)
                 client))
              ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-notifications-notify) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session &rest args)
                 (setq resume (plist-get args :resolve))))
              ((symbol-function
                'hermes-dashboard-transport-clarify-question-respond)
               (lambda (_client request question answer &optional resolve _reject)
                 (setq sent (list request question answer))
                 (funcall resolve '((status . "ok")))))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (push prompt reads)
                 "Remaining")))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored")
       (hermes-chat--load-session-history (current-buffer))
       (funcall
        resume
        '((session_id . "live") (stored_session_id . "stored")
          (messages . (((role . "assistant") (text . "History"))))
          (pending_clarify
           . ((request_id . "req-batch")
              (questions . [((qid . "q0") (question . "Locked"))
                            ((qid . "q1") (question . "Open"))])
              (answers . ((q0 . "Accepted")))))))
       (should (string-match-p "Answered: Accepted" (buffer-string)))
       (hermes-chat-respond-to-prompt "req-batch")
       (should (equal reads '("Open: ")))
       (should (equal sent '("req-batch" "q1" "Remaining")))))))

(ert-deftest hermes-chat-dashboard-stale-resume-cannot-install-clarify ()
  "A late session-resume result cannot install a prompt after reset."
  (let ((client (hermes-test--dashboard-client)) resume)
    (cl-letf (((symbol-function 'hermes-chat--dashboard-start)
               (lambda (&rest _)
                 (setq hermes-chat--dashboard-client client)
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session &rest args)
                 (setq resume (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored")
       (hermes-chat--load-session-history (current-buffer))
       (hermes-chat--reset-transcript)
       (funcall resume
                '((session_id . "stale")
                  (pending_clarify
                   . ((request_id . "req-stale")
                      (questions . [((qid . "q0") (question . "Stale"))])))))
       (should-not (hermes-chat--pending-prompt-p))
       (should-not (string-match-p "Stale" (buffer-string)))))))

(ert-deftest hermes-chat-dashboard-stale-non-model-override-stays-session-bound ()
  "Late reasoning and fast results settle without mutating their successor."
  (dolist (cell '((reasoning . "high") ("fast" . "fast")))
    (let ((client (hermes-test--dashboard-client)) resolve continued aborted action requests)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
                 (lambda (_client key _value &rest args)
                   (push (list key (plist-get args :session-id)) requests)
                   (unless resolve (setq resolve (plist-get args :resolve))))))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-session-ready-p t
               hermes-chat--dashboard-active-session-id "sid-old")
         (pcase (car cell)
           ('reasoning
            (setq hermes-chat--dashboard-create-reasoning-effort (cdr cell)))
           ("fast" (setq hermes-chat--dashboard-create-fast-p t)))
         (hermes-chat--dashboard-apply-create-overrides
          client (lambda () (setq continued t))
          hermes-chat--lifecycle-generation (lambda (message) (setq aborted message)))
         (setq hermes-chat--dashboard-active-session-id "sid-new")
         (funcall resolve '((key . "stale")))
         (should-not continued)
         (should aborted)
         (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (should (= (length requests) 1))
         (should-not (hermes-chat--dashboard-create-config-cells))
         (should-not hermes-chat--create-overrides-retry-session-id))))))

(ert-deftest hermes-chat-dashboard-reconnected-keeps-each-buffer-detached ()
  "A shared reconnect leaves each buffer's durable session lazy."
  :tags '(shared-socket-isolation)
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        resumed shared buf-a buf-b)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :callback (plist-get args :callback))))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest _args)
                 (push session-id resumed))))
      (unwind-protect
          (progn
            (setq buf-a (generate-new-buffer (hermes-test--chat-buffer-name))
                  buf-b (generate-new-buffer (hermes-test--chat-buffer-name)))
            (with-current-buffer buf-a
              (hermes-chat-mode)
              (setq shared (hermes-chat--dashboard-start
                            (lambda (_event)))
                    hermes-chat--dashboard-client shared
                    hermes-chat--session-id "stored-a"))
            (with-current-buffer buf-b
              (hermes-chat-mode)
              (hermes-chat--dashboard-start (lambda (_event)))
              (setq hermes-chat--dashboard-client shared
                    hermes-chat--session-id "stored-b"))
            ;; Simulate reconnect for each buffer.
            (with-current-buffer buf-a
              (funcall (hermes-chat--transport-callback
                        (current-buffer) "asst-a" t
                        (hermes-chat--next-transport-generation))
                       '(:type status :status "reconnected")))
            (with-current-buffer buf-b
              (funcall (hermes-chat--transport-callback
                        (current-buffer) "asst-b" t
                        (hermes-chat--next-transport-generation))
                       '(:type status :status "reconnected")))
            (should-not resumed)
            (with-current-buffer buf-a
              (should (equal hermes-chat--session-id "stored-a")))
            (with-current-buffer buf-b
              (should (equal hermes-chat--session-id "stored-b"))))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest hermes-work-browser-exact-owner-keys-and-snapshots ()
  "The native list consumes real owner snapshots and never acquires a client."
  (save-window-excursion
    (hermes-test--with-process-wire
      (hermes-chat-work-refresh)
      (hermes-test--work-answer client "{\"active\":[{\"subagent_id\":\"same\",\"owner_agent_session_id\":\"A\",\"status\":\"running\",\"goal\":\"界 50% goal\"}]}")
      (hermes-test--work-answer client "{\"processes\":[{\"session_id\":\"same\",\"status\":\"exited\",\"exit_code\":7,\"command\":\"printf done\",\"cwd\":\"/remote/inert\",\"output_tail\":\"last output\"}]}")
      (let ((chat (current-buffer)) (owner hermes-chat--work-owner)
            (sent (length frames)) view details instance)
        (insert "draft text")
        (backward-char 3)
        (let ((offset (- (point) (hermes-chat--input-position))))
          (unwind-protect
              (cl-letf (((symbol-function 'hermes-browser--run-on-client)
                         (lambda (&rest _) (ert-fail "Work view acquired a client"))))
                (call-interactively (keymap-lookup hermes-chat-mode-map "C-c C-w"))
                (setq view (current-buffer))
                (should (derived-mode-p 'hermes-work-mode))
                (should (eq owner hermes-work--owner))
                (should (= 2 (length tabulated-list-entries)))
                (should (equal (mapcar #'car tabulated-list-entries)
                               '((delegate . "same") (process . "same"))))
                (should (string-match-p "not a full work ledger" (hermes-work--scope-text owner)))
                (should (eq (get-text-property 0 'face (aref (cadar tabulated-list-entries) 0))
                            'hermes-work-running))
                (rename-buffer "*renamed work view*" t)
                (with-current-buffer chat (hermes-chat-work))
                (should (eq view (current-buffer)))
                (goto-char (point-min))
                (search-forward "printf done")
                (call-interactively (keymap-lookup hermes-work-mode-map "RET"))
                (setq details (current-buffer))
                (should (derived-mode-p 'special-mode))
                (should buffer-read-only)
                (should (string-match-p "Backend output tail: last output" (buffer-string)))
                (should (string-match-p "Remote cwd (inert): /remote/inert" (buffer-string)))
                (should-not (next-button (point-min)))
                (should (= sent (length frames)))
                (call-interactively (keymap-lookup (current-local-map) "q"))
                (with-current-buffer view
                  (cl-letf (((symbol-function 'hermes-list-subagents)
                             (lambda () (interactive) (setq instance hermes-instance))))
                    (call-interactively (keymap-lookup hermes-work-mode-map "i")))
                  (should (equal instance (plist-get owner :instance)))
                  (call-interactively (keymap-lookup hermes-work-mode-map "g")))
                (should (= (1+ sent) (length frames)))
                (with-current-buffer chat
                  (should (equal (buffer-substring-no-properties (hermes-chat--input-position) (point-max)) "draft text"))
                  (should (= offset (- (point) (hermes-chat--input-position))))
                  (hermes-test--work-answer client "{\"active\":[]}")
                  (hermes-test--work-answer client "{\"processes\":[{\"session_id\":\"same\",\"status\":\"running\"}]}"))
                (with-current-buffer view
                  (should (equal (tabulated-list-get-id) '(process . "same")))
                  (should (= 1 (length tabulated-list-entries))))
                ;; Replacement must detach, not retarget, even after renaming.
                (with-current-buffer chat (hermes-chat--work-bind client "runtime" "B"))
                (let ((publication
                       (seq-find (lambda (timer)
                                   (and (eq (cadr timer) #'hermes-work--render)
                                        (eq (car (nth 2 timer)) owner))) timers)))
                  (should publication)
                  (apply (cadr publication) (nth 2 publication)))
                (with-current-buffer view
                  (should (string-match-p "Agents stale" (buffer-string)))
                  (should (string-match-p "Stale" (buffer-string)))
                  (should-error (hermes-work-refresh) :type 'user-error)
                  (should-not (keymap-lookup hermes-work-mode-map "k"))))
            (dolist (buffer (list view details))
              (when (buffer-live-p buffer) (kill-buffer buffer)))))))))

(ert-deftest hermes-work-list-only-visibility-and-mode-replacement ()
  "Only the exact list, not details or a reused name/mode, keeps cadence visible."
  (save-window-excursion
    (hermes-test--with-process-wire
      (let ((owner hermes-chat--work-owner) view replacement)
        (unwind-protect
            (progn
              (hermes-chat-work)
              (setq view (current-buffer))
              (delete-other-windows)
              (should (hermes-chat--work-visible-p owner))
              (setq replacement (generate-new-buffer "*work-details-only*"))
              (with-current-buffer replacement (special-mode))
              (set-window-buffer (selected-window) replacement)
              (should-not (hermes-chat--work-visible-p owner))
              (hermes-chat--work-visibility)
              (should-not (plist-get owner :timer))
              (set-window-buffer (selected-window) view)
              (with-current-buffer view (fundamental-mode))
              (should-not (plist-get owner :view))
              (should-not (hermes-chat--work-visible-p owner))
              (with-current-buffer view
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "unrelated replacement")))
              (hermes-work--render owner)
              (should (equal (with-current-buffer view (buffer-string)) "unrelated replacement")))
          (dolist (buffer (list view replacement))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest hermes-work-details-link-and-widths ()
  "Details open the same owner list; columns fit real windows and preserve rows."
  (save-window-excursion
    (hermes-test--with-process-wire
      (hermes-chat-work-refresh)
      (hermes-test--work-answer client "{\"active\":[]}")
      (hermes-test--work-answer client "{\"processes\":[{\"session_id\":\"p\",\"status\":\"running\",\"command\":\"界% very long command with long description\",\"started_at\":0}]}")
      (let ((owner hermes-chat--work-owner) view details)
        (unwind-protect
            (progn
              (hermes-chat-session-details)
              (setq details (get-buffer "*Hermes Session Details*"))
              (with-current-buffer details
                (goto-char (point-min))
                (search-forward "Browse observed work")
                (let ((button (button-at (1- (point)))))
                  (should (equal (button-label button) "Browse observed work"))
                  (button-activate button)))
              (setq view (plist-get owner :view))
              (should (buffer-live-p view))
              (should (eq (window-buffer (selected-window)) view))
              (with-current-buffer view
                (dolist (width '(12 20 30 40 50 80 120))
                  (cl-letf (((symbol-function 'window-body-width)
                             (lambda (&rest _) width)))
                    (hermes-work--resize (selected-window)))
                  (hermes-work--render owner)
                  (goto-char (point-min))
                  (while (and (not (eobp)) (not (tabulated-list-get-id))) (forward-line))
                  (should (equal (tabulated-list-get-id) '(process . "p")))
                  ;; Native tabulated-list truncates with display properties,
                  ;; leaving full cell text available to copying and help.
                  (let ((end (line-end-position)) (columns 0))
                    (while (< (point) end)
                      (let* ((next (next-single-property-change (point) 'display nil end))
                             (display (get-text-property (point) 'display)))
                        (setq columns
                              (pcase display
                                (`(space :align-to ,column) column)
                                ((pred stringp) (+ columns (string-width display)))
                                ('nil (+ columns (string-width
                                                  (buffer-substring-no-properties (point) next))))
                                (_ (ert-fail (format "Unexpected display spec: %S" display)))))
                        (goto-char next)))
                    (should (<= columns width)))))
              ;; Old list render must not write into a successor that steals its slot.
              (with-current-buffer view
                (setq hermes-work--owner nil)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "Successor view content")))
              (let ((before (with-current-buffer view (buffer-string))))
                (hermes-work--render owner)
                (should (equal before (with-current-buffer view (buffer-string))))))
          (dolist (buffer (list view details))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest hermes-work-refresh-publishes-stale-before-reply ()
  "List and owner refresh repaint retained evidence before any response."
  (dolist (entry '(list owner))
    (save-window-excursion
      (hermes-test--with-process-wire
        (hermes-chat-work-refresh)
        (hermes-test--work-answer client "{\"active\":[]}")
        (hermes-test--work-answer client "{\"processes\":[{\"session_id\":\"p\",\"status\":\"running\"}]}")
        (let ((chat (current-buffer)) (owner hermes-chat--work-owner) view details)
          (insert "retained draft")
          (backward-char 3)
          (let ((draft-point (point)))
            (unwind-protect
                (progn
                  (hermes-chat-work)
                  (setq view (current-buffer))
                  (goto-char (point-min))
                  (search-forward "Running")
                  (let ((row (tabulated-list-get-id)) (column (current-column))
                        (window (selected-window)))
                    (if (eq entry 'list)
                        (call-interactively (keymap-lookup hermes-work-mode-map "g"))
                      (funcall (plist-get owner :refresh)))
                    (should (eq (current-buffer) view))
                    (should (eq (selected-window) window))
                    (should (equal row (tabulated-list-get-id)))
                    (should (= column (current-column))))
                  (should (= (length frames) 3))
                  (let ((state (aref (cadar tabulated-list-entries) 0)))
                    (should (equal state "Stale"))
                    (should (eq (get-text-property 0 'face state) 'hermes-work-unknown)))
                  (should (string-match-p "Agents stale" (buffer-string)))
                  (should (string-match-p "Processes stale" (buffer-string)))
                  (hermes-work-refresh)
                  (should (= (length frames) 3))
                  (call-interactively (keymap-lookup hermes-work-mode-map "RET"))
                  (setq details (current-buffer))
                  (should (string-match-p "Observed process — Stale" (buffer-string)))
                  (with-current-buffer chat
                    (should (= draft-point (point)))
                    (should (equal (buffer-substring-no-properties
                                    (hermes-chat--input-position) (point-max)) "retained draft"))
                    (should (equal (hermes-chat--work-label nil) "Work ?"))
                    (should (eq (get-text-property 0 'face (hermes-chat--work-label nil))
                                'hermes-work-unknown))
                    (hermes-test--work-answer client "{\"active\":[]}")
                    (hermes-test--work-answer client "{\"processes\":[{\"session_id\":\"p\",\"status\":\"running\"}]}"))
                  (with-current-buffer view
                    (should (equal (aref (cadar tabulated-list-entries) 0) "Running"))
                    (should (string-match-p "Processes current" (buffer-string)))))
              (dolist (buffer (list view details))
                (when (buffer-live-p buffer) (kill-buffer buffer))))))))))

(ert-deftest hermes-work-refresh-render-reentry-and-exits ()
  "Freshness publication owns its cycle across reentry and nonlocal exits."
  (dolist (action '(refresh replace kill-view error quit))
    (save-window-excursion
      (hermes-test--with-process-wire
        (let ((chat (current-buffer)) (owner hermes-chat--work-owner) view rendered caught)
          (unwind-protect
              (progn
                (hermes-chat-work)
                (setq view (current-buffer))
                (setf (plist-get owner :render)
                      (lambda (target)
                        (setq rendered t)
                        (setf (plist-get target :render) #'hermes-work--render)
                        (pcase action
                          ('refresh (funcall (plist-get target :refresh)))
                          ('replace
                           (with-current-buffer chat
                             (hermes-chat--work-bind client "runtime" "B")
                             (hermes-chat-work-refresh)))
                          ('kill-view (kill-buffer view))
                          ((or 'error 'quit) (signal action '("Render failed"))))
                        (hermes-work--render target)))
                (condition-case condition
                    (funcall (plist-get owner :refresh))
                  ((error quit) (setq caught (car condition))))
                (should rendered)
                (with-current-buffer chat
                  (if (memq action '(error quit))
                      (progn
                        (should (eq caught action))
                        (should-not frames)
                        (should-not (plist-get owner :cycle))
                        (should-not (plist-get owner :request))
                        (setf (plist-get owner :render) #'hermes-work--render)
                        (hermes-chat-work-refresh))
                    (should-not caught))
                  (should (= (length frames) 1))
                  (should (= (hash-table-count
                              (hermes-dashboard-transport-client-pending client)) 1))
                  (when (eq action 'replace)
                    (should-not (eq owner hermes-chat--work-owner))
                    (should (plist-get hermes-chat--work-owner :cycle))
                    (should-not (plist-get owner :request)))
                  (hermes-chat-work-refresh)
                  (should (= (length frames) 1))
                  (hermes-test--work-answer client "{\"active\":[]}")
                  (hermes-test--work-answer client "{\"processes\":[]}")
                  (should-not (plist-get hermes-chat--work-owner :cycle))))
            (when (buffer-live-p view) (kill-buffer view))))))))

(provide 'hermes-chat-dashboard-tests)
;;; hermes-chat-dashboard-tests.el ends here
