;;; hermes-kanban-tests.el --- kanban tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)
(require 'string-edit)

(defun hermes-kanban-test--multiple-backends (terminate &optional phase change native)
  "Exercise a fresh multi-backend chain, optionally retiring at PHASE.
TERMINATE selects run termination instead of creation.  CHANGE names the
retirement; NATIVE uses recursive minibuffers rather than canned answers."
  (let* ((hermes-instances (copy-tree '(("a" . "http://a.invalid") ("b" . "http://b.invalid"))))
         (origin (generate-new-buffer " *kanban multiple*"))
         (buffers (buffer-list))
         (clients (mapcar (lambda (entry)
                            (make-hermes-dashboard-transport-client :base-url (cdr entry) :ready-p t))
                          hermes-instances))
         (text (symbol-function 'read-string))
         (confirm (symbol-function 'yes-or-no-p))
         (choice (symbol-function 'completing-read))
         (choices 0) (acquired 0) (released 0) (pending 0) (auth-count 0)
         requests timers errors prompts
         (retire (lambda ()
                   (with-current-buffer origin
                     (pcase change
                       ('instance (setq hermes-instance (cadr hermes-instances)))
                       ('catalogue (setq hermes-instances (cdr hermes-instances)))
                       ('mode (fundamental-mode))
                       ('claim (setq hermes-buffer--owner (cons t major-mode)))
                       ('generation (hermes-browser--next-request-generation))))))
         (reader (lambda (fn answer args)
                   (push (car args) prompts)
                   (if native
                       (minibuffer-with-setup-hook
                           (lambda ()
                             (when (and (eq phase 'selection) (eq fn choice)) (funcall retire))
                             (delete-minibuffer-contents) (insert answer)
                             (setq unread-command-events (list ?\r)))
                         (apply fn args))
                     (when (and (eq phase 'selection) (eq fn choice)) (funcall retire))
                     (if (eq fn confirm) t answer)))))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest args) (funcall reader text "sample" args)))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest args) (funcall reader confirm "yes" args)))
                  ((symbol-function 'completing-read)
                   (lambda (&rest args)
                     (cl-incf choices)
                     (funcall reader choice (if (= choices 1) "a" "b") args)))
                  ((symbol-function 'hermes-browser--existing-client) (lambda () nil))
                  ((symbol-function 'hermes-dashboard-transport-acquire)
                   (lambda (&rest _)
                     (cl-incf acquired)
                     (when (eq phase 'acquisition) (funcall retire))
                     (or (seq-find (lambda (client)
                                     (equal (hermes-dashboard-transport-client-base-url client)
                                            (hermes-instance-url hermes-instance))) clients)
                         (error "Unknown acquired instance"))))
                  ((symbol-function 'hermes-dashboard-transport-release)
                   (lambda (_) (cl-incf released)))
                  ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                   (lambda (&rest _)
                     (let ((promise (hermes--promise-make)) (number (cl-incf auth-count))
                           (base hermes-dashboard-transport--api-auth-base-url))
                       (cl-incf pending)
                       (push (run-at-time
                              0 nil
                              (lambda ()
                                (when (or (and (eq phase 'auth) (= number 1))
                                          (and (eq phase 'mutation-auth) (= number 2)))
                                  (funcall retire))
                                (hermes--promise-resolve
                                 promise (list :base-url base :session-token "fixture"))
                                (cl-decf pending))) timers)
                       promise)))
                  ((symbol-function 'hermes-dashboard-transport--default-http-request-async)
                   (lambda (url &rest args)
                     (push (list (plist-get args :method) url
                                 (when (plist-get args :data)
                                   (json-parse-string (plist-get args :data) :object-type 'alist
                                                      :false-object :false))) requests)
                     (hermes--promise-resolved
                      '(:body ((boards . nil) (task . ((id . "task/a") (current_run_id . 42)))))))))
          (with-current-buffer origin
            (pcase terminate
              ('detail
               (hermes-kanban-task-mode)
               (setq hermes-kanban-task--board-slug "work"
                     hermes-kanban-task--task-id "task/a"))
              ('diagnostics
               (hermes-kanban-diagnostics-mode)
               (setq hermes-kanban-diagnostics--slug "work"
                     tabulated-list-entries '(("task/a" ["warning" "Task" "-" "Diagnostic"])))
               (tabulated-list-print) (goto-char (point-min)))
              ('t
               (hermes-kanban-mode)
               (setq hermes-kanban--slug "work"
                     tabulated-list-entries '(("task/a" ["running" "0" "-" "Task"])))
               (tabulated-list-print) (goto-char (point-min))))
            (condition-case err
                (call-interactively (if terminate #'hermes-kanban-terminate-run #'hermes-kanban-create-board))
              (user-error (push err errors))))
          (hermes-test--wait-until (lambda () (zerop pending)) nil "Multi-backend chain")
          (unless change (should-not errors))
          (should (= acquired released))
          (list :choices choices :requests (nreverse requests) :prompts (nreverse prompts)))
      (mapc #'cancel-timer timers)
      (when (buffer-live-p origin) (kill-buffer origin))
      (dolist (buffer (seq-difference (buffer-list) buffers))
        (when (and (buffer-live-p buffer)
                   (with-current-buffer buffer (derived-mode-p 'hermes-kanban-boards-mode)))
          (kill-buffer buffer))))))

(defun hermes-kanban-test--assert-multiple-backends (terminate)
  "Check exact creation or TERMINATE requests through native routing."
  (progn
    (let* ((result (hermes-kanban-test--multiple-backends terminate nil nil (not noninteractive)))
           (requests (plist-get result :requests)))
      (should (= (plist-get result :choices) 1))
      (should (equal requests
                     (if terminate
                         (append
                          '(("GET" "http://a.invalid/api/plugins/kanban/tasks/task%2Fa?board=work" nil)
                            ("POST" "http://a.invalid/api/plugins/kanban/runs/42/terminate?board=work" ((reason . "sample"))))
                          (pcase terminate
                            ('detail '(("GET" "http://a.invalid/api/plugins/kanban/tasks/task%2Fa?board=work" nil)))
                            ('diagnostics '(("GET" "http://a.invalid/api/plugins/kanban/diagnostics?board=work" nil)))
                            (_ '(("GET" "http://a.invalid/api/plugins/kanban/board?board=work" nil)
                                 ("GET" "http://a.invalid/api/plugins/kanban/orchestration" nil)))))
                       '(("POST" "http://a.invalid/api/plugins/kanban/boards"
                          ((slug . "sample") (name . "sample") (switch . :false)))
                         ("GET" "http://a.invalid/api/plugins/kanban/boards" nil)))))
      (when terminate
        (should (member "Terminate run #42 of task task/a? " (plist-get result :prompts)))))))

(ert-deftest hermes-kanban-multiple-backends-create-one-choice ()
  (hermes-kanban-test--assert-multiple-backends nil))

(ert-deftest hermes-kanban-multiple-backends-terminate-one-choice ()
  (dolist (view '(t detail diagnostics))
    (hermes-kanban-test--assert-multiple-backends view)))

(ert-deftest hermes-kanban-multiple-backends-retirement ()
  "The selected endpoint never hides retirement of the original origin."
  (dolist (terminate '(nil t))
    (dolist (phase (append '(selection acquisition auth) (and terminate '(mutation-auth))))
      (dolist (change (if (eq phase 'acquisition) '(claim generation) '(instance catalogue mode claim generation)))
        (ert-info ((format "terminate=%s phase=%s change=%s" terminate phase change))
          (let* ((result (hermes-kanban-test--multiple-backends terminate phase change (not noninteractive)))
                 (requests (plist-get result :requests)))
            (should (= (plist-get result :choices) 1))
            (should-not (seq-find (lambda (r) (not (equal (car r) "GET"))) requests))
            (unless (eq phase 'mutation-auth) (should-not requests))))))))

(defconst hermes-kanban-test--siblings
  '(hermes-kanban-rename-board hermes-kanban-set-status
    hermes-kanban-change-assignee hermes-kanban-comment hermes-kanban-reclaim
    hermes-kanban-create-board hermes-kanban-create-task
    hermes-kanban-create-triage-task hermes-kanban-terminate-run))

(defun hermes-kanban-test--sibling-wire (command change &optional legacy phase native)
  "Run COMMAND with CHANGE at PHASE and return serialized requests.
LEGACY uses URL-only routing; `unowned' starts without a claim or instance.
NATIVE uses actual recursive readers.
PHASE is a reader number, `auth', `receipt', or `readback'."
  (let* ((hermes-instances (cond ((eq legacy t) nil)
                                  ((eq legacy 'multiple)
                                   '(("a" . "http://a.invalid") ("b" . "http://b.invalid")))
                                  (t '(("a" . "http://a.invalid")))))
         (hermes-dashboard-transport-url "http://a.invalid/")
         (origin (generate-new-buffer " *kanban sibling*"))
         (foreign (generate-new-buffer " *kanban foreign*"))
         (client (make-hermes-dashboard-transport-client :base-url "http://a.invalid" :ready-p t))
         (text-reader (symbol-function 'read-string))
         (confirm-reader (symbol-function 'yes-or-no-p))
         (completion-reader (symbol-function 'completing-read))
         (body-reader (symbol-function 'read-string-from-buffer))
         (count 0) (choices 0) (writes 0) (pending 0) timers requests errors notices
         (change-owner
          (lambda ()
            (pcase change
              ('instance (setq hermes-instance '("b" . "http://b.invalid")))
              ('legacy (setq hermes-dashboard-transport-url "http://b.invalid"))
              ('row (goto-char (point-max)))
              ('board (setq hermes-kanban--slug "other"))
              ('claim (setq hermes-buffer--owner (cons t major-mode)))
              ('mode (fundamental-mode))
              ('generation (hermes-browser--next-request-generation))
              ('supersede (hermes-kanban--read-and-act #'ignore #'ignore nil))
              ('killed (kill-buffer origin))
              ('replaced (let ((name (buffer-name origin)))
                           (kill-buffer origin)
                           (with-current-buffer foreign (rename-buffer name))))
              ('foreign (set-buffer foreign)))))
         (reader
          (lambda (function answer args &optional body)
            (cl-incf count)
            (if native
                (if body
                    ;; The native string editor enters recursive-edit, not a minibuffer.
                    (let ((timer
                           (run-at-time
                            0 nil (lambda ()
                                    (when (equal count (or phase 1))
                                      (with-current-buffer origin (funcall change-owner)))
                                    (insert answer)
                                    (execute-kbd-macro (kbd "C-c C-c"))))))
                      (unwind-protect (apply function args) (cancel-timer timer)))
                  (minibuffer-with-setup-hook
                      (lambda ()
                        (when (equal count (or phase 1))
                          (with-current-buffer origin (funcall change-owner)))
                        (delete-minibuffer-contents) (insert answer)
                        (setq unread-command-events (list ?\r)))
                    (apply function args)))
              (when (equal count (or phase 1)) (funcall change-owner))
              (if (eq function confirm-reader) t answer)))))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) notices)))
                  ((symbol-function 'hermes-browser--existing-client) (lambda () nil))
                  ((symbol-function 'hermes-dashboard-transport-acquire)
                   (lambda (&rest _)
                     (setf (hermes-dashboard-transport-client-base-url client)
                           (string-remove-suffix "/" (hermes-instance-url hermes-instance))) client))
                  ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
                  ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                   (lambda (&rest _)
                     (let ((promise (hermes--promise-make)))
                       (cl-incf pending)
                       (push (run-at-time
                        0 nil
                        (lambda ()
                          (when (or (and (eq phase 'auth) (= writes 0))
                                    (and (eq phase 'readback) (> writes 0)))
                            (with-current-buffer origin (funcall change-owner)))
                          (hermes--promise-resolve
                           promise (list :base-url (hermes-dashboard-transport-client-base-url client)
                                         :session-token "fixture"))
                          (cl-decf pending))) timers)
                       promise)))
                  ((symbol-function 'hermes-dashboard-transport--default-http-request-async)
                   (lambda (url &rest args)
                     (push (list url (plist-get args :method) (plist-get args :data)) requests)
                     (unless (equal (plist-get args :method) "GET") (cl-incf writes))
                     (when (and (eq phase 'receipt) (> writes 0))
                       (with-current-buffer origin (funcall change-owner)))
                     (hermes--promise-resolved
                      '(:body ((task . ((id . "task/a") (current_run_id . 42))))))))
                  ((symbol-function 'hermes-kanban--profile-candidates) (lambda () '("worker")))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest args) (funcall reader confirm-reader "yes" args)))
                  ((symbol-function 'read-string)
                   (lambda (&rest args) (funcall reader text-reader "renamed" args)))
                  ((symbol-function 'read-string-from-buffer)
                   (lambda (&rest args) (funcall reader body-reader "body α\nnext" args t)))
                  ((symbol-function 'read-number)
                   (lambda (&rest _) (string-to-number (funcall reader text-reader "7" '("Priority: ")))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest args)
                     (funcall reader completion-reader
                              (if (equal (car args) "Hermes instance: ")
                                  (if (= (cl-incf choices) 1) "a" "b")
                                (if (eq command 'hermes-kanban-set-status) "done" "worker")) args))))
          (with-current-buffer origin
            (if (memq command '(hermes-kanban-rename-board hermes-kanban-create-board))
                (progn (hermes-kanban-boards-mode)
                       (setq tabulated-list-entries
                             (hermes-kanban--board-rows '(((slug . "work") (name . "Work"))))))
              (hermes-kanban-mode)
              (setq hermes-kanban--slug "work"
                    tabulated-list-entries '(("task/a" ["todo" "0" "-" "Task"]))))
            (unless (memq legacy '(unowned multiple)) (hermes-buffer--claim major-mode))
            (unless legacy (setq hermes-instance (car hermes-instances)))
            (tabulated-list-print) (goto-char (point-min))
            (condition-case err (call-interactively command)
              (user-error (push err errors))))
          (hermes-test--wait-until (lambda () (zerop pending)) nil "Kanban auth and readback")
          (should (or (> count 0) (and (eq phase 'auth) change)))
          (unless change (should-not errors))
          (when (eq legacy 'multiple) (should (= choices 1)))
          (when (and change (not (eq change 'foreign)) (or (null phase) (numberp phase)))
            (should (= count (or phase 1))))
          (when (eq phase 'receipt)
            (should (= 1 (cl-count-if
                          (lambda (notice) (string-match-p "check board before retrying" notice))
                          notices))))
          (nreverse requests))
      (when (buffer-live-p origin) (kill-buffer origin))
      (mapc #'cancel-timer timers)
      (when (buffer-live-p foreign) (kill-buffer foreign)))))

(ert-deftest hermes-kanban-multiple-backends-sibling-readback ()
  "Every sibling retains its choice through real serialized readback."
  (dolist (command hermes-kanban-test--siblings)
    (let ((requests (hermes-kanban-test--sibling-wire command nil 'multiple nil (not noninteractive))))
      (should (= 1 (cl-count-if (lambda (r) (not (equal (cadr r) "GET"))) requests)))
      (should (seq-every-p (lambda (r) (string-prefix-p "http://a.invalid/" (car r))) requests)))))

(ert-deftest hermes-kanban-sibling-retarget-refuses-before-write ()
  (dolist (command hermes-kanban-test--siblings)
    (ert-info ((symbol-name command))
      (let ((requests (hermes-kanban-test--sibling-wire command 'instance)))
        (should-not (cl-remove-if (lambda (r) (equal (cadr r) "GET")) requests))))))

(ert-deftest hermes-kanban-sibling-original-serialization-and-readback ()
  (dolist (legacy '(nil t unowned multiple))
    (dolist (command hermes-kanban-test--siblings)
      (ert-info ((format "%s legacy=%s" command legacy))
        (let* ((requests (hermes-kanban-test--sibling-wire command nil legacy))
               (writes (cl-remove-if (lambda (r) (equal (cadr r) "GET")) requests))
               (expected
                (pcase command
                  ('hermes-kanban-rename-board '("/boards/work" "PATCH" "{\"name\":\"renamed\"}"))
                  ('hermes-kanban-set-status '("/tasks/task%2Fa?board=work" "PATCH" "{\"status\":\"done\"}"))
                  ('hermes-kanban-change-assignee '("/tasks/task%2Fa?board=work" "PATCH" "{\"assignee\":\"worker\"}"))
                  ('hermes-kanban-comment '("/tasks/task%2Fa/comments?board=work" "POST" "{\"body\":\"body α\\nnext\"}"))
                  ('hermes-kanban-reclaim '("/tasks/task%2Fa/reclaim?board=work" "POST" "{\"reason\":\"renamed\"}"))
                  ('hermes-kanban-create-board '("/boards" "POST" "{\"slug\":\"renamed\",\"name\":\"renamed\",\"switch\":false}"))
                  ('hermes-kanban-create-task '("/tasks?board=work" "POST" "{\"title\":\"renamed\",\"priority\":7,\"body\":\"body α\\nnext\",\"assignee\":\"worker\"}"))
                  ('hermes-kanban-create-triage-task '("/tasks?board=work" "POST" "{\"title\":\"renamed\",\"priority\":7,\"body\":\"body α\\nnext\",\"triage\":true}"))
                  ('hermes-kanban-terminate-run '("/runs/42/terminate?board=work" "POST" "{\"reason\":\"renamed\"}")))))
          (should (= (length writes) 1))
          (should (equal (caar writes)
                         (concat "http://a.invalid/api/plugins/kanban" (car expected))))
          (should (equal (cadar writes) (cadr expected)))
          ;; Emacs 29's native serializer returns characters; newer versions
          ;; return UTF-8 bytes.  Parse the actual JSON, preserving literal
          ;; Unicode content, null/false and the exact set of body fields.
          (should (equal (json-parse-string (caddar writes) :object-type 'alist
                                            :false-object :false :null-object :null)
                         (json-parse-string (caddr expected) :object-type 'alist
                                            :false-object :false :null-object :null)))
          (should (equal (car (cadr (member (car writes) requests)))
                         (concat "http://a.invalid/api/plugins/kanban/"
                                 (if (memq command '(hermes-kanban-rename-board hermes-kanban-create-board))
                                     "boards" "board?board=work")))))))))

(ert-deftest hermes-kanban-sibling-lifecycle-auth-and-readback ()
  (dolist (command hermes-kanban-test--siblings)
    (dolist (phase '(1 auth readback receipt))
      (dolist (change '(instance legacy row claim mode generation killed replaced))
        (ert-info ((format "%s %s %s" command phase change))
          (let* ((requests (hermes-kanban-test--sibling-wire command change (eq change 'legacy) phase))
                 (writes (cl-remove-if (lambda (r) (equal (cadr r) "GET")) requests)))
            (should (= (length writes) (if (memq phase '(readback receipt)) 1 0)))
            (when (memq phase '(readback receipt))
              (should-not (cdr (member (car writes) requests))))))))
    (let ((requests (hermes-kanban-test--sibling-wire command 'foreign)))
      (should (= 1 (cl-count-if (lambda (r) (not (equal (cadr r) "GET"))) requests))))
    (unless (memq command '(hermes-kanban-rename-board hermes-kanban-create-board))
      (should-not (cl-remove-if (lambda (r) (equal (cadr r) "GET"))
                               (hermes-kanban-test--sibling-wire command 'board))))))

(ert-deftest hermes-kanban-sibling-successive-reader-retirement ()
  "Refuse follow-up readers as soon as the original owner retires."
  (dolist (entry '((hermes-kanban-create-board . 2)
                   (hermes-kanban-create-task . 4)
                   (hermes-kanban-create-triage-task . 3)
                   (hermes-kanban-reclaim . 2)
                   (hermes-kanban-terminate-run . 2)))
    (cl-loop for phase from 2 to (cdr entry) do
             (ert-info ((format "%s reader=%s" (car entry) phase))
               (should-not
                (cl-remove-if (lambda (r) (equal (cadr r) "GET"))
                              (hermes-kanban-test--sibling-wire
                               (car entry) 'instance nil phase)))))))

(ert-deftest hermes-kanban-sibling-native-input-ownership ()
  (skip-unless (not noninteractive))
  (dolist (command hermes-kanban-test--siblings)
    (dolist (legacy '(nil t unowned))
      (dolist (change '(nil instance generation mode supersede))
        (ert-info ((format "%s legacy=%s change=%s" command legacy change))
          (let ((requests (hermes-kanban-test--sibling-wire command change legacy nil t)))
            (should (= (cl-count-if (lambda (r) (not (equal (cadr r) "GET"))) requests)
                       (if change 0 1)))))))))

(defun hermes-kanban-test--input-wire (kind change &optional legacy waiting native readback)
  "Invoke KIND with CHANGE during input, returning actual HTTP requests.
LEGACY selects URL-only resolution instead of a named backend.
WAITING delays CHANGE until authentication.  NATIVE uses real minibuffers.
READBACK retains the real board refresh."
  (let* ((hermes-instances (unless legacy '(("a" . "http://a.invalid"))))
         (hermes-dashboard-transport-url "http://a.invalid/")
         (origin (generate-new-buffer " *kanban input*"))
         (foreign (generate-new-buffer " *kanban foreign*"))
         (auth (hermes--promise-make))
         (native-entered 0)
         (render-board (symbol-function 'hermes-kanban--render-board))
         (render-boards (symbol-function 'hermes-kanban--render-boards))
         (confirm (symbol-function 'yes-or-no-p))
         (read-text (symbol-function 'read-string))
         (reader
          (lambda (function answer &rest args)
            (if native
                (minibuffer-with-setup-hook
                    (lambda ()
                      (cl-incf native-entered)
                      (with-current-buffer origin (funcall change origin foreign))
                      (delete-minibuffer-contents)
                      (insert answer)
                      (setq unread-command-events (list ?\r)))
                  (apply function args))
              (unless waiting (funcall change origin foreign))
              answer)))
         (client (make-hermes-dashboard-transport-client
                  :base-url "http://a.invalid" :ready-p t))
         requests)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
                  ((symbol-function 'hermes-dashboard-transport-acquire)
                   (lambda (&rest _)
                     (setf (hermes-dashboard-transport-client-base-url client)
                           (string-remove-suffix "/" (hermes-instance-url hermes-instance)))
                     client))
                  ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
                  ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                   (lambda (&rest _) auth))
                  ((symbol-function 'hermes-dashboard-transport--default-http-request-async)
                   (lambda (url &rest request)
                     (setq request
                           (append (list :url url :body
                                         (when (plist-get request :data)
                                           (json-parse-string (plist-get request :data) :object-type 'alist)))
                                   request))
                     (push request requests)
                     (hermes--promise-resolved '(:body ((ok . t))))))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest args) (apply reader confirm "yes" args)))
                  ((symbol-function 'read-string)
                   (lambda (&rest args) (apply reader read-text "New title" args)))
                  ((symbol-function 'read-number) (lambda (&rest _) 7))
                  ((symbol-function 'hermes-kanban--render-board)
                   (lambda (&rest args) (when readback (apply render-board args))))
                  ((symbol-function 'hermes-kanban--render-boards)
                   (lambda (&rest args) (when readback (apply render-boards args)))))
          (with-current-buffer origin
            (if (eq kind 'archive)
                (progn
                  (hermes-kanban-boards-mode)
                  (setq tabulated-list-entries
                        (hermes-kanban--board-rows '(((slug . "work") (name . "Work"))))))
              (hermes-kanban-mode)
              (setq hermes-kanban--slug "work"
                    tabulated-list-entries '(("task/a" ["todo" "0" "-" "Old title"]))))
            (tabulated-list-print)
            (goto-char (point-min))
            (condition-case nil
                (call-interactively (if (eq kind 'archive)
                                        #'hermes-kanban-archive-board #'hermes-kanban-edit))
              (user-error nil)))
          (when waiting (with-current-buffer origin (funcall change origin foreign)))
          (hermes--promise-resolve
           auth (list :base-url (hermes-dashboard-transport-client-base-url client)
                      :session-token "fixture"))
          (when native (should (> native-entered 0)))
          (nreverse requests))
      (when (buffer-live-p origin) (kill-buffer origin))
      (kill-buffer foreign))))

(ert-deftest hermes-kanban-input-original-owner-readback ()
  "An accepted write performs its real original-backend refresh."
  (dolist (kind '(archive edit))
    (let ((requests (hermes-kanban-test--input-wire kind #'ignore nil nil nil t)))
      (should (= 1 (cl-count-if (lambda (r) (not (equal "GET" (plist-get r :method)))) requests)))
      (should (equal (plist-get (cadr requests) :url)
                     (if (eq kind 'archive)
                         "http://a.invalid/api/plugins/kanban/boards"
                       "http://a.invalid/api/plugins/kanban/board?board=work")))
      (when (eq kind 'edit)
        (should (equal (plist-get (car requests) :data)
                       "{\"title\":\"New title\",\"priority\":7}"))))))

(ert-deftest hermes-kanban-receipt-precedes-final-lease-release ()
  "A cold request settles before releasing its final native transport lease."
  (let* ((hermes-instances '(("a" . "http://a.invalid")))
         (hermes-dashboard-transport-idle-close-delay 0)
         (client (make-hermes-dashboard-transport-client
                  :base-url "http://a.invalid" :ready-p t :refcount 1))
         accepted)
    (with-temp-buffer
      (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
                ((symbol-function 'hermes-dashboard-transport-acquire) (lambda (&rest _) client))
                ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                 (lambda (&rest _)
                   (hermes--promise-resolved '(:base-url "http://a.invalid" :session-token "fixture"))))
                ((symbol-function 'hermes-dashboard-transport--default-http-request-async)
                 (lambda (&rest _) (hermes--promise-resolved '(:body ((ok . t)))))))
        (hermes-kanban--then (hermes-kanban--api "PATCH" "/tasks/task" '((body . "body")))
                            (lambda (_) (setq accepted t)))
        (should (= (hermes-dashboard-transport-client-refcount client) 0))
        (should accepted)))))

(ert-deftest hermes-kanban-input-retarget-emits-no-write ()
  "Public archive and edit refuse changed owners before acquisition."
  (dolist (kind '(archive edit))
    (should-not
     (hermes-kanban-test--input-wire
      kind (lambda (_origin _foreign)
             (setq-local hermes-instance '("b" . "http://b.invalid")))))))

(ert-deftest hermes-kanban-input-current-owner-writes-once ()
  "Fresh named and legacy origins retain the original serialized target."
  (dolist (legacy '(nil t))
    (dolist (kind '(archive edit))
      (let ((requests (hermes-kanban-test--input-wire kind #'ignore legacy)))
        (should (= (length requests) 1))
        (should (equal (plist-get (car requests) :url)
                       (if (eq kind 'archive)
                           "http://a.invalid/api/plugins/kanban/boards/work"
                         "http://a.invalid/api/plugins/kanban/tasks/task%2Fa?board=work")))))))

(ert-deftest hermes-kanban-detail-rejects-executable-fence-label ()
  "The actual detail renderer must not enable a global minor mode."
  (require 'autorevert)
  (let ((markdown-fontify-code-blocks-natively t)
        (global-auto-revert-mode nil))
    (unwind-protect
        (with-temp-buffer
          (hermes-kanban-task-mode)
          (hermes-kanban--display-task
           '((task . ((id . "task") (body . "```global-auto-revert\nliteral\n```\n"))))
           "work" t)
          (font-lock-ensure)
          (should-not global-auto-revert-mode))
      (global-auto-revert-mode -1))))

(ert-deftest hermes-kanban-input-lifecycle-and-authentication ()
  "Selection, claims, modes, generations and endpoints fence real dispatch."
  (dolist (kind '(archive edit))
    (dolist (waiting '(nil t))
      (dolist (change '(row board claim mode generation killed replaced legacy))
        (ert-info ((format "%s %s waiting=%s" kind change waiting))
          (should-not
           (hermes-kanban-test--input-wire
            kind
            (lambda (origin _foreign)
              (pcase change
                ('row (goto-char (point-max)))
                ('board (if (eq kind 'edit) (setq hermes-kanban--slug "other")
                          (goto-char (point-max))))
                ('claim (setq hermes-buffer--owner (cons t major-mode)))
                ('mode (fundamental-mode))
                ('generation (hermes-browser--next-request-generation))
                ('killed (kill-buffer origin))
                ('replaced
                 (let ((name (buffer-name origin)))
                   (kill-buffer origin)
                   (with-current-buffer _foreign
                     (rename-buffer name) (insert "unrelated draft"))))
                ('legacy (setq hermes-dashboard-transport-url "http://b.invalid"))))
            (eq change 'legacy) waiting))))))
  (dolist (kind '(archive edit))
    (should (= 1 (length (hermes-kanban-test--input-wire
                         kind (lambda (_origin foreign) (set-buffer foreign))))))))

(ert-deftest hermes-kanban-input-native-recursive-minibuffer ()
  "Actual recursive minibuffers retain named and legacy mutation owners."
  (skip-unless (not noninteractive))
  (dolist (kind '(archive edit))
    (dolist (legacy '(nil t))
      (dolist (retire '(nil instance generation mode))
        (let ((requests
               (hermes-kanban-test--input-wire
                kind (lambda (_origin _foreign)
                       (pcase retire
                         ('instance (setq hermes-instance '("b" . "http://b.invalid")))
                         ('generation (hermes-browser--next-request-generation))
                         ('mode (fundamental-mode))))
                legacy nil t)))
          (should (= (length requests) (if retire 0 1))))))))

(ert-deftest hermes-kanban-detail-preserves-literal-guarded-markdown ()
  "Literal copy and outline survive unsafe, nested and programming fences."
  (require 'autorevert)
  (dolist (native '(nil t))
    (let ((markdown-fontify-code-blocks-natively native)
          (markdown-fontify-code-block-default-mode 'global-auto-revert-mode)
          (global-auto-revert-mode nil)
          (hooks 0)
          (text "# Δοκιμή\n```\nblank label\n```\n```org\n#+begin_src emacs-lisp\n(message \"nested\")\n#+end_src\n```\n```emacs-lisp\n(defun example () t)\n```\n| a | b |\n|---|---|\n"))
      (let ((emacs-lisp-mode-hook (list (lambda () (cl-incf hooks))))
            (org-mode-hook (list (lambda () (cl-incf hooks)))))
        (with-temp-buffer
          (hermes-kanban-task-mode)
          (hermes-kanban--display-task `((task . ((id . "t") (body . ,text)))) "work" t)
          (font-lock-flush)
          (font-lock-ensure)
          (should-not global-auto-revert-mode)
          (should (= hooks 0))
          (should outline-minor-mode)
          (should (string-match-p (regexp-quote text) (buffer-string)))
          (should (equal (filter-buffer-substring (point-min) (point-max))
                         (buffer-substring-no-properties (point-min) (point-max))))
          (should-not (text-property-not-all (point-min) (point-max) 'hermes-chat-table nil))
          (when native
            (goto-char (point-min))
            (search-forward "defun example")
            (should (get-text-property (- (point) 3) 'face))))))))

(ert-deftest hermes-kanban-body-editor-roundtrip ()
  "Open, edit and explicitly save only the literal body, then read it back."
  (should (commandp 'hermes-kanban-edit-body))
  (dolist (body '("first\nδεύτερο\n" ""))
    (let ((hermes-instances '(("a" . "http://a.invalid")))
          (client (make-hermes-dashboard-transport-client
                   :base-url "http://a.invalid" :ready-p t))
          (stored "Old body") requests editor)
      (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
                ((symbol-function 'hermes-dashboard-transport-acquire) (lambda (&rest _) client))
                ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
                ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                 (lambda (&rest _)
                   (hermes--promise-resolved '(:base-url "http://a.invalid" :session-token "fixture"))))
                ((symbol-function 'hermes-dashboard-transport--default-http-request-async)
                 (lambda (url &rest request)
                     (setq request
                           (append (list :url url :body
                                         (when (plist-get request :data)
                                           (json-parse-string (plist-get request :data) :object-type 'alist)))
                                   request))
                   (push request requests)
                   (when (equal (plist-get request :method) "PATCH")
                     (should (equal (mapcar #'car (plist-get request :body)) '(body)))
                     (setq stored (alist-get 'body (plist-get request :body))))
                   (hermes--promise-resolved
                    `(:body ((task . ((id . "task/a") (body . ,stored)))))))))
        (unwind-protect
            (with-temp-buffer
              (hermes-kanban-task-mode)
              (setq hermes-kanban-task--task-id "task/a"
                    hermes-kanban-task--board-slug "work")
              (call-interactively #'hermes-kanban-edit-body)
              (setq editor (get-buffer "*Hermes Task Body*"))
              (set-buffer editor)
              (should (derived-mode-p 'hermes-kanban-body-mode))
              (should (equal (buffer-string) "Old body"))
              (erase-buffer)
              (insert body)
              (if noninteractive
                  (call-interactively (key-binding (kbd "C-c C-c")))
                (execute-kbd-macro (kbd "C-c C-c")))
              (should (equal stored body))
              (should (equal (buffer-string) body))
              (should-not (buffer-modified-p))
              (should (equal (mapcar (lambda (r) (plist-get r :method)) (reverse requests))
                             '("GET" "PATCH" "GET")))
              (dolist (request requests)
                (should (equal (plist-get request :url)
                               "http://a.invalid/api/plugins/kanban/tasks/task%2Fa?board=work"))))
          (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest hermes-kanban-body-editor-retains-draft-on-retirement-and-failure ()
  "Save refuses stale owners, survives failed receipts, and never retries."
  (dolist (stage '(source-mode source-instance source-board source-task source-claim
                  editor-mode editor-instance editor-board editor-task editor-claim
                  auth-source auth-kill receipt-retired patch-error readback-error
                  changed-draft))
    (ert-info ((format "%s" stage))
      (let* ((hermes-instances '(("a" . "http://a.invalid")))
             (source (generate-new-buffer " *body source*"))
             (client (make-hermes-dashboard-transport-client
                      :base-url "http://a.invalid" :ready-p t))
             (auth (hermes--promise-make))
             (receipt (hermes--promise-make))
             editor saving requests messages)
        (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
                  ((symbol-function 'hermes-dashboard-transport-acquire) (lambda (&rest _) client))
                  ((symbol-function 'hermes-dashboard-transport-release) #'ignore)
                  ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                   (lambda (&rest _)
                     (if (and saving (memq stage '(auth-source auth-kill))) auth
                       (hermes--promise-resolved '(:base-url "http://a.invalid" :session-token "fixture")))))
                  ((symbol-function 'hermes-dashboard-transport--default-http-request-async)
                   (lambda (url &rest request)
                     (setq request
                           (append (list :url url :body
                                         (when (plist-get request :data)
                                           (json-parse-string (plist-get request :data) :object-type 'alist)))
                                   request))
                     (push request requests)
                     (cond
                      ((equal (plist-get request :method) "PATCH") receipt)
                      ((and saving (eq stage 'readback-error))
                       (hermes--promise-rejected "HTTP 404 task not found"))
                      (t (hermes--promise-resolved
                          `(:body ((task . ((id . "task") (body . ,(if saving "Draft\nλ" "Old")))))))))))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (unwind-protect
              (progn
                (with-current-buffer source
                  (hermes-kanban-task-mode)
                  (hermes-buffer--claim 'hermes-kanban-task-mode)
                  (setq hermes-kanban-task--task-id "task" hermes-kanban-task--board-slug "work")
                  (hermes-kanban-edit-body))
                (setq editor (get-buffer "*Hermes Task Body*"))
                (should editor)
                (with-current-buffer editor (erase-buffer) (insert "Draft\nλ"))
                (with-current-buffer source
                  (pcase stage
                    ('source-mode (fundamental-mode))
                    ('source-instance (setq hermes-instance '("b" . "http://b.invalid")))
                    ('source-board (setq hermes-kanban-task--board-slug "other"))
                    ('source-task (setq hermes-kanban-task--task-id "other"))
                    ('source-claim (hermes-buffer--retire))))
                (with-current-buffer editor
                  (pcase stage
                    ('editor-mode (fundamental-mode))
                    ('editor-instance (setq hermes-instance '("b" . "http://b.invalid")))
                    ('editor-board (setq hermes-kanban-body--board "other"))
                    ('editor-task (setq hermes-kanban-body--task "other"))
                    ('editor-claim (hermes-buffer--retire)))
                  (setq saving t)
                  (condition-case nil (hermes-kanban-body-save) (user-error nil))
                  (when (eq stage 'changed-draft) (insert "newer")))
                (when (memq stage '(auth-source receipt-retired))
                  (with-current-buffer source (hermes-browser--next-request-generation)))
                (when (eq stage 'auth-kill) (kill-buffer editor))
                (hermes--promise-resolve auth '(:base-url "http://a.invalid" :session-token "fixture"))
                (if (eq stage 'patch-error) (hermes--promise-reject receipt "HTTP 404 task not found")
                  (hermes--promise-resolve receipt '(:body ((task . ((id . "task")))))))
                (should (= (cl-count "PATCH" requests :test #'equal
                                     :key (lambda (r) (plist-get r :method)))
                           (if (memq stage '(receipt-retired patch-error readback-error changed-draft)) 1 0)))
                (when (buffer-live-p editor)
                  (with-current-buffer editor
                    (should (string-prefix-p "Draft\nλ" (buffer-string)))
                    (should (buffer-modified-p))
                    (unless (memq stage '(editor-mode editor-instance editor-board editor-task editor-claim))
                      (should-not hermes-kanban-body--save))))
                (when (eq stage 'receipt-retired)
                  (should (seq-some (lambda (text) (string-match-p "check board before retrying" text)) messages))))
            (when (buffer-live-p source) (kill-buffer source))
            (when (buffer-live-p editor) (kill-buffer editor))))))))

(ert-deftest hermes-kanban-api-uses-selected-dashboard-client ()
  "Kanban REST requests use the client for the selected instance."
  (let (seen-client)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success _on-error)
                 (hermes--promise-then
                  (funcall make-promise 'remote-client) on-success)))
              ((symbol-function
                'hermes-dashboard-transport-api-request-async)
               (lambda (_method _path &rest args)
                 (setq seen-client (plist-get args :client))
                 (hermes--promise-resolved '((boards . nil))))))
      (hermes-kanban--api "GET" "/boards")
      (should (eq seen-client 'remote-client)))))

(ert-deftest hermes-kanban-superseded-mutation-requires-board-check ()
  "Stale write receipts warn without refreshing or repeating the mutation."
  (dolist (method '("POST" "PATCH" "DELETE" "GET"))
    (let ((pending (hermes--promise-make))
          (hermes-instances '(("test" . "http://example.test")))
          (instance '("test" . "http://example.test"))
          (calls 0) succeeded messages)
      (with-temp-buffer
        (setq-local hermes-instance instance)
        (cl-letf (((symbol-function 'hermes-browser--run-on-client)
                   (lambda (make-promise &optional on-success _on-error)
                     (hermes--promise-then
                      (funcall make-promise 'client) on-success)))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (&rest _)
                     (cl-incf calls)
                     pending))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (hermes-kanban--then
           (hermes-kanban--api method "/tasks/task/comments")
           (lambda (_) (setq succeeded t)))
          (hermes-browser--next-request-generation)
          (hermes--promise-resolve pending '((ok . t)))
          (should (= calls 1))
          (should-not succeeded)
          (if (equal method "GET")
              (should-not messages)
            (should (= (length messages) 1))
            (should (string-match-p
                     "superseded; check board before retrying"
                     (car messages)))))))))

(ert-deftest hermes-kanban-comment-settles-current-or-superseded-owner ()
  "A comment receipt refreshes only its current owner, otherwise warns."
  (dolist (superseded '(nil t))
    (let ((pending (hermes--promise-make))
          (hermes-instances '(("test" . "http://example.test")))
          requests messages refreshed)
      (with-temp-buffer
        (hermes-kanban-task-mode)
        (setq hermes-instance '("test" . "http://example.test")
              hermes-kanban-task--task-id "task"
              hermes-kanban-task--board-slug "work")
        (cl-letf (((symbol-function 'hermes-browser--run-on-client)
                   (lambda (make-promise &optional on-success _on-error)
                     (hermes--promise-then
                      (funcall make-promise 'client) on-success)))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (method path &rest args)
                     (push (list method path (plist-get args :body)) requests)
                     pending))
                  ((symbol-function 'read-string-from-buffer)
                   (lambda (&rest _) "Comment text"))
                  ((symbol-function 'hermes-kanban--context-refresher)
                   (lambda () (lambda () (setq refreshed t))))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (call-interactively #'hermes-kanban-comment)
          (when superseded (hermes-browser--next-request-generation))
          (hermes--promise-resolve pending '((ok . t)))
          (should (equal requests
                         '(("POST" "/api/plugins/kanban/tasks/task/comments"
                            ((body . "Comment text"))))))
          (should (eq refreshed (not superseded)))
          (should (equal messages
                         (list (if superseded
                                   (concat "Hermes: Kanban update superseded; "
                                           "check board before retrying")
                                 "Comment added to task task")))))))))

(ert-deftest hermes-kanban-status-display-uses-shared-icons ()
  "Status display helpers share icons, labels, and raw status properties."
  (should (equal hermes-kanban--current-board-marker "📍"))
  (should (equal hermes-kanban--board-count-statuses
                 '("triage" "todo" "scheduled" "ready" "running" "blocked"
                   "review" "done" "archived")))
  (should (equal hermes-kanban--statuses
                 '("triage" "todo" "scheduled" "ready" "running" "blocked"
                   "review" "done" "archived")))
  (dolist (spec '(("triage" "💡")
                  ("todo" "📝")
                  ("scheduled" "⏰")
                  ("ready" "✅")
                  ("running" "⚙️")
                  ("blocked" "⛔")
                  ("review" "👀")
                  ("done" "🏁")
                  ("archived" "🗄️")))
    (pcase-let ((`(,status ,icon) spec))
      (should (equal (hermes-kanban--status-icon status) icon))
      (let ((formatted (hermes-kanban--format-status status)))
        (should (equal (substring-no-properties formatted)
                       (format "%s %s" icon status)))
        (should (equal (get-text-property 0 'hermes-kanban-status formatted)
                       status)))
      (let ((indicator (hermes-kanban--format-status-indicator status)))
        (should (equal (substring-no-properties indicator) icon))
        (should (equal (get-text-property 0 'hermes-kanban-status indicator)
                       status)))))
  (let ((running (hermes-kanban--format-status "running")))
    (should (equal (hermes-kanban--entry-status
                    (vector running "2" "elisp-dev" "Do thing"))
                   "running"))
    (should (equal (hermes-kanban--entry-status
                    (vector "⚙️ running" "2" "elisp-dev" "Do thing"))
                   "running")))
  (let ((running (hermes-kanban-format-status "running")))
    (should (equal (substring-no-properties running) "⚙️ running"))
    (should (eq (get-text-property 0 'face running)
                'hermes-kanban-running-face)))
  (should (equal (hermes-kanban--format-status-count
                  '((ready . 2)) "ready")
                 "2"))
  (let* ((raw (copy-sequence "done"))
         (formatted (hermes-kanban--format-status raw)))
    (should (equal (substring-no-properties formatted) "🏁 done"))
    (should (equal (get-text-property 0 'hermes-kanban-status formatted)
                   "done"))
    (should-not (text-properties-at 0 raw))))

(ert-deftest hermes-kanban-workflow-statuses-have-distinct-faces ()
  "Every Kanban workflow column has its own customizable face."
  (dolist (spec '(("triage" hermes-kanban-triage-face)
                  ("todo" hermes-kanban-todo-face)
                  ("scheduled" hermes-kanban-scheduled-face)
                  ("ready" hermes-kanban-ready-face)
                  ("running" hermes-kanban-running-face)
                  ("blocked" hermes-kanban-blocked-face)
                  ("review" hermes-kanban-review-face)
                  ("done" hermes-kanban-done-face)
                  ("archived" hermes-kanban-archived-face)))
    (pcase-let ((`(,status ,face) spec))
      (should (facep face))
      (should (eq (plist-get (hermes-kanban--status-info status) :face) face))
      (should (eq (get-text-property
                   0 'face (hermes-kanban--format-status-count nil status))
                  face))
      (should (eq (get-text-property
                   0 'face (hermes-kanban--format-status-indicator status))
                  face)))))

(ert-deftest hermes-kanban-rows-face-every-column ()
  "Kanban rows give every board, task, and diagnostic column a face."
  (let* ((board (car (hermes-kanban--board-rows
                      '(((slug . "main") (name . "Main") (is_current . t)
                         (total . 7)
                         (counts . ((triage . 1) (todo . 1) (ready . 1)
                                    (running . 1) (blocked . 1) (done . 1)
                                    (archived . 1))))))))
         (row (car (hermes-kanban--task-rows
                    '(((tasks . (((id . "t1") (status . "triage")
                                  (priority . 2) (assignee . "planner")
                                  (title . "Rough idea")))))))))
         (diagnostic (hermes-kanban--diagnostic-row
                      '((task_id . "t1") (task_title . "Rough idea")
                        (task_assignee . "planner")
                        (diagnostics . (((severity . "critical")
                                         (title . "Worker failed")))))))
         (board-entry (cadr board))
         (entry (cadr row))
         (diagnostic-entry (cadr diagnostic)))
    (should (eq (get-text-property 0 'face (aref board-entry 0))
                'hermes-browser-default))
    (should (eq (get-text-property 0 'face (aref board-entry 1))
                'hermes-browser-name))
    (should (eq (get-text-property 0 'face (aref board-entry 2))
                'hermes-browser-total))
    (cl-mapc (lambda (cell face)
               (should (eq (get-text-property 0 'face cell) face)))
             (append (seq-subseq board-entry 3) nil)
             '(hermes-kanban-triage-face hermes-kanban-todo-face
               hermes-kanban-scheduled-face hermes-kanban-ready-face
               hermes-kanban-running-face hermes-kanban-blocked-face
               hermes-kanban-review-face hermes-kanban-done-face
               hermes-kanban-archived-face))
    (should (eq (get-text-property 0 'face (aref entry 0))
                'hermes-kanban-triage-face))
    (should (eq (get-text-property 0 'face (aref entry 1))
                'hermes-browser-priority))
    (should (eq (get-text-property 0 'face (aref entry 2))
                'hermes-browser-assignee))
    (should (eq (get-text-property 0 'face (aref entry 3))
                'hermes-browser-title))
    (should (equal (get-text-property 0 'face (aref diagnostic-entry 0))
                   '(hermes-browser-error hermes-browser-severity)))
    (should (eq (get-text-property 0 'face (aref diagnostic-entry 1))
                'hermes-browser-title))
    (should (eq (get-text-property 0 'face (aref diagnostic-entry 2))
                'hermes-browser-assignee))
    (should (eq (get-text-property 0 'face (aref diagnostic-entry 3))
                'hermes-browser-diagnostic))))

(ert-deftest hermes-kanban-tabulated-list-formats-scale-with-width ()
  "Kanban tabulated-list formats fit and flex by display width."
  (dolist (width '(30 40 50 80 120))
    (let ((boards (hermes-kanban--boards-tabulated-list-format width))
          (tasks (hermes-kanban--tasks-tabulated-list-format width)))
      (should (= (hermes-test--tabulated-list-format-total-width boards)
                 width))
      (should (<= (hermes-test--tabulated-list-format-total-width tasks)
                  width))
      (should (equal (car (aref boards 0)) ""))
      (should (equal (car (aref boards 1)) "📋"))
      (should (equal (car (aref boards 3)) "💡"))
      (should (>= (cadr (aref boards 0)) 1))
      (should (>= (cadr (aref boards 1)) 1))
      (should (>= (cadr (aref tasks 0)) 1))
      (should (>= (cadr (aref tasks 3)) 1))
      (should (<= (cadr (aref tasks 3))
                  hermes-kanban--task-title-column-max-width))
      (when (>= width 60)
        (should (>= (cadr (aref boards 1)) 12))
        (should (>= (cadr (aref boards 3)) 4))
        (should (>= (cadr (aref tasks 0)) 6))
        (should (>= (cadr (aref tasks 2)) 10))
        (should (>= (cadr (aref tasks 3)) 20)))))
  (let ((narrow-boards (hermes-kanban--boards-tabulated-list-format 50))
        (wide-boards (hermes-kanban--boards-tabulated-list-format 120))
        (narrow-tasks (hermes-kanban--tasks-tabulated-list-format 50))
        (wide-tasks (hermes-kanban--tasks-tabulated-list-format 120)))
    (should (< (cadr (aref narrow-boards 1))
               (cadr (aref wide-boards 1))))
    (should (< (cadr (aref narrow-boards 3))
               (cadr (aref wide-boards 3))))
    (should (< (cadr (aref narrow-tasks 2))
               (cadr (aref wide-tasks 2))))
    (should (< (cadr (aref narrow-tasks 3))
               (cadr (aref wide-tasks 3))))
    (should (= (cadr (aref (hermes-kanban--tasks-tabulated-list-format 200) 3))
               hermes-kanban--task-title-column-max-width))))

(ert-deftest hermes-kanban-window-size-change-recomputes-format ()
  "Kanban tabulated-list modes recompute widths when their window resizes."
  (dolist (mode '(hermes-kanban-boards-mode hermes-kanban-mode))
    (let ((buffer (hermes-buffer--get " *Hermes resize test*" mode)))
      (unwind-protect
          (with-current-buffer buffer
            (let (printed)
              (cl-letf (((symbol-function 'window-body-width)
                         (lambda (_window &optional _pixelwise) 120))
                        ((symbol-function 'tabulated-list-print)
                         (lambda (&rest _) (setq printed t))))
                (setq tabulated-list-format
                      (if (derived-mode-p 'hermes-kanban-boards-mode)
                          (hermes-kanban--boards-tabulated-list-format 50)
                        (hermes-kanban--tasks-tabulated-list-format 50)))
                (hermes-kanban--window-size-change 'fake-window)
                (should printed)
                (let ((total (hermes-test--tabulated-list-format-total-width
                              tabulated-list-format)))
                  (if (derived-mode-p 'hermes-kanban-boards-mode)
                      (should (= total 120))
                    (should (<= total 120))
                    (should (<= (cadr (aref tabulated-list-format 3))
                                hermes-kanban--task-title-column-max-width)))))))
        (kill-buffer buffer)))))

(ert-deftest hermes-kanban-board-rows-from-boards ()
  "Board rows map name/total/per-status counts and mark the current board."
  (cl-labels ((column-for (status)
                (+ 3 (cl-position status hermes-kanban--board-count-statuses
                                  :test #'equal))))
    (let* ((rows (hermes-kanban--board-rows
                  '(((slug . "emacs-lisp") (name . "Emacs Lisp")
                     (is_current . t) (total . 6)
                     (counts . ((triage . 2) (todo . 1) (running . 2)
                                (archived . 1)))))))
           (entry (cadr (car rows))))
      (should (equal (caar rows) (cons "emacs-lisp" "Emacs Lisp")))
      (should (equal (aref entry 0) "📍"))
      (should (equal (aref entry 1) "Emacs Lisp"))
      (should (equal (aref entry 2) "6"))
      (should (equal (aref entry (column-for "triage")) "2"))
      (should (equal (aref entry (column-for "todo")) "1"))
      (should (equal (aref entry (column-for "running")) "2"))
      (should (equal (aref entry (column-for "archived")) "1")))))

(ert-deftest hermes-kanban-task-rows-from-columns ()
  "Task rows flatten dashboard status columns into status/pri/assignee/title."
  (let* ((title "Do thing with a long title that tabulated-list truncates by column")
         (rows (hermes-kanban--task-rows
                `(((name . "todo")
                   (tasks . (((id . "t1") (status . "todo") (priority . 2)
                              (assignee . "elisp-dev") (title . ,title)
                              (created_at . 1000)))))
                  ((name . "running") (tasks . nil))))))
    (should (equal (caar rows) "t1"))
    (should (= (length rows) 1))
    (let ((status (aref (cadr (car rows)) 0)))
      (should (equal (substring-no-properties status) "📝"))
      (should (equal (get-text-property 0 'hermes-kanban-status status)
                     "todo")))
    (should (equal (aref (cadr (car rows)) 1) "2"))
    (should (equal (aref (cadr (car rows)) 2) "elisp-dev"))
    (should (equal (aref (cadr (car rows)) 3) title))))

(ert-deftest hermes-kanban-task-rows-sort-newest-first ()
  "Task rows are sorted by `created_at' descending across all status columns."
  (let* ((columns
          `(((name . "done")
             (tasks . (((id . "old") (status . "done") (priority . 3)
                        (title . "Oldest") (created_at . 1000))
                       ((id . "mid") (status . "done") (priority . 5)
                        (title . "Middle") (created_at . 2000)))))
            ((name . "todo")
             (tasks . (((id . "new") (status . "todo") (priority . 1)
                        (title . "Newest") (created_at . 3000))
                       ((id . "newer2") (status . "todo") (priority . 2)
                        (title . "Second") (created_at . 2500)))))))
         (ids (mapcar #'car (hermes-kanban--task-rows columns))))
    ;; Newest created_at first, regardless of the backend's status column order
    ;; (backend returns "done" before "todo" here).
    (should (equal ids '("new" "newer2" "mid" "old")))))

(ert-deftest hermes-kanban-task-rows-missing-created-at-sorts-oldest ()
  "Tasks with missing or non-numeric `created_at' sort after dated ones."
  (let* ((columns
          `(((name . "todo")
             (tasks . (((id . "dated") (status . "todo") (priority . 1)
                        (title . "Dated") (created_at . 1000))
                       ((id . "missing") (status . "todo") (priority . 2)
                         (title . "No timestamp"))
                       ((id . "string-ts") (status . "todo") (priority . 3)
                        (title . "Bad timestamp") (created_at . "oops")))))))
         (rows (hermes-kanban--task-rows columns))
         (ids (mapcar #'car rows)))
    ;; Dated first, then the two timestamp-less tasks in input order (stable).
    (should (equal ids '("dated" "missing" "string-ts")))))

(ert-deftest hermes-kanban-render-boards-lists-boards ()
  "The boards overview fetches /boards and renders one row per board."
  (cl-letf (((symbol-function 'hermes-kanban--api)
             (lambda (method path &optional _body _query)
               (should (equal method "GET"))
               (should (equal path "/boards"))
               (hermes--promise-resolved '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
						       (is_current . t) (total . 1)
						       (counts . ((ready . 1)))))))))))
    (unwind-protect
        (progn
          (hermes-list-kanban)
          (with-current-buffer "*Hermes Kanban Boards*"
            (should (derived-mode-p 'hermes-kanban-boards-mode))
            (should (equal (car (aref tabulated-list-format 1)) "📋"))
            (should (equal (caar tabulated-list-entries)
                           (cons "emacs-lisp" "Emacs Lisp")))))
      (when (get-buffer "*Hermes Kanban Boards*")
        (kill-buffer "*Hermes Kanban Boards*")))))

(ert-deftest hermes-kanban-render-boards-pins-resolved-instance ()
  "The boards overview owns the instance selected by its entry command."
  (let ((instance '("remote" . "https://hermes.example.test")))
    (cl-letf (((symbol-function 'hermes-instance-resolve)
               (lambda () instance))
              ((symbol-function 'hermes-kanban--api)
               (lambda (&rest _)
                 (hermes--promise-resolved '((boards . nil))))))
      (unwind-protect
          (progn
            (hermes-list-kanban)
            (with-current-buffer "*Hermes Kanban Boards*"
              (should (equal hermes-instance instance))))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-cold-boards-request-owns-destination-buffer ()
  "A pending cold request survives return from an unowned command buffer."
  (let ((pending (hermes--promise-make))
        (instance '("remote" . "https://hermes.example.test")))
    (cl-letf (((symbol-function 'hermes-instance-resolve) (lambda () instance))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success on-error)
                 (hermes--promise-then (funcall make-promise 'client)
                                       on-success on-error)))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) pending)))
      (unwind-protect
          (progn
            (with-temp-buffer (hermes-list-kanban))
            (hermes--promise-resolve
             pending '((boards . (((slug . "owned") (name . "Owned"))))))
            (with-current-buffer "*Hermes Kanban Boards*"
              (should (equal hermes-instance instance))
              (should (equal (caar tabulated-list-entries)
                             '("owned" . "Owned")))))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-boards-revert-refreshes-without-display ()
  "Reverting the boards overview refreshes in place; the command displays."
  (let (displayed)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
                                (is_current . t) (total . 1)
                                (counts . ((ready . 1))))))))))
              ((symbol-function 'pop-to-buffer)
               (lambda (&rest _) (setq displayed t))))
      (unwind-protect
          (progn
            (hermes-kanban--boards-revert)
            (should-not displayed)
            (with-current-buffer "*Hermes Kanban Boards*"
              (should (equal (caar tabulated-list-entries)
                             (cons "emacs-lisp" "Emacs Lisp"))))
            (hermes-list-kanban)
            (should displayed))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-boards-discard-late-response ()
  "A late boards overview response cannot replace the latest refresh."
  (let ((old (hermes--promise-make)) (new (hermes--promise-make)) (calls 0))
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (&rest _)
                 (cl-incf calls)
                 (if (= calls 1) old new)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (unwind-protect
          (progn
            (hermes-list-kanban)
            (hermes-list-kanban)
            (hermes--promise-resolve
             new '((boards . (((slug . "new") (name . "New"))))))
            (hermes--promise-resolve
             old '((boards . (((slug . "old") (name . "Old"))))))
            (with-current-buffer "*Hermes Kanban Boards*"
              (should (equal (caar tabulated-list-entries)
                             (cons "new" "New")))))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-boards-ignore-late-rejection ()
  "A stale boards rejection cannot report an error after newer success."
  (let ((old (hermes--promise-make)) (new (hermes--promise-make))
        (calls 0) messages)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (&rest _)
                 (cl-incf calls)
                 (if (= calls 1) old new)))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-list-kanban)
            (hermes-list-kanban)
            (hermes--promise-resolve new '((boards . [])))
            (hermes--promise-reject old "stale boards error")
            (should-not
             (seq-some (lambda (text) (string-match-p "stale boards" text))
                       messages)))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-boards-hide-superseded-rejection ()
  "Retargeting the boards request does not expose its internal sentinel."
  (let ((pending (hermes--promise-make)) messages)
    (cl-letf (((symbol-function 'hermes-kanban--api) (lambda (&rest _) pending))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-list-kanban)
            (hermes--promise-reject pending hermes-kanban--superseded)
            (should-not messages))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-board-actions-dispatch-rest-calls ()
  "Board overview actions use REST endpoints, safe archive, and refresh."
  (let (calls prompts)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved (pcase path
					     ("/boards"
					      '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
							    (is_current . t) (total . 1)
							    (counts . ((ready . 1))))))))
					     ("/boards/emacs-lisp/switch" '((current . "emacs-lisp")))
					     ("/boards/emacs-lisp" '((board . ((slug . "emacs-lisp")
									       (name . "Renamed")))))
					     (_ (error "unexpected path: %s" path))))))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (push prompt prompts)
                 t))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (hermes-kanban-switch-board)
              (goto-char (point-min))
              (hermes-kanban-rename-board " Renamed ")
              (goto-char (point-min))
              (hermes-kanban-archive-board))
            (should (member '("POST" "/boards/emacs-lisp/switch" nil nil)
                            calls))
            (should (member '("PATCH" "/boards/emacs-lisp"
                              ((name . "Renamed")) nil)
                            calls))
            (should (member '("DELETE" "/boards/emacs-lisp" nil nil)
                            calls))
            (should (= (cl-count-if (lambda (call)
                                      (equal (cadr call) "/boards"))
                                    calls)
                       4))
            (should (= (length prompts) 2))
            (should (cl-some (lambda (prompt)
                               (string-match-p "current board" prompt))
                             prompts))
            (should (cl-some (lambda (prompt)
                               (string-match-p "hard delete" prompt))
                             prompts)))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-board-mutation-refresh-keeps-origin-instance ()
  "A delayed board mutation refreshes through its originating instance."
  (let* ((local '("local" . "http://127.0.0.1:9119"))
         (remote '("remote" . "https://hermes.example.test"))
         (hermes-instances (list local remote))
         (mutation (hermes--promise-make))
         refreshed-instance)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (&rest _) mutation))
              ((symbol-function 'hermes-kanban--render-boards)
               (lambda (&optional _)
                 (setq refreshed-instance (hermes-instance-resolve))))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "Unexpected instance prompt")))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-kanban-boards-mode)
        (setq hermes-instance remote
              tabulated-list-entries
              (hermes-kanban--board-rows
               '(((slug . "work") (name . "Work") (total . 0)
                  (counts . nil)))))
        (tabulated-list-print)
        (goto-char (point-min))
        (hermes-kanban-switch-board)
        (with-temp-buffer
          (hermes--promise-resolve mutation '((current . "work"))))
        (should (equal refreshed-instance remote))))))

(ert-deftest hermes-kanban-rename-board-rejects-blank-name ()
  "Whitespace-only board renames signal before PATCH or refresh."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 (hermes--promise-resolved '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
							 (is_current . t) (total . 1))))))))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (should-error (hermes-kanban-rename-board "   ")
                            :type 'user-error))
            (should-not calls))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-archive-current-board-cancel-stops-before-delete ()
  "Declining the current-board archive prompt skips DELETE and refresh."
  (let (calls prompts)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 (hermes--promise-resolved '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
							 (is_current . t) (total . 1))))))))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (push prompt prompts)
                 nil))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (should-error (hermes-kanban-archive-board)
                            :type 'user-error))
            (should-not calls)
            (should (= (length prompts) 1))
            (should (string-match-p "current board" (car prompts))))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-archive-default-board-stops-before-prompt ()
  "The protected default board is rejected before prompts or DELETE."
  (let (calls prompted)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 (hermes--promise-resolved '((boards . (((slug . "default") (name . "Default")
							 (is_current . t) (total . 1))))))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (setq prompted t)
                 t))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (let ((err (should-error (hermes-kanban-archive-board)
                                       :type 'user-error)))
                (should (string-match-p "protected.*cannot be archived"
                                        (error-message-string err)))))
            (should-not calls)
            (should-not prompted))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-archive-board-cancel-skips-delete ()
  "Declining the normal archive prompt skips DELETE and refresh."
  (let (calls prompts)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 (hermes--promise-resolved '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
							 (total . 1))))))))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (push prompt prompts)
                 nil))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (should-not (hermes-kanban-archive-board)))
            (should-not calls)
            (should (= (length prompts) 1))
            (should (string-match-p "hard delete" (car prompts))))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-open-board-renders-tasks ()
  "Opening a board fetches tasks and shows automatic triage orchestration."
  (cl-letf (((symbol-function 'window-body-width)
             (lambda (&optional _window _pixelwise) 80))
            ((symbol-function 'hermes-kanban--api)
             (lambda (method path &optional _body query)
               (should (equal method "GET"))
               (hermes--promise-resolved
                (if (equal path "/orchestration")
                    '((auto_decompose . t))
                  (should (equal path "/board"))
                  (should (equal (cdr (assq 'board query)) "emacs-lisp"))
                  '((columns . (((name . "todo")
                                 (tasks . (((id . "t1") (status . "todo")
                                            (title . "Do thing")))))))
                    (assignees . ("elisp-dev"))))))))
    (unwind-protect
        (progn
          (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
          (with-current-buffer "*Hermes Kanban*"
            (should (derived-mode-p 'hermes-kanban-mode))
            (should (equal hermes-kanban--slug "emacs-lisp"))
            (should (equal hermes-kanban--assignees '("elisp-dev")))
            (should (= (hermes-test--tabulated-list-format-total-width
                        tabulated-list-format)
                       80))
            (should (>= (cadr (aref tabulated-list-format 0)) 6))
            (should (>= (cadr (aref tabulated-list-format 3)) 20))
            (should (equal (caar tabulated-list-entries) "t1"))
            (should (eq hermes-kanban--orchestration-mode 'auto))
            (should (string-match-p
                     "Triage: auto"
                     (hermes-kanban--triage-mode-indicator)))))
      (when (get-buffer "*Hermes Kanban*") (kill-buffer "*Hermes Kanban*")))))

(ert-deftest hermes-kanban-board-discards-late-response ()
  "A late response for board A cannot replace the newer board B render."
  (let ((a (hermes--promise-make)) (b (hermes--promise-make)))
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (_method _path &optional _body query)
                 (if (equal (cdr (assq 'board query)) "a") a b))))
      (unwind-protect
          (progn
            (hermes-kanban--render-board "a" "Board A")
            (hermes-kanban--render-board "b" "Board B")
            (hermes--promise-resolve
             b '((columns . (((name . "todo")
                              (tasks . (((id . "b-task") (status . "todo")
                                         (title . "B")))))))
                 (assignees)))
            (hermes--promise-resolve
             a '((columns . (((name . "todo")
                              (tasks . (((id . "a-task") (status . "todo")
                                         (title . "A")))))))
                 (assignees)))
            (with-current-buffer "*Hermes Kanban*"
              (should (equal hermes-kanban--slug "b"))
              (should (equal (caar tabulated-list-entries) "b-task"))))
        (when (get-buffer "*Hermes Kanban*")
          (kill-buffer "*Hermes Kanban*"))))))

(ert-deftest hermes-kanban-board-discards-late-orchestration-response ()
  "Older orchestration state cannot replace the latest board refresh state."
  (let ((settings-a (hermes--promise-make))
        (settings-b (hermes--promise-make))
        (settings-call 0))
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (_method path &optional _body query)
                 (if (equal path "/orchestration")
                     (prog1 (if (zerop settings-call) settings-a settings-b)
                       (setq settings-call (1+ settings-call)))
                   (hermes--promise-resolved
                    `((columns . (((name . "todo") (tasks . []))))
                      (assignees)
                      (board . ,(cdr (assq 'board query)))))))))
      (unwind-protect
          (progn
            (hermes-kanban--render-board "a" "Board A")
            (hermes-kanban--render-board "b" "Board B")
            (hermes--promise-resolve
             settings-b '((auto_decompose . :json-false)))
            (hermes--promise-resolve settings-a '((auto_decompose . t)))
            (with-current-buffer "*Hermes Kanban*"
              (should (equal hermes-kanban--slug "b"))
              (should (eq hermes-kanban--orchestration-mode 'manual))))
        (when (get-buffer "*Hermes Kanban*")
          (kill-buffer "*Hermes Kanban*"))))))

(ert-deftest hermes-kanban-board-switch-retargets-live-tail ()
  "Switching boards disconnects the old live socket and reconnects for the new slug."
  (let (disconnected connected)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '((columns . (((name . "todo") (tasks . []))))
                    (assignees) (latest_event_id . 9)))))
              ((symbol-function 'hermes-kanban--events-disconnect)
               (lambda (tail)
                 (setq disconnected (hermes-kanban--events-tail-slug tail))))
              ((symbol-function 'hermes-kanban--events-connect)
               (lambda (tail)
                 (setq connected (hermes-kanban--events-tail-slug tail)))))
      (unwind-protect
          (progn
            (with-current-buffer
              (hermes-buffer--get "*Hermes Kanban*" #'hermes-kanban-mode)
              (setq hermes-kanban--slug "a"
                    hermes-kanban--events-tail
                    (hermes-kanban--events-tail-create
                     :buffer (current-buffer) :slug "a" :socket 'old)))
            (hermes-kanban--render-board "b" "Board B")
            (with-current-buffer "*Hermes Kanban*"
              (should (equal disconnected "a"))
              (should (equal connected "b"))
              (should (equal (hermes-kanban--events-tail-slug
                              hermes-kanban--events-tail)
                             "b"))
              (should (= (hermes-kanban--events-tail-cursor
                          hermes-kanban--events-tail)
                         9))))
        (when (get-buffer "*Hermes Kanban*")
          (kill-buffer "*Hermes Kanban*"))))))

(ert-deftest hermes-kanban-instance-switch-retargets-live-tail ()
  "Reusing the board buffer for another instance replaces the live socket."
  (let (disconnected connected)
    (cl-letf (((symbol-function 'hermes-kanban--events-disconnect)
               (lambda (tail)
                 (setq disconnected (hermes-kanban--events-tail-slug tail))))
              ((symbol-function 'hermes-kanban--events-connect)
               (lambda (tail)
                 (setq connected
                       (list (hermes-kanban--events-tail-slug tail)
                             (hermes-kanban--events-tail-instance tail))))))
      (with-temp-buffer
        (hermes-kanban-mode)
        (setq hermes-instance '("a" . "http://a")
              hermes-kanban--slug "work"
              hermes-kanban--events-tail
              (hermes-kanban--events-tail-create
               :buffer (current-buffer) :slug "work" :socket 'old))
        (setq hermes-instance '("b" . "http://b"))
        (hermes-kanban--events-retarget "work" 4)
        (should (equal disconnected "work"))
        (should (equal connected '("work" ("b" . "http://b"))))
        (should (= 4 (hermes-kanban--events-tail-cursor
                      hermes-kanban--events-tail)))))))

(ert-deftest hermes-kanban-same-instance-retarget-keeps-live-tail ()
  "A live refresh for the same board and instance does not reconnect."
  (let (disconnected connected)
    (cl-letf (((symbol-function 'hermes-kanban--events-disconnect)
               (lambda (&rest _) (setq disconnected t)))
              ((symbol-function 'hermes-kanban--events-connect)
               (lambda (&rest _) (setq connected t))))
      (with-temp-buffer
        (hermes-kanban-mode)
        (setq hermes-instance '("a" . "http://a")
              hermes-kanban--events-tail
              (hermes-kanban--events-tail-create
               :buffer (current-buffer) :slug "work"
               :instance '("a" . "http://a") :socket 'old :cursor 2))
        (hermes-kanban--events-retarget "work" 9)
        (should-not disconnected)
        (should-not connected)
        (should (= 2 (hermes-kanban--events-tail-cursor
                      hermes-kanban--events-tail)))))))

(ert-deftest hermes-kanban-show-fetches-task-at-point ()
  "Showing fetches the task on the current row and renders its body."
  (let (show-path)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (_method path &optional _body _query)
                 (hermes--promise-resolved (cond
					    ((equal path "/board")
					     '((columns . (((name . "todo")
							    (tasks . (((id . "t1") (status . "todo")
								       (title . "Do thing")))))))
					       (assignees)))
					    (t (setq show-path path)
					       '((task . ((id . "t1") (title . "Do thing") (status . "todo")
							  (body . "details here"))))))))))
      (unwind-protect
          (progn
            (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
            (with-current-buffer "*Hermes Kanban*"
              (goto-char (point-min))
              (hermes-kanban-show))
            (should (equal show-path "/tasks/t1"))
            (with-current-buffer "*Hermes Kanban Task*"
              (should (derived-mode-p 'hermes-kanban-task-mode))
              (should (derived-mode-p 'special-mode))
              (should outline-minor-mode)
              (should buffer-read-only)
              (should (equal hermes-kanban-task--task-id "t1"))
              (should (string-match-p "## Description" (buffer-string)))
              (should (string-match-p "details here" (buffer-string)))))
        (dolist (b '("*Hermes Kanban*" "*Hermes Kanban Task*"))
          (when (get-buffer b) (kill-buffer b)))))))

(ert-deftest hermes-kanban-open-task-fetches-task-by-id ()
  "Opening a task fetches fresh detail by id and board."
  (let (request)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional _body query)
                 (setq request (list method path query))
                 (hermes--promise-resolved
                  '((task . ((id . "t1") (title . "Do thing")
                             (status . "running") (body . "details"))))))))
      (unwind-protect
          (progn
            (hermes-kanban-open-task "t1" "emacs-lisp")
            (should (equal request
                           '("GET" "/tasks/t1" ((board . "emacs-lisp")))))
            (with-current-buffer "*Hermes Kanban Task*"
              (should (equal hermes-kanban-task--task-id "t1"))
              (should (equal hermes-kanban-task--board-slug "emacs-lisp"))))
        (when (get-buffer "*Hermes Kanban Task*")
          (kill-buffer "*Hermes Kanban Task*"))))))

(ert-deftest hermes-kanban-open-board-task-selects-task-row ()
  "Opening a task's board selects that task after rendering."
  (cl-letf (((symbol-function 'hermes-kanban--api)
             (lambda (_method path &optional _body _query)
               (hermes--promise-resolved
                (if (equal path "/orchestration")
                    '((auto_decompose . :json-false))
                  '((columns
                     . (((name . "todo")
                         (tasks . (((id . "t1") (status . "todo")
                                    (title . "First"))
                                   ((id . "t2") (status . "todo")
                                    (title . "Second")))))))
                    (assignees)))))))
    (unwind-protect
        (progn
          (hermes-kanban-open-board-task "emacs-lisp" "t2")
          (with-current-buffer "*Hermes Kanban*"
            (should (equal hermes-kanban--slug "emacs-lisp"))
            (should (equal (tabulated-list-get-id) "t2"))))
      (when (get-buffer "*Hermes Kanban*")
        (kill-buffer "*Hermes Kanban*")))))

(ert-deftest hermes-kanban-task-detail-discards-late-response ()
  "A late task A refresh cannot replace the newer task B detail."
  (let ((a (hermes--promise-make)) (b (hermes--promise-make)))
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (_method path &rest _)
                 (if (string-suffix-p "/a" path) a b))))
      (unwind-protect
          (progn
            (with-current-buffer
              (hermes-buffer--get "*Hermes Kanban Task*" #'hermes-kanban-task-mode)
              (setq hermes-kanban-task--task-id "a"
                    hermes-kanban-task--board-slug "board")
              (hermes-kanban--task-revert)
              (setq hermes-kanban-task--task-id "b")
              (hermes-kanban--task-revert))
            (hermes--promise-resolve
             b '((task . ((id . "b") (title . "Task B")
                           (status . "todo") (body . "new")))))
            (hermes--promise-resolve
             a '((task . ((id . "a") (title . "Task A")
                           (status . "todo") (body . "old")))))
            (with-current-buffer "*Hermes Kanban Task*"
              (should (equal hermes-kanban-task--task-id "b"))
              (should (string-match-p "Task B" (buffer-string)))
              (should-not (string-match-p "Task A" (buffer-string)))))
        (when (get-buffer "*Hermes Kanban Task*")
          (kill-buffer "*Hermes Kanban Task*"))))))

(ert-deftest hermes-kanban-log-discards-late-response ()
  "A late task A log refresh cannot replace the newer task B log."
  (let ((a (hermes--promise-make)) (b (hermes--promise-make)))
    (cl-letf (((symbol-function 'hermes-kanban--fetch-log)
               (lambda (id _board) (if (equal id "a") a b))))
      (unwind-protect
          (progn
            (with-current-buffer
              (hermes-buffer--get "*Hermes Kanban Log*" #'hermes-kanban-log-mode)
              (setq hermes-kanban-log--task-id "a"
                    hermes-kanban-log--board-slug "board")
              (hermes-kanban--log-revert)
              (setq hermes-kanban-log--task-id "b")
              (hermes-kanban--log-revert))
            (hermes--promise-resolve
             b '((task_id . "b") (exists . t) (content . "new log")))
            (hermes--promise-resolve
             a '((task_id . "a") (exists . t) (content . "old log")))
            (with-current-buffer "*Hermes Kanban Log*"
              (should (equal hermes-kanban-log--task-id "b"))
              (should (string-match-p "new log" (buffer-string)))
              (should-not (string-match-p "old log" (buffer-string)))))
        (when (get-buffer "*Hermes Kanban Log*")
          (kill-buffer "*Hermes Kanban Log*"))))))

(ert-deftest hermes-kanban-task-detail-runs-extension-functions ()
  "Task detail extensions receive the payload and board in the task buffer."
  (let (observed)
    (unwind-protect
        (let ((hermes-kanban-task-detail-functions
               (list (lambda (payload board)
                       (setq observed (list (current-buffer) payload board))
                       (insert "\nExtension content\n")))))
          (hermes-kanban--display-task
           '((task . ((id . "t_1234abcd") (title . "Task")
                      (status . "todo") (body . "Body"))))
           "default")
          (with-current-buffer "*Hermes Kanban Task*"
            (should (equal (car observed) (current-buffer)))
            (should (equal (cadr observed)
                           '((task . ((id . "t_1234abcd") (title . "Task")
                                      (status . "todo") (body . "Body"))))))
            (should (equal (caddr observed) "default"))
            (should (string-match-p "Extension content" (buffer-string)))))
      (when (get-buffer "*Hermes Kanban Task*")
        (kill-buffer "*Hermes Kanban Task*")))))

(ert-deftest hermes-kanban-format-task-detail-renders-markdown-sections ()
  "Task detail formatting includes Markdown task sections and rows."
  (let* ((payload
          '((task . ((id . "t1") (title . "Do thing") (status . "running")
                     (priority . 5) (assignee . "elisp-dev")
                     (created_at . 1700000000)
                     (body . "details here")
                     (diagnostics . (((kind . "stale_running")
                                      (severity . "error")
                                      (title . "Stale worker")
                                      (detail . "No heartbeat")
                                      (count . 2)
                                      (run_id . 7)
                                      (actions . (((kind . "reclaim")
                                                   (label . "Reclaim")
                                                   (suggested . t)))))))))
            (comments . (((id . 1) (author . "thanos")
                          (body . "needs eyes") (created_at . 1700000010))))
            (events . (((id . 2) (kind . "claimed")
                        (created_at . 1700000020)
                        (payload . ((run_id . 7))))))
            (attachments . (((id . 5) (filename . "report.txt")
                             (content_type . "text/plain") (size . 42)
                             (uploaded_by . "dashboard")
                             (stored_path . "/tmp/report.txt"))))
            (runs . (((id . 7) (profile . "elisp-dev")
                      (status . "finished") (outcome . "blocked")
                      (worker_pid . 1234)
                      (started_at . 1700000000) (ended_at . 1700000060)
                      (summary . "needs review")
                      (metadata . ((tests . 3)))
                      (error . "review-required"))))))
         (text (hermes-kanban--format-task-detail payload)))
    (should (string-match-p (regexp-quote "# Do thing") text))
    (should (string-match-p (regexp-quote "- Status: `⚙️ running`") text))
    (should (string-match-p "## Description" text))
    (should (string-match-p "## Run history (1)" text))
    (should (string-match-p (regexp-quote "### Run #7 — blocked @elisp-dev") text))
    (should (string-match-p "needs review" text))
    (should (string-match-p "tests" text))
    (should (string-match-p "## Diagnostics (1)" text))
    (should (string-match-p (regexp-quote "### [error] stale_running: Stale worker") text))
    (should (string-match-p "Reclaim" text))
    (should (string-match-p "## Attachments (1)" text))
    (should (string-match-p (regexp-quote "### report.txt (#5) (42 B)") text))
    (should (string-match-p "/tmp/report.txt" text))
    (should (string-match-p "## Comments (1)" text))
    (should (string-match-p (regexp-quote "— thanos") text))
    (should (string-match-p "needs eyes" text))
    (should (string-match-p "## Events (1)" text))
    (should (string-match-p (regexp-quote "— claimed") text))
    (should (string-match-p "Payload:" text))))

(ert-deftest hermes-kanban-format-task-detail-renders-empty-states ()
  "Task detail formatting names empty Markdown sections instead of omitting them."
  (let ((text (hermes-kanban--format-task-detail
               '((task . ((id . "t-empty") (title . "Empty task")
                          (status . "todo") (body . "")))
                 (comments) (events) (attachments) (runs)))))
    (should (string-match-p "## Diagnostics (0)" text))
    (should (string-match-p "— no diagnostics —" text))
    (should (string-match-p "## Attachments (0)" text))
    (should (string-match-p "— no attachments —" text))
    (should (string-match-p "## Comments (0)" text))
    (should (string-match-p "— no comments —" text))
    (should (string-match-p "## Events (0)" text))
    (should (string-match-p "— no events —" text))
    (should (string-match-p "## Run history (0)" text))
    (should (string-match-p "— no runs —" text))))

(ert-deftest hermes-kanban-format-task-shows-failure-fields ()
  "A distressed task surfaces branch, run, failure count, and last error."
  (let ((text (hermes-kanban--format-task
               '((id . "t9") (title . "Flaky") (status . "running")
                 (priority . 3) (assignee . "elisp-dev")
                 (created_at . 1700000000)
                 (branch_name . "feat/flaky")
                 (current_run_id . 42)
                 (consecutive_failures . 2)
                 (last_failure_error . "worker crashed")))))
    (should (string-match-p (regexp-quote "- Branch: `feat/flaky`") text))
    (should (string-match-p (regexp-quote "- Run: `#42`") text))
    (should (string-match-p (regexp-quote "- Failures: 2") text))
    (should (string-match-p (regexp-quote "- Last error: worker crashed") text))))

(ert-deftest hermes-kanban-format-task-hides-healthy-failure-fields ()
  "A healthy task adds no branch, run, failure, or error lines."
  (let ((text (hermes-kanban--format-task
               '((id . "t1") (title . "Fine") (status . "todo")
                 (priority . 5) (created_at . 1700000000)
                 (consecutive_failures . 0) (last_failure_error . nil)))))
    (should-not (string-match-p "- Branch:" text))
    (should-not (string-match-p "- Run:" text))
    (should-not (string-match-p "- Failures:" text))
    (should-not (string-match-p "- Last error:" text))))

(ert-deftest hermes-kanban-format-failure-fields-renders-present-only ()
  "Only present fields render; a lone branch yields just the branch line."
  (should (equal "" (hermes-kanban--format-failure-fields
                     '((consecutive_failures . 0)))))
  (should (equal "- Branch: `main`\n"
                 (hermes-kanban--format-failure-fields
                  '((branch_name . "main") (consecutive_failures . 0))))))

;;; Group N: recovery actions

(ert-deftest hermes-kanban-run-id-for-task-reads-current-run ()
  "The run id comes off the task's current_run_id; absent ids yield nil."
  (should (equal 7 (hermes-kanban--run-id-for-task '((current_run_id . 7)))))
  (should-not (hermes-kanban--run-id-for-task '((current_run_id))))
  (should-not (hermes-kanban--run-id-for-task '((id . "t1")))))

(ert-deftest hermes-kanban-reason-body-omits-empty-reason ()
  "A nil reason drops the body; a reason becomes a one-key alist."
  (should-not (hermes-kanban--reason-body nil))
  (should (equal '((reason . "stuck")) (hermes-kanban--reason-body "stuck"))))

(ert-deftest hermes-kanban-read-reason-trims-and-nils-blank ()
  "A blank reason reads as nil; surrounding whitespace is trimmed."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
    (should-not (hermes-kanban--read-reason "Reason: ")))
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  boom ")))
    (should (equal "boom" (hermes-kanban--read-reason "Reason: ")))))

(ert-deftest hermes-kanban-terminate-run-without-run-reports-and-skips ()
  "A task with no active run is reported and never hits the terminate endpoint."
  (let (calls msgs)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved nil)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
      (hermes-kanban--terminate-run-for-task '((id . "t1")) "t1" nil #'ignore)
      (should-not calls)
      (should (cl-some (lambda (m) (string-match-p "no active run" m)) msgs)))))

(ert-deftest hermes-kanban-terminate-run-posts-to-run-endpoint ()
  "Confirming terminates the resolved run id, omitting an empty reason."
  (let (calls refreshed)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (hermes-kanban--terminate-run-for-task
       '((id . "t1") (current_run_id . 42)) "t1" '((board . "emacs-lisp"))
       (lambda () (setq refreshed t)))
      (should (member '("POST" "/runs/42/terminate" nil ((board . "emacs-lisp")))
                      calls))
      (should refreshed))))

(ert-deftest hermes-kanban-terminate-run-keeps-origin-instance ()
  "A delayed task lookup terminates its run on the originating instance."
  (let* ((local '("local" . "http://127.0.0.1:9119"))
         (remote '("remote" . "https://hermes.example.test"))
         (hermes-instances (list local remote))
         (lookup (hermes--promise-make))
         (origin (generate-new-buffer " *Hermes terminate origin*"))
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-kanban--api)
                   (lambda (method path &optional _body _query)
                     (push (list method path (hermes-instance-resolve)) calls)
                     (if (equal method "GET")
                         lookup
                       (hermes--promise-resolved '((ok . t))))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) (ert-fail "Unexpected instance prompt")))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'read-string) (lambda (&rest _) ""))
                  ((symbol-function 'revert-buffer) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (with-current-buffer origin
            (hermes-kanban-task-mode)
            (setq hermes-instance remote
                  hermes-kanban-task--task-id "t1"
                  hermes-kanban-task--board-slug "work")
            (hermes-kanban-terminate-run))
          (with-temp-buffer
            (hermes--promise-resolve
             lookup '((task . ((id . "t1") (current_run_id . 42))))))
          (should (equal (nreverse calls)
                         `(("GET" "/tasks/t1" ,remote)
                           ("POST" "/runs/42/terminate" ,remote)))))
      (when (buffer-live-p origin) (kill-buffer origin)))))

(ert-deftest hermes-kanban-comment-posts-from-task-detail-buffer ()
  "Commenting from the task detail view posts to the task and refreshes."
  (let (calls refreshed)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'read-string-from-buffer)
               (lambda (prompt initial)
                 (should (equal prompt "Comment: "))
                 (should (equal initial ""))
                 "looks good"))
              ((symbol-function 'revert-buffer) (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (with-temp-buffer
        (hermes-kanban-task-mode)
        (setq hermes-kanban-task--task-id "t1"
              hermes-kanban-task--board-slug "emacs-lisp")
        (hermes-kanban-comment)
        (should (member '("POST" "/tasks/t1/comments" ((body . "looks good"))
                          ((board . "emacs-lisp")))
                        calls))
        (should refreshed)))))

(ert-deftest hermes-kanban-reclaim-posts-to-reclaim-endpoint ()
  "Reclaiming the task at point POSTs reclaim with the board query and reason."
  (let (calls)
    (cl-letf (((symbol-function 'window-body-width)
               (lambda (&optional _w _p) 80))
              ((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved
                  (if (equal path "/board")
                      '((columns . (((name . "running")
                                     (tasks . (((id . "t1") (status . "running")
                                                (title . "Do thing")))))))
                        (assignees))
                    '((ok . t))))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'read-string) (lambda (&rest _) "stuck"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
            (with-current-buffer "*Hermes Kanban*"
              (goto-char (point-min))
              (hermes-kanban-reclaim))
            (should (member '("POST" "/tasks/t1/reclaim"
                              ((reason . "stuck")) ((board . "emacs-lisp")))
                            calls)))
        (when (get-buffer "*Hermes Kanban*") (kill-buffer "*Hermes Kanban*"))))))

;;; Group N: diagnostics overview

(ert-deftest hermes-kanban-diagnostic-summary-counts-extra ()
  "A single diagnostic shows its title; extras add a (+N more) suffix."
  (should (equal "No heartbeat"
                 (hermes-kanban--diagnostic-summary '((title . "No heartbeat")) 1)))
  (should (equal "No heartbeat (+2 more)"
                 (hermes-kanban--diagnostic-summary '((title . "No heartbeat")) 3))))

(ert-deftest hermes-kanban-diagnostic-row-uses-top-and-falls-back ()
  "A row carries the task id, top severity, title, assignee, and summary."
  (let ((row (hermes-kanban--diagnostic-row
              '((task_id . "t1") (task_title . "Stuck task")
                (task_assignee . "elisp-dev")
                (diagnostics . [((severity . "critical") (title . "No heartbeat"))
                                ((severity . "warning") (title . "Retried"))])))))
    (should (equal "t1" (car row)))
    (should (equal ["critical" "Stuck task" "elisp-dev" "No heartbeat (+1 more)"]
                   (cadr row))))
  (let ((row (hermes-kanban--diagnostic-row
              '((task_id . "t2") (task_title . "")
                (diagnostics . [((severity . "warning") (title . "Slow"))])))))
    (should (equal ["warning" "t2" "-" "Slow"] (cadr row)))))

(ert-deftest hermes-kanban-diagnostic-rows-tolerates-missing-optionals ()
  "Rows build from groups whose diagnostics omit run_id and data."
  (let ((rows (hermes-kanban--diagnostic-rows
               [((task_id . "t1") (task_title . "A")
                 (diagnostics . [((severity . "error") (title . "X"))]))
                ((task_id . "t2") (task_title . "B")
                 (diagnostics . [((severity . "warning") (title . "Y"))]))])))
    (should (equal '("t1" "t2") (mapcar #'car rows)))))

(ert-deftest hermes-kanban-render-diagnostics-lists-tasks ()
  "Rendering fetches /diagnostics with the board query and lists distressed tasks."
  (let (query)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional _body q)
                 (should (equal method "GET"))
                 (should (equal path "/diagnostics"))
                 (setq query q)
                 (hermes--promise-resolved
                  '((diagnostics . [((task_id . "t1") (task_title . "Stuck")
                                     (task_assignee . "elisp-dev")
                                     (diagnostics . [((severity . "critical")
                                                      (title . "No heartbeat"))]))])
                    (count . 1)))))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-diagnostics "emacs-lisp" "Emacs Lisp")
            (should (equal (cdr (assq 'board query)) "emacs-lisp"))
            (with-current-buffer "*Hermes Kanban Diagnostics*"
              (should (derived-mode-p 'hermes-kanban-diagnostics-mode))
              (should (equal hermes-kanban-diagnostics--slug "emacs-lisp"))
              (should-not (local-variable-p 'hermes-kanban--slug))
              (should-not (local-variable-p 'hermes-kanban--assignees))
              (let (opened)
                (cl-letf (((symbol-function 'hermes-kanban--open-task)
                           (lambda (&rest args) (setq opened args))))
                  (goto-char (point-min))
                  (call-interactively (key-binding (kbd "RET"))))
                (should (equal opened '("t1" "emacs-lisp" nil))))
              (should (equal (caar tabulated-list-entries) "t1"))))
        (when (get-buffer "*Hermes Kanban Diagnostics*")
          (kill-buffer "*Hermes Kanban Diagnostics*"))))))

(ert-deftest hermes-kanban-legacy-diagnostics-refuse-public-operations ()
  "An old diagnostics view cannot silently dispatch to the active board."
  (let ((hermes-instances '(("test" . "http://example.test")))
        calls prompts)
    (with-temp-buffer
      (hermes-kanban-diagnostics-mode)
      ;; A retained pre-upgrade view owns only the old board-local fields.
      (setq hermes-instance (car hermes-instances)
            hermes-kanban--slug "retained-board"
            hermes-kanban--name "Retained"
            tabulated-list-entries
            (hermes-kanban--diagnostic-rows
             '(((task_id . "task") (task_title . "Task")))))
      (tabulated-list-print)
      (goto-char (point-min))
      (should-not (local-variable-p 'hermes-kanban-diagnostics--slug))
      (cl-letf (((symbol-function 'hermes-kanban--api)
                 (lambda (&rest args) (push args calls)
                   (hermes--promise-make)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (push t prompts) "blocked")))
        (dolist (command (list (key-binding (kbd "g"))
                              (key-binding (kbd "RET"))
                              #'hermes-kanban-set-status))
          (should-error (call-interactively command) :type 'user-error))
        (should-not calls)
        (should-not prompts)
        ;; Installing the new context (as reopening does) restores dispatch.
        (setq hermes-kanban-diagnostics--slug "retained-board")
        (call-interactively (key-binding (kbd "g")))
        (call-interactively (key-binding (kbd "RET")))
        (call-interactively #'hermes-kanban-set-status)
        (should (= (length calls) 3))
        (should (equal (mapcar #'car (reverse calls)) '("GET" "GET" "PATCH")))
        (dolist (call calls)
          (should (equal (nth 3 call) '((board . "retained-board")))))))))

(ert-deftest hermes-kanban-render-diagnostics-inherits-buffer-instance ()
  "A diagnostics buffer inherits its board's instance."
  (let ((instance '("remote" . "https://hermes.example.test"))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test"))))
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (&rest _)
                 (hermes--promise-resolved '((diagnostics . nil))))))
      (unwind-protect
          (with-temp-buffer
            (setq hermes-instance instance)
            (hermes-kanban--render-diagnostics "work" "Work")
            (with-current-buffer "*Hermes Kanban Diagnostics*"
              (should (equal hermes-instance instance))))
        (when (get-buffer "*Hermes Kanban Diagnostics*")
          (kill-buffer "*Hermes Kanban Diagnostics*"))))))

(ert-deftest hermes-kanban-render-diagnostics-reports-empty-board ()
  "An empty board renders no rows and reports the empty state."
  (let (msgs)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (_m _p &optional _b _q)
                 (hermes--promise-resolved '((diagnostics . []) (count . 0)))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
      (unwind-protect
          (progn
            (hermes-kanban--render-diagnostics "emacs-lisp" "Emacs Lisp")
            (with-current-buffer "*Hermes Kanban Diagnostics*"
              (should-not tabulated-list-entries))
            (should (cl-some (lambda (m) (string-match-p "No active diagnostics" m))
                             msgs)))
        (when (get-buffer "*Hermes Kanban Diagnostics*")
          (kill-buffer "*Hermes Kanban Diagnostics*"))))))

(ert-deftest hermes-kanban-diagnostics-discard-late-response ()
  "A late diagnostics response cannot replace the latest board refresh."
  (let ((old (hermes--promise-make)) (new (hermes--promise-make)))
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (_method _path &optional _body query)
                 (if (equal (cdr (assq 'board query)) "old") old new)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (unwind-protect
          (progn
            (hermes-kanban--render-diagnostics "old" "Old")
            (hermes-kanban--render-diagnostics "new" "New")
            (hermes--promise-resolve
             new '((diagnostics . (((task_id . "new-task")
                                     (diagnostics . (((severity . "warning")
                                                      (title . "New")))))))))
            (hermes--promise-resolve
             old '((diagnostics . (((task_id . "old-task")
                                     (diagnostics . (((severity . "error")
                                                      (title . "Old")))))))))
            (with-current-buffer "*Hermes Kanban Diagnostics*"
              (should (equal hermes-kanban-diagnostics--slug "new"))
              (should (equal (caar tabulated-list-entries) "new-task"))))
        (when (get-buffer "*Hermes Kanban Diagnostics*")
          (kill-buffer "*Hermes Kanban Diagnostics*"))))))

(ert-deftest hermes-kanban-diagnostics-ignore-late-rejection ()
  "A stale diagnostics rejection cannot report an error after newer success."
  (let ((old (hermes--promise-make)) (new (hermes--promise-make)) messages)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (_method _path &optional _body query)
                 (if (equal (cdr (assq 'board query)) "old") old new)))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-kanban--render-diagnostics "old" "Old")
            (hermes-kanban--render-diagnostics "new" "New")
            (hermes--promise-resolve new '((diagnostics . [])))
            (hermes--promise-reject old "stale diagnostics error")
            (should-not
             (seq-some
              (lambda (text) (string-match-p "stale diagnostics" text))
              messages)))
        (when (get-buffer "*Hermes Kanban Diagnostics*")
          (kill-buffer "*Hermes Kanban Diagnostics*"))))))

(defun hermes-kanban-test--face-match-p (face expected)
  "Return non-nil when FACE contains EXPECTED."
  (cond
   ((eq face expected) t)
   ((listp face) (memq expected face))))

(defun hermes-kanban-test--line-has-face-p (text line expected)
  "Return non-nil when LINE in TEXT has EXPECTED face on any character."
  (when-let* ((start (string-match (regexp-quote line) text)))
    (cl-loop for i from start below (+ start (length line))
             thereis (hermes-kanban-test--face-match-p
                      (get-text-property i 'face text)
                      expected))))

(ert-deftest hermes-kanban-show-log-fetches-selected-task-log ()
  "Log viewing goes through the dashboard REST endpoint for the selected task."
  (let (log-path log-query)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional _body query)
                 (should (equal method "GET"))
                 (hermes--promise-resolved (cond
					    ((equal path "/board")
					     '((columns . (((name . "running")
							    (tasks . (((id . "t1") (status . "running")
								       (title . "Do thing")))))))
					       (assignees . ("elisp-dev"))))
					    ((equal path "/tasks/t1/log")
					     (setq log-path path
						   log-query query)
					     '((task_id . "t1") (path . "/logs/t1.log")
					       (exists . t) (size_bytes . 12)
					       (content . "a/foo.el → b/foo.el\n@@ -0,0 +1,4 @@\n+one\n+two\n… omitted 2 diff line(s) across 1 additional file(s)/section(s)\n")
					       (truncated . :json-false)))
					    (t (error "unexpected path: %s" path)))))))
      (unwind-protect
          (progn
            (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
            (with-current-buffer "*Hermes Kanban*"
              (goto-char (point-min))
              (hermes-kanban-show-log))
            (should (equal log-path "/tasks/t1/log"))
            (should (equal (cdr (assq 'board log-query)) "emacs-lisp"))
            (should (equal (cdr (assq 'tail log-query)) 100000))
            (with-current-buffer "*Hermes Kanban Log*"
              (should (derived-mode-p 'hermes-kanban-log-mode))
              (should (equal hermes-kanban-log--task-id "t1"))
              (should (equal hermes-kanban-log--board-slug "emacs-lisp"))
              (let ((text (buffer-string)))
                (should (string-match-p "Worker log for t1" text))
                (should (string-match-p "/logs/t1.log" text))
                (should (hermes-kanban-test--line-has-face-p
                         text "@@ -0,0 +1,4 @@" 'diff-hunk-header))
                (should (hermes-kanban-test--line-has-face-p
                         text "+one" 'diff-added)))))
        (dolist (b '("*Hermes Kanban*" "*Hermes Kanban Log*"))
          (when (get-buffer b) (kill-buffer b)))))))

(ert-deftest hermes-kanban-format-log-renders-empty-and-error-states ()
  "Worker log formatting is explicit when the backend reports no log or an error."
  (should (string-match-p "no worker log"
                          (hermes-kanban--format-log
                           '((task_id . "t1") (exists . :json-false)
                             (content . "")))))
  (should (string-match-p "failed to load worker log: boom"
                          (hermes-kanban--format-log
                           '((task_id . "t1") (error . "boom"))))))

(ert-deftest hermes-kanban-format-log-sanitizes-control-output ()
  "Worker log formatting renders CR and ANSI control output readably."
  (let* ((text (hermes-kanban--format-log
                `((task_id . "t1") (exists . t)
                  (content . ,(concat "start\rprogress\r\ndone\n"
                                      "\33[31merror\33[0m\n")))))
         (plain (substring-no-properties text)))
    (should-not (string-match-p "\r" plain))
    (should-not (string-match-p (regexp-quote "\33[") plain))
    (should (string-match-p "start\nprogress\ndone" plain))
    (should (string-match-p "error" plain))))

(ert-deftest hermes-kanban-format-log-fontifies-embedded-diff ()
  "Worker log formatting applies diff faces to embedded unified diffs."
  (let* ((content (concat "before diff\n"
                          "a//lisp/foo.el → b//lisp/foo.el\n"
                          "@@ -17,2 +17,2 @@\n"
                          " context\n"
                          "-old\n"
                          "+new\n"
                          "middle diff\n"
                          "diff --git a/lisp/bar.el b/lisp/bar.el\n"
                          "--- a/lisp/bar.el\n"
                          "+++ b/lisp/bar.el\n"
                          "@@ -1 +1 @@\n"
                          "-before\n"
                          "+after\n"
                          "after diff\n"))
         (text (hermes-kanban--format-log
                `((task_id . "t1") (exists . t) (content . ,content))))
         (plain (substring-no-properties text)))
    (should (string-match-p "before diff" plain))
    (should (string-match-p (regexp-quote "@@ -17,2 +17,2 @@") plain))
    (should (string-match-p "-old" plain))
    (should (string-match-p "\\+new" plain))
    (should (string-match-p (regexp-quote "@@ -1 +1 @@") plain))
    (should (string-match-p "-before" plain))
    (should (string-match-p "\\+after" plain))
    (should (string-match-p "after diff" plain))
    (should (hermes-kanban-test--line-has-face-p
             text "@@ -17,2 +17,2 @@" 'diff-hunk-header))
    (should (hermes-kanban-test--line-has-face-p
             text "-old" 'diff-indicator-removed))
    (should (hermes-kanban-test--line-has-face-p
             text "-old" 'diff-removed))
    (should (hermes-kanban-test--line-has-face-p
             text "+new" 'diff-indicator-added))
    (should (hermes-kanban-test--line-has-face-p
             text "+new" 'diff-added))
    (should (hermes-kanban-test--line-has-face-p
             text "@@ -1 +1 @@" 'diff-hunk-header))
    (should (hermes-kanban-test--line-has-face-p
             text "-before" 'diff-removed))
    (should (hermes-kanban-test--line-has-face-p
             text "+after" 'diff-added))))

(ert-deftest hermes-kanban-format-log-fontifies-truncated-hermes-diff ()
  "Worker log formatting accepts Hermes' explicit diff omission marker."
  (let* ((content (concat "  ┊ review diff\n"
                          "a/foo.el → b/foo.el\n"
                          "@@ -0,0 +1,4 @@\n"
                          "+one\n"
                          "+two\n"
                          "… omitted 2 diff line(s) across "
                          "1 additional file(s)/section(s)\n"))
         (text (hermes-kanban--format-log
                `((task_id . "t1") (exists . t) (content . ,content)))))
    (should (hermes-kanban-test--line-has-face-p
             text "@@ -0,0 +1,4 @@" 'diff-hunk-header))
    (should (hermes-kanban-test--line-has-face-p
             text "+one" 'diff-added))
    (should (hermes-kanban-test--line-has-face-p
             text "+two" 'diff-added))
    (should (string-match-p "… omitted 2 diff line" text))))

(ert-deftest hermes-kanban-format-log-does-not-fontify-ordinary-plus-minus-lines ()
  "Worker log formatting ignores ordinary plus/minus lines without hunks."
  (let* ((content (concat "worker said\n"
                          "+not a diff addition\n"
                          "-not a diff removal\n"
                          "@@ -1 +1 @@\n"
                          "-incomplete hunk\n"))
         (text (hermes-kanban--format-log
                `((task_id . "t1") (exists . t) (content . ,content))))
         (plain (substring-no-properties text)))
    (should (string-match-p "\\+not a diff addition" plain))
    (should (string-match-p "-not a diff removal" plain))
    (should (string-match-p (regexp-quote "@@ -1 +1 @@") plain))
    (should (string-match-p "-incomplete hunk" plain))
    (dolist (face '(diff-added diff-indicator-added diff-removed
                    diff-indicator-removed diff-hunk-header))
      (should-not (hermes-kanban-test--line-has-face-p
                   text "+not a diff addition" face))
      (should-not (hermes-kanban-test--line-has-face-p
                   text "-not a diff removal" face))
      (should-not (hermes-kanban-test--line-has-face-p
                   text "@@ -1 +1 @@" face))
      (should-not (hermes-kanban-test--line-has-face-p
                   text "-incomplete hunk" face)))))

(ert-deftest hermes-kanban-log-rejects-bare-blank-diff-body-line ()
  "A bare blank line is not valid unified-diff context."
  (with-temp-buffer
    (insert "@@ -1,2 +1,2 @@\n-old\n+new\n\n")
    (goto-char (point-min))
    (should-not (hermes-kanban--consume-diff-hunk))))

(ert-deftest hermes-kanban-log-refontify-restores-stale-buffer-faces ()
  "Refontifying a stale log buffer restores embedded diff faces."
  (with-temp-buffer
    (hermes-kanban-log-mode)
    (let ((inhibit-read-only t))
      (insert (concat "worker said\n"
                      "diff --git a/foo.el b/foo.el\n"
                      "--- a/foo.el\n"
                      "+++ b/foo.el\n"
                      "@@ -1 +1 @@\n"
                      "-old\n"
                      "+new\n")))
    (set-buffer-modified-p nil)
    (should-not (hermes-kanban-test--line-has-face-p
                 (buffer-string) "+new" 'diff-added))
    (hermes-kanban-log--refontify-buffer)
    (should (hermes-kanban-test--line-has-face-p
             (buffer-string) "@@ -1 +1 @@" 'diff-hunk-header))
    (should (hermes-kanban-test--line-has-face-p
             (buffer-string) "-old" 'diff-removed))
    (should (hermes-kanban-test--line-has-face-p
             (buffer-string) "+new" 'diff-added))
    (should-not (buffer-modified-p))))

(ert-deftest hermes-kanban-log-mode-navigates-embedded-diff-hunks ()
  "Log-mode n/p commands move across embedded unified diff hunks.
Incomplete header-shaped blocks that the fontifier rejects are skipped."
  (with-temp-buffer
    (hermes-kanban-log-mode)
    (let ((inhibit-read-only t))
      (insert (hermes-kanban--render-log-content
               (concat "worker said\n"
                       ;; Incomplete hunk-shaped block: a header that
                       ;; announces one old and one new line, but the
                       ;; following lines are not +/- body lines, so
                       ;; `hermes-kanban--consume-diff-hunk' rejects it
                       ;; and the fontifier does not fontify it.
                       "@@ -1 +1 @@\n"
                       "this is just prose, not a diff body\n"
                       "a//lisp/foo.el → b//lisp/foo.el\n"
                       "@@ -1 +1 @@\n"
                       "-old\n"
                       "+new\n"
                       "between\n"
                       "@@ -5 +5 @@\n"
                       "-alpha\n"
                       "+beta\n"))))
    (should (eq (lookup-key hermes-kanban-log-mode-map (kbd "n"))
                'hermes-kanban-log-next-hunk))
    (should (eq (lookup-key hermes-kanban-log-mode-map (kbd "p"))
                'hermes-kanban-log-previous-hunk))
    (goto-char (point-min))
    ;; `n' must skip the incomplete header block at the top and land on
    ;; the first VALID hunk inside the fontified diff.
    (hermes-kanban-log-next-hunk)
    (should (looking-at (regexp-quote "@@ -1 +1 @@")))
    (let ((first-hunk (point)))
      (hermes-kanban-log-next-hunk)
      (should (looking-at (regexp-quote "@@ -5 +5 @@")))
      ;; `p' must also skip the incomplete block and land back on the
      ;; first valid hunk, not on the bogus header above it.
      (hermes-kanban-log-previous-hunk)
      (should (= (point) first-hunk)))))

;;; Group N: live events tail

(defun hermes-kanban-test--await (predicate)
  "Wait briefly for PREDICATE while servicing disposable socket traffic."
  (let ((deadline (+ (float-time) 3)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(ert-deftest hermes-kanban-events-callback-fault-retires-real-socket ()
  "Callback faults close both peers before reconnect; stale callbacks are inert."
  (require 'websocket)
  (dolist (frame '("{\"events\":[{\"id\":1,\"task_id\":\"task\",\"kind\":\"blocked\"}],\"cursor\":1}"
                   "{\"events\":42,\"cursor\":1}"))
    (let (server peers client successor tail old-close old-error)
      (with-temp-buffer
        (hermes-kanban-mode)
        (setq hermes-instance '("test" . "http://example.invalid")
              hermes-kanban--slug "board")
        (unwind-protect
            (progn
              (setq server (websocket-server
                            0 :host "127.0.0.1"
                            :on-open (lambda (ws) (push ws peers))))
              (let* ((url (format "ws://127.0.0.1:%d/events"
                                  (process-contact server :service)))
                     (hermes-kanban--events-debounce 60))
                (cl-letf (((symbol-function
                            'hermes-dashboard-transport-kanban-events-url-async)
                           (lambda (&rest _)
                             (hermes--promise-resolved
                              (list :url url :redacted-url url :secrets nil))))
                          ((symbol-function 'hermes-notifications-notify)
                           (lambda (&rest _) (error "Injected notification failure"))))
                  (hermes-kanban-toggle-live)
                  (setq tail hermes-kanban--events-tail
                        client (hermes-kanban--events-tail-socket tail)
                        old-close (websocket-on-close client)
                        old-error (websocket-on-error client))
                  (hermes-kanban-test--await
                   (lambda () (and peers (eq 'open (websocket-ready-state client)))))
                  (websocket-send-text (car peers) frame)
                  (hermes-kanban-test--await
                   (lambda () (hermes-kanban--events-tail-reconnect-timer tail)))
                  ;; Physical closure, not just dropping the owner's reference.
                  (should-not (process-live-p (websocket-conn client)))
                  (hermes-kanban-test--await
                   (lambda () (not (process-live-p (websocket-conn (car peers))))))
                  (cancel-timer (hermes-kanban--events-tail-reconnect-timer tail))
                  (hermes-kanban--events-do-reconnect tail)
                  (setq successor (hermes-kanban--events-tail-socket tail))
                  (hermes-kanban-test--await
                   (lambda () (and (= 2 (length peers))
                                   (eq 'open (websocket-ready-state successor)))))
                  (funcall old-close client)
                  (funcall old-error client 'on-message '(error "Late fault"))
                  (should (eq successor (hermes-kanban--events-tail-socket tail)))
                  (should (websocket-openp successor))
                  (should-not (hermes-kanban--events-tail-reconnect-timer tail))
                  (should (= 1 (seq-count #'websocket-openp peers)))
                  (hermes-kanban-toggle-live)
                  (should-not hermes-kanban--events-tail)
                  (should-not (process-live-p (websocket-conn successor)))
                  (hermes-kanban-test--await
                   (lambda () (not (seq-some #'websocket-openp peers)))))))
          (when tail (hermes-kanban--events-disconnect tail))
          (when client (websocket-close client))
          (when successor (websocket-close successor))
          (when server (websocket-server-close server)))))))

(ert-deftest hermes-kanban-notification-action-keeps-original-owner ()
  "A retained action cannot select colliding tasks after board or instance reuse."
  (dolist (replacement '(board instance disabled mode killed display))
    (let ((buffer (generate-new-buffer " *kanban notification owner*"))
          callback opened)
      (unwind-protect
          (with-current-buffer buffer
            (hermes-kanban-mode)
            (hermes-browser--own-instance '("first" . "http://first.invalid"))
            (setq hermes-kanban--slug "first-board"
                  tabulated-list-entries
                  '(("other" ["todo" "0" "worker" "Other"])
                    ("task" ["todo" "0" "worker" "Original"])))
            (tabulated-list-print t)
            (let ((tail (hermes-kanban--events-tail-create
                         :buffer buffer :slug hermes-kanban--slug
                         :instance hermes-instance)))
              (setq hermes-kanban--events-tail tail)
              (cl-letf (((symbol-function 'hermes-notifications-notify)
                         (lambda (_event _title _body &rest opts)
                           (setq callback (plist-get opts :open))))
                        ((symbol-function 'pop-to-buffer)
                         (lambda (target &rest _) (push target opened)))
                        ((symbol-function 'hermes-kanban--events-connect) #'ignore)
                        ((symbol-function 'hermes-kanban--api)
                         (lambda (&rest _) (ert-fail "Notification dispatched HTTP"))))
                (hermes-kanban--notify-event
                 tail '(:task-id "task" :event kanban-attention :label "blocked"))
                ;; Ordinary refreshes do not retire the board's notification.
                (hermes-browser--next-request-generation)
                (with-temp-buffer (funcall callback))
                (should (equal opened (list buffer)))
                (should (equal (tabulated-list-get-id) "task"))
                (setq opened nil)
                (pcase replacement
                  ('board (setq hermes-kanban--slug "second-board"))
                  ('instance
                   (hermes-browser--own-instance '("second" . "http://second.invalid")))
                  ('disabled (hermes-kanban-toggle-live))
                  ('mode (fundamental-mode))
                  ('killed (kill-buffer buffer))
                  ('display
                   ;; Display hooks may retarget the buffer before row selection.
                   (cl-letf (((symbol-function 'pop-to-buffer)
                              (lambda (&rest _)
                                (setq hermes-kanban--slug "second-board")
                                (goto-char (point-min)))))
                     (funcall callback))
                   (should (equal (tabulated-list-get-id) "other"))))
                (when (memq replacement '(board instance))
                  ;; Matching IDs in the successor must not lend authority.
                  (setq tabulated-list-entries
                        '(("other" ["todo" "0" "worker" "Other"])
                          ("task" ["todo" "0" "worker" "Unrelated successor"])))
                  (tabulated-list-print t)
                  (goto-char (point-min))
                  (funcall callback)
                  (should-not opened)
                  (should (equal (tabulated-list-get-id) "other"))
                  (hermes-kanban--events-retarget hermes-kanban--slug 0))
                (funcall callback)
                (should-not opened))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-kanban-notification-classifies-attention-and-done-events ()
  "Kanban notification policy ignores routine activity and classifies outcomes."
  (should (equal (hermes-kanban--event-notice
                  '((task_id . "t1") (kind . "blocked")))
                 '(:task-id "t1" :event kanban-attention
                   :label "blocked" :urgency critical)))
  (should (equal (hermes-kanban--event-notice
                  '((task_id . "t2") (kind . "status")
                    (payload . ((status . "review")))))
                 '(:task-id "t2" :event kanban-attention
                   :label "ready for review" :urgency normal)))
  (should (equal (hermes-kanban--event-notice
                  '((task_id . "t3") (kind . "completed")))
                 '(:task-id "t3" :event kanban-done
                   :label "completed" :urgency normal)))
  (should-not (hermes-kanban--event-notice
               '((task_id . "t4") (kind . "heartbeat")))))

(ert-deftest hermes-kanban-notification-batch-keeps-last-transition-per-task ()
  "One frame emits only the last meaningful transition for each task."
  (should
   (equal
    (hermes-kanban--event-notices
     '(((task_id . "t1") (kind . "blocked"))
       ((task_id . "t1") (kind . "status")
        (payload . ((status . "review"))))
       ((task_id . "t2") (kind . "claimed"))
       ((task_id . "t2") (kind . "timed_out"))))
    '((:task-id "t1" :event kanban-attention
       :label "ready for review" :urgency normal)
      (:task-id "t2" :event kanban-attention
       :label "timed out" :urgency critical)))))

(ert-deftest hermes-kanban-notification-default-skips-done-but-shows-blocked ()
  "The default event set emits attention notices but not routine completion."
  (let ((tail (hermes-kanban--events-tail-create
               :buffer (current-buffer) :cursor 1))
        notifications)
    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil))
              ((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'notifications-notify)
               (lambda (&rest args) (push args notifications) 12))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'timer)))
      (hermes-kanban--events-handle-frame
       tail
       (concat "{\"events\":["
               "{\"id\":4,\"task_id\":\"done-task\",\"kind\":\"completed\"},"
               "{\"id\":5,\"task_id\":\"blocked-task\",\"kind\":\"blocked\"}"
               "],\"cursor\":5}"))
      (should (= 1 (length notifications)))
      (should (string-match-p "blocked-task"
                              (plist-get (car notifications) :body))))))

(ert-deftest hermes-kanban-notification-click-selects-task-row ()
  "Opening a Kanban notice displays its board and selects the matching task."
  (with-temp-buffer
    (hermes-kanban-mode)
    (setq tabulated-list-entries
          '(("t1" ["⚙️" "1" "worker" "First task"])
            ("t2" ["⛔" "2" "worker" "Second task"])))
    (tabulated-list-print t)
    (let ((buffer (current-buffer)))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (&rest _) buffer)))
        (hermes-kanban--open-notification-task buffer "t2"))
      (should (equal (tabulated-list-get-id) "t2"))
      (should (equal (hermes-kanban--notification-task-title buffer "t2")
                     "Second task")))))

(ert-deftest hermes-kanban-events-handle-frame-advances-cursor-and-schedules ()
  "A `{events,cursor}' frame advances the cursor and debounces one refresh."
  (let ((tail (hermes-kanban--events-tail-create
               :buffer (current-buffer) :cursor 1))
        scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (setq scheduled t) 'timer)))
      (hermes-kanban--events-handle-frame
       tail "{\"events\":[{\"id\":5}],\"cursor\":5}")
      (should (= 5 (hermes-kanban--events-tail-cursor tail)))
      (should scheduled))))

(ert-deftest hermes-kanban-events-handle-frame-retries-bad-json ()
  "A malformed frame preserves the cursor and schedules only reconnect."
  (let ((tail (hermes-kanban--events-tail-create
               :buffer (current-buffer) :cursor 3))
        scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (setq scheduled t) 'timer)))
      (hermes-kanban--events-handle-frame tail "not json")
      (should (= 3 (hermes-kanban--events-tail-cursor tail)))
      (should scheduled)
      (should-not (hermes-kanban--events-tail-refresh-timer tail)))))

(ert-deftest hermes-kanban-live-indicator-reflects-tail-state ()
  "The indicator is shadow when off, warning while retrying, success when live."
  (with-temp-buffer
    (should (eq 'shadow (get-text-property 0 'face (hermes-kanban--live-indicator))))
    (setq-local hermes-kanban--events-tail (hermes-kanban--events-tail-create))
    (let ((ind (hermes-kanban--live-indicator)))
      (should (string-match-p "retry" ind))
      (should (eq 'warning (get-text-property 1 'face ind))))
    (setf (hermes-kanban--events-tail-socket hermes-kanban--events-tail) 'ws)
    (let ((ind (hermes-kanban--live-indicator)))
      (should (string-match-p "live" ind))
      (should (eq 'success (get-text-property 1 'face ind))))))

(ert-deftest hermes-kanban-live-indicator-requires-current-board-tail ()
  "A connected tail for another slug is not shown as live on this board."
  (with-temp-buffer
    (setq-local hermes-kanban--slug "b"
                hermes-kanban--events-tail
                (hermes-kanban--events-tail-create :slug "a" :socket 'ws))
    (let ((indicator (hermes-kanban--live-indicator)))
      (should-not (string-match-p "live" indicator))
      (should (eq 'warning (get-text-property 1 'face indicator))))))

(ert-deftest hermes-kanban-events-stale-close-does-not-clear-new-socket ()
  "The close callback from socket A cannot clear replacement socket B."
  (let* ((socket-a 'socket-a)
         (socket-b 'socket-b)
         (tail (hermes-kanban--events-tail-create
                :buffer (current-buffer) :socket socket-b))
         scheduled)
    (cl-letf (((symbol-function 'hermes-kanban--events-reconnect)
               (lambda (&rest _) (setq scheduled t))))
      (hermes-kanban--events-on-down tail socket-a)
      (should (eq (hermes-kanban--events-tail-socket tail) socket-b))
      (should-not scheduled))))

(ert-deftest hermes-kanban-events-connect-failure-schedules-reconnect ()
  "A failed URL resolve re-enters the reconnect backoff instead of dying."
  (let (scheduled
        (tail (hermes-kanban--events-tail-create :buffer (current-buffer))))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay &rest _) (push delay scheduled) 'timer))
              ((symbol-function
                'hermes-dashboard-transport-kanban-events-url-async)
               (lambda (&rest _) (hermes--promise-rejected "boom"))))
      (hermes-kanban--events-connect tail)
      (should (equal scheduled '(1))))))

(ert-deftest hermes-kanban-events-connect-uses-owner-instance-url ()
  "A board's live-events socket resolves against its pinned instance."
  (let ((instance '("remote" . "https://hermes.example.test"))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test")))
        seen-url)
    (with-temp-buffer
      (setq hermes-instance instance)
      (let ((tail (hermes-kanban--events-tail-create
                   :buffer (current-buffer) :slug "work")))
        (cl-letf (((symbol-function
                    'hermes-dashboard-transport-kanban-events-url-async)
                   (lambda (&rest _)
                     (setq seen-url hermes-dashboard-transport-url)
                     (hermes--promise-rejected "stop")))
                  ((symbol-function 'hermes-kanban--events-on-down) #'ignore))
          (hermes-kanban--events-connect tail)
          (should (equal seen-url (hermes-instance-url instance))))))))

(ert-deftest hermes-kanban-events-reconnect-backs-off-and-stops-when-dead ()
  "Reconnect doubles the backoff, never double-schedules, and stops if dead."
  (let (scheduled
        (tail (hermes-kanban--events-tail-create
               :buffer (current-buffer) :backoff 2)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay &rest _) (push delay scheduled) 'timer)))
      (hermes-kanban--events-reconnect tail)
      (should (equal scheduled '(2)))
      (should (= 4 (hermes-kanban--events-tail-backoff tail)))
      (hermes-kanban--events-reconnect tail)
      (should (equal scheduled '(2)))))
  (let ((dead (generate-new-buffer "k")) (count 0))
    (kill-buffer dead)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (cl-incf count) 'timer)))
      (hermes-kanban--events-reconnect
       (hermes-kanban--events-tail-create :buffer dead))
      (should (= 0 count)))))

(ert-deftest hermes-kanban-toggle-live-requires-board-mode ()
  "Toggling live updates outside a board buffer signals a `user-error'."
  (with-temp-buffer
    (should-error (hermes-kanban-toggle-live) :type 'user-error)))

(ert-deftest hermes-kanban-toggle-live-on-seeds-cursor-then-off ()
  "Toggling on seeds the cursor from the last render and installs teardown."
  (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80))
            ((symbol-function 'hermes-kanban--events-connect) #'ignore))
    (with-temp-buffer
      (hermes-kanban-mode)
      (setq hermes-kanban--slug "emacs-lisp"
            hermes-kanban--latest-event-id 7)
      (hermes-kanban-toggle-live)
      (should hermes-kanban--events-tail)
      (should (= 7 (hermes-kanban--events-tail-cursor
                    hermes-kanban--events-tail)))
      (should (memq #'hermes-kanban--events-teardown kill-buffer-hook))
      (should (memq #'hermes-kanban--events-teardown change-major-mode-hook))
      (hermes-kanban-toggle-live)
      (should-not hermes-kanban--events-tail))))

(ert-deftest hermes-kanban-mode-exit-and-kill-release-tail-once ()
  "Mode exit and kill each deactivate and release one captured live tail."
  (dolist (exit '(mode-change kill))
    (let ((buffer (generate-new-buffer " *hermes-kanban-cleanup*"))
          cancelled closed old-tail)
      (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled)))
                ((symbol-function 'websocket-close)
                 (lambda (socket) (push socket closed))))
        (unwind-protect
            (with-current-buffer buffer
              (hermes-kanban-mode)
              (setq old-tail
                    (hermes-kanban--events-tail-create
                     :buffer buffer :slug "old" :socket 'old-socket
                     :refresh-timer 'refresh :reconnect-timer 'reconnect)
                    hermes-kanban--events-tail old-tail)
              (if (eq exit 'kill)
                  (kill-buffer buffer)
                (fundamental-mode)
                (hermes-kanban-mode)
                (let ((successor
                       (hermes-kanban--events-tail-create
                        :buffer buffer :slug "new")))
                  (setq hermes-kanban--events-tail successor)
                  (cl-letf (((symbol-function 'revert-buffer)
                             (lambda (&rest _) (ert-fail "stale refresh")))
                            ((symbol-function 'hermes-kanban--events-connect)
                             (lambda (&rest _) (ert-fail "stale reconnect"))))
                    (hermes-kanban--events-refresh old-tail)
                    (hermes-kanban--events-do-reconnect old-tail))
                  (should (eq hermes-kanban--events-tail successor)))
                (kill-buffer buffer)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
        (should-not (hermes-kanban--events-tail-active old-tail))
        (should (equal (sort cancelled
                             (lambda (left right)
                               (string< (symbol-name left) (symbol-name right))))
                       '(reconnect refresh)))
        (should (equal closed '(old-socket)))))))

(ert-deftest hermes-kanban-render-board-seeds-latest-event-id ()
  "Rendering a board records its latest_event_id for live seeding."
  (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80))
            ((symbol-function 'hermes-kanban--api)
             (lambda (_m _p &optional _b _q)
               (hermes--promise-resolved
                '((columns . (((name . "todo") (tasks . []))))
                  (assignees) (latest_event_id . 42))))))
    (unwind-protect
        (progn
          (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
          (with-current-buffer "*Hermes Kanban*"
            (should (= 42 hermes-kanban--latest-event-id))))
      (when (get-buffer "*Hermes Kanban*") (kill-buffer "*Hermes Kanban*")))))

(ert-deftest hermes-kanban-render-board-inherits-buffer-instance ()
  "A board detail buffer inherits the overview's instance."
  (let ((instance '("remote" . "https://hermes.example.test"))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test"))))
    (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80))
              ((symbol-function 'hermes-kanban--api)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '((columns . nil) (assignees . nil))))))
      (unwind-protect
          (with-temp-buffer
            (setq hermes-instance instance)
            (hermes-kanban--render-board "work" "Work")
            (with-current-buffer "*Hermes Kanban*"
              (should (equal hermes-instance instance))))
        (when (get-buffer "*Hermes Kanban*")
          (kill-buffer "*Hermes Kanban*"))))))

(ert-deftest hermes-kanban-open-task-inherits-buffer-instance ()
  "A task detail buffer inherits its board's instance."
  (let ((instance '("remote" . "https://hermes.example.test"))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test"))))
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '((task . ((id . "t1") (title . "Task")
                             (status . "todo") (body . "Body"))))))))
      (unwind-protect
          (with-temp-buffer
            (setq hermes-instance instance)
            (hermes-kanban-open-task "t1" "work")
            (with-current-buffer "*Hermes Kanban Task*"
              (should (equal hermes-instance instance))))
        (when (get-buffer "*Hermes Kanban Task*")
          (kill-buffer "*Hermes Kanban Task*"))))))

(ert-deftest hermes-kanban-profile-candidates-merge-cache-and-board-assignees ()
  "Candidates merge the warmed profile cache with board-known assignees."
  (let ((hermes-dashboard-transport--profile-cache nil))
    (hermes-dashboard-transport--store-profile-cache
     '((profiles . (((name . "default") (is_default . t))
                    ((name . "elisp-dev"))
                    ((name . "reviewer"))))))
    (with-temp-buffer
      (hermes-kanban-mode)
      (setq hermes-kanban--assignees '("elisp-dev" "spike"))
      (should (equal (hermes-kanban--profile-candidates)
                     '("default" "elisp-dev" "reviewer" "spike"))))))

(ert-deftest hermes-kanban-profile-candidates-empty-when-no-source ()
  "With no cache and no board assignees, candidates is empty and never errors."
  (let ((hermes-dashboard-transport--profile-cache nil))
    (with-temp-buffer
      (hermes-kanban-mode)
      (setq hermes-kanban--assignees nil)
      (should (equal (hermes-kanban--profile-candidates) nil)))))

(ert-deftest hermes-kanban-profile-candidates-use-task-detail-assignees ()
  "Task detail completions include assignees captured from the board."
  (let ((hermes-dashboard-transport--profile-cache nil))
    (with-temp-buffer
      (hermes-kanban-task-mode)
      (setq hermes-kanban-task--assignees '("elisp-dev" "reviewer"))
      (should (equal (hermes-kanban--profile-candidates)
                     '("elisp-dev" "reviewer"))))))

(ert-deftest hermes-kanban-change-assignee-patches-from-task-detail ()
  "Changing assignee from the task detail PATCHes /tasks/:id and reverts."
  (let (calls reverted)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "elisp-dev"))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (with-temp-buffer
        (hermes-kanban-task-mode)
        (setq hermes-kanban-task--task-id "t1"
              hermes-kanban-task--board-slug "emacs-lisp"
              hermes-kanban-task--status "ready")
        (hermes-kanban-change-assignee)
        (should (member '("PATCH" "/tasks/t1" ((assignee . "elisp-dev"))
                          ((board . "emacs-lisp")))
                        calls))
        (should reverted)))))

(ert-deftest hermes-kanban-change-assignee-reassigns-running-task-detail ()
  "Changing assignee for a running task detail uses reclaiming reassign."
  (let (calls reverted)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "elisp-dev"))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (with-temp-buffer
        (hermes-kanban-task-mode)
        (setq hermes-kanban-task--task-id "t1"
              hermes-kanban-task--board-slug "emacs-lisp"
              hermes-kanban-task--status "running")
        (hermes-kanban-change-assignee)
        (should (member '("POST" "/tasks/t1/reassign"
                          ((profile . "elisp-dev") (reclaim_first . t))
                          ((board . "emacs-lisp")))
                        calls))
        (should reverted)))))

(ert-deftest hermes-kanban-create-triage-task-posts-triage-body ()
  "Creating a triage idea sends Markdown and uses automatic routing."
  (let (call refreshed reported)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "Rough idea"))
              ((symbol-function 'read-string-from-buffer)
               (lambda (&rest _) "## Context\n\nImprove creation."))
              ((symbol-function 'completing-read)
               (lambda (&rest _)
                 (ert-fail "Triage should not ask for an assignee")))
              ((symbol-function 'read-number)
               (lambda (&rest _) 3))
              ((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (setq call (list method path body query))
                 (hermes--promise-resolved '((task . ((id . "t1")))))))
              ((symbol-function 'hermes-kanban--render-board)
               (lambda (slug name &optional _in-place)
                 (setq refreshed (list slug name))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq reported (apply #'format format-string args)))))
      (with-temp-buffer
        (hermes-kanban-mode)
        (setq hermes-kanban--slug "main"
              hermes-kanban--name "Main"
              hermes-kanban--assignees '("specifier")
              hermes-kanban--orchestration-mode 'auto)
        (hermes-kanban-create-triage-task)
        (should (equal call
                       '("POST" "/tasks"
                         ((title . "Rough idea") (priority . 3)
                          (body . "## Context\n\nImprove creation.")
                          (triage . t))
                         ((board . "main")))))
        (should (equal refreshed '("main" "Main")))
        (should (equal
                 reported
                 "Created triage task t1; queued for automatic decomposition"))))))

(ert-deftest hermes-kanban-create-task-keeps-optional-assignee ()
  "Creating a normal task sends its description and chosen assignee."
  (let (call)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "Implement task"))
              ((symbol-function 'read-string-from-buffer)
               (lambda (&rest _) "Detailed acceptance criteria."))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "coder"))
              ((symbol-function 'read-number)
               (lambda (&rest _) 1))
              ((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (setq call (list method path body query))
                 (hermes--promise-resolved '((task . ((id . "t2")))))))
              ((symbol-function 'hermes-kanban--render-board) #'ignore)
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-kanban-mode)
        (setq hermes-kanban--slug "main"
              hermes-kanban--name "Main"
              hermes-kanban--assignees '("coder"))
        (hermes-kanban-create-task)
        (should (equal call
                       '("POST" "/tasks"
                         ((title . "Implement task") (priority . 1)
                          (body . "Detailed acceptance criteria.")
                          (assignee . "coder"))
                         ((board . "main")))))))))

(ert-deftest hermes-kanban-create-task-does-not-reopen-board-during-switch ()
  "A late task creation result does not supersede an in-flight board switch."
  (let ((created (hermes--promise-make))
        (new-board (hermes--promise-make)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "Implement task"))
              ((symbol-function 'read-string-from-buffer)
               (lambda (&rest _) "Description"))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "coder"))
              ((symbol-function 'read-number)
               (lambda (&rest _) 1))
              ((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional _body query)
                 (cond
                  ((equal method "POST") created)
                  ((equal path "/orchestration")
                   (hermes--promise-resolved
                    '((auto_decompose . :json-false))))
                  ((equal (cdr (assq 'board query)) "new") new-board)
                  (t
                   (hermes--promise-resolved
                    '((columns . (((name . "todo") (tasks . []))))
                      (assignees)))))))
              ((symbol-function 'message) #'ignore))
      (unwind-protect
          (with-current-buffer
            (hermes-buffer--get "*Hermes Kanban*" #'hermes-kanban-mode)
            (setq hermes-kanban--slug "old"
                  hermes-kanban--name "Old")
            (hermes-kanban-create-task)
            (hermes-kanban--render-board "new" "New")
            (hermes--promise-resolve
             created '((task . ((id . "t1")))))
            (hermes--promise-resolve
             new-board
             '((columns . (((name . "todo")
                            (tasks . (((id . "new-task") (status . "todo")
                                       (title . "New")))))))
               (assignees)))
            (should (equal hermes-kanban--slug "new"))
            (should (equal (caar tabulated-list-entries) "new-task")))
        (when (get-buffer "*Hermes Kanban*")
          (kill-buffer "*Hermes Kanban*"))))))

(ert-deftest hermes-kanban-create-task-body-omits-empty-description ()
  "Task creation omits blank optional description and assignee fields."
  (should (equal (hermes-kanban--create-task-body
                  "Title" " \n" 0 "" t)
                 '((title . "Title") (priority . 0) (triage . t)))))

(ert-deftest hermes-kanban-triage-mode-indicator-distinguishes-manual-mode ()
  "Manual orchestration is visible and visually distinct in the mode line."
  (with-temp-buffer
    (setq-local hermes-kanban--orchestration-mode 'manual)
    (let ((indicator (hermes-kanban--triage-mode-indicator)))
      (should (string-match-p "Triage: manual" indicator))
      (should (eq (get-text-property 1 'face indicator) 'warning)))))

(ert-deftest hermes-kanban-specify-triage-task-posts-and-refreshes-detail ()
  "Specifying a triage task reports its new title and refreshes its detail."
  (let (call reported reverted)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query timeout)
                 (setq call (list method path body query timeout))
                 (hermes--promise-resolved
                  '((ok . t) (task_id . "t1") (new_title . "Clear task")))))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq reported (apply #'format format-string args)))))
      (with-temp-buffer
        (hermes-kanban-task-mode)
        (setq hermes-kanban-task--task-id "t1"
              hermes-kanban-task--board-slug "main"
              hermes-kanban-task--status "triage")
        (hermes-kanban-specify-triage-task)
        (should (equal call
                       '("POST" "/tasks/t1/specify" ((author . :null))
                         ((board . "main")) 300)))
        (should (equal reported "Specified task: Clear task"))
        (should reverted)))))

(ert-deftest hermes-kanban-specify-summary-reports-resolved-failure ()
  "A resolved non-OK specifier outcome keeps its backend reason visible."
  (should (equal (hermes-kanban--specify-summary
                  '((ok . :false) (reason . "no auxiliary client configured")))
                 "Specify failed: no auxiliary client configured")))

(ert-deftest hermes-kanban-triage-action-rejects-non-triage-task ()
  "Triage-only actions do not call the backend for another task status."
  (let (called)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (&rest _)
                 (setq called t)
                 (hermes--promise-resolved '((ok . t)))))
              ((symbol-function 'revert-buffer) #'ignore)
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (hermes-kanban-task-mode)
        (setq hermes-kanban-task--task-id "t1"
              hermes-kanban-task--board-slug "main"
              hermes-kanban-task--status "todo")
        (should-error (hermes-kanban-specify-triage-task)
                      :type 'user-error)
        (should-not called)))))

(ert-deftest hermes-kanban-triage-actions-are-discoverable-in-keymaps ()
  "Board and task popups expose their applicable triage commands."
  (should (eq (lookup-key hermes-kanban-mode-map (kbd "i"))
              #'hermes-kanban-create-triage-task))
  (dolist (map (list hermes-kanban-mode-map hermes-kanban-task-mode-map))
    (should (eq (lookup-key map (kbd "S"))
                #'hermes-kanban-specify-triage-task))
    (should (eq (lookup-key map (kbd "x"))
                #'hermes-kanban-decompose-triage-task))))

(ert-deftest hermes-kanban-decompose-triage-task-reports-child-ids ()
  "Decomposing a triage task reports the generated child ids."
  (let (call reported reverted)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query timeout)
                 (setq call (list method path body query timeout))
                 (hermes--promise-resolved
                  '((ok . t) (fanout . t) (child_ids . ["c1" "c2"])))))
              ((symbol-function 'revert-buffer)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq reported (apply #'format format-string args)))))
      (with-temp-buffer
        (hermes-kanban-task-mode)
        (setq hermes-kanban-task--task-id "t1"
              hermes-kanban-task--board-slug "main"
              hermes-kanban-task--status "triage")
        (hermes-kanban-decompose-triage-task)
        (should (equal call
                       '("POST" "/tasks/t1/decompose" ((author . :null))
                         ((board . "main")) 300)))
        (should (equal reported "Decomposed task into 2 children: c1, c2"))
        (should reverted)))))

(ert-deftest hermes-kanban-decompose-summary-reports-resolved-failure ()
  "A resolved non-OK decomposer outcome keeps its backend reason visible."
  (should (equal (hermes-kanban--decompose-summary
                  '((ok . :false) (reason . "decomposer unavailable")))
                 "Decompose failed: decomposer unavailable")))

(ert-deftest hermes-kanban-decompose-summary-reports-single-task-title ()
  "A non-fanout decomposition reports the rewritten task title."
  (should (equal (hermes-kanban--decompose-summary
                  '((ok . t) (fanout . :false) (child_ids . [])
                    (new_title . "Clear task")))
                 "Kept as one task: Clear task")))

(ert-deftest hermes-kanban-dispatch-summary-formats-counts ()
  "The dispatch summary reports non-zero counters and auto-assignments."
  (should (equal (hermes-kanban--dispatch-summary
                  '((reclaimed . 1) (promoted . 2)
                    (spawned . [["t1" "dev" "/ws"] ["t2" "dev" "/ws"]])
                    (auto_assigned_default . ["t2"])
                    (skipped_unassigned . ["t3"])
                    (skipped_nonspawnable . ["t4"])))
                 "Dispatcher: 2 spawned (1 auto-assigned), 2 promoted, 1 reclaimed, 1 skipped unassigned"))
  (should (equal (hermes-kanban--dispatch-summary
                  '((reclaimed . 0) (promoted . 0) (spawned . [])
                    (skipped_nonspawnable . ["t4"])))
                 "Dispatcher: nothing ready to dispatch")))

(ert-deftest hermes-kanban-nudge-dispatch-posts-and-reports ()
  "The nudge command POSTs /dispatch with the board query and echoes counts."
  (let (seen-method seen-path seen-query reported)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional _body query)
                 (setq seen-method method
                       seen-path path
                       seen-query query)
                 (hermes--promise-resolved
                  '((spawned . [["t1" "dev" "/ws"]]) (promoted . 0)
                    (reclaimed . 0)))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) reported)))
              ((symbol-function 'hermes-kanban--board-slug-for-command)
               (lambda () "main"))
              ((symbol-function 'revert-buffer) (lambda (&rest _))))
      (hermes-kanban-nudge-dispatch)
      (should (equal seen-method "POST"))
      (should (equal seen-path "/dispatch"))
      (should (equal (cdr (assq 'board seen-query)) "main"))
      (should (cl-some (lambda (m) (string-match-p "1 spawned" m))
                       reported)))))

(ert-deftest hermes-kanban-events-replay-notifies-only-unseen-ids ()
  "Overlapping, repeated and older batches never notify accepted IDs twice."
  (let ((tail (hermes-kanban--events-tail-create :buffer (current-buffer)))
        notices (refreshes 0))
    (cl-letf (((symbol-function 'hermes-kanban--notify-events)
               (lambda (_tail events)
                 (setq notices (append notices (mapcar (lambda (event)
                                                       (alist-get 'id event)) events)))))
              ((symbol-function 'hermes-kanban--events-schedule-refresh)
               (lambda (_) (cl-incf refreshes))))
      (dolist (text '("{\"events\":[{\"id\":1},{\"id\":2}],\"cursor\":2}"
                      "{\"events\":[{\"id\":1},{\"id\":2}],\"cursor\":2}"
                      "{\"events\":[{\"id\":2},{\"id\":3}],\"cursor\":3}"
                      "{\"events\":[{\"id\":1}],\"cursor\":1}"))
        (hermes-kanban--events-handle-frame tail text))
      (should (equal notices '(1 2 3)))
      (should (= refreshes 2))
      (should (= (hermes-kanban--events-tail-cursor tail) 3)))))

(ert-deftest hermes-kanban-events-invalid-frames-never-invent-cursor ()
  "Invalid frames reconnect, then stop; recovery still delivers unseen events."
  (let ((tail (hermes-kanban--events-tail-create :buffer (current-buffer) :cursor 7))
        reconnected accepted)
    (cl-letf (((symbol-function 'hermes-kanban--events-reconnect)
               (lambda (owner) (push (hermes-kanban--events-tail-cursor owner) reconnected)))
              ((symbol-function 'hermes-kanban--notify-events)
               (lambda (_ events) (setq accepted events)))
              ((symbol-function 'hermes-kanban--events-schedule-refresh) #'ignore))
      (hermes-kanban--events-handle-frame tail "not json")
      (hermes-kanban--events-handle-frame tail "{\"events\":[{\"id\":8}]}")
      (should (equal reconnected '(7 7)))
      (should (= (hermes-kanban--events-tail-cursor tail) 7))
      (hermes-kanban--events-handle-frame tail "{\"events\":[{\"id\":8}],\"cursor\":8}")
      (should (equal accepted '(((id . 8)))))
      (should (= (hermes-kanban--events-tail-parse-failures tail) 0))
      (dolist (text '("{\"events\":42,\"cursor\":99}"
                      "{\"events\":[{\"id\":9}],\"cursor\":99}"
                      "{\"events\":[{\"id\":10},{\"id\":9}],\"cursor\":10}"))
        (hermes-kanban--events-handle-frame tail text))
      (should (= (hermes-kanban--events-tail-cursor tail) 8))
      (should-not (hermes-kanban--events-tail-active tail))
      (should (equal reconnected '(8 8 7 7)))
      (hermes-kanban--events-handle-frame tail "{\"events\":[{\"id\":9}],\"cursor\":9}")
      (should (= (hermes-kanban--events-tail-cursor tail) 8)))))

(ert-deftest hermes-kanban-events-cursorless-replay-does-not-duplicate-notice ()
  "Reject cursor-less events before notifying, then accept the recovered batch."
  (let ((tail (hermes-kanban--events-tail-create :buffer (current-buffer)))
        notices)
    (cl-letf (((symbol-function 'hermes-kanban--events-reconnect) #'ignore)
              ((symbol-function 'hermes-kanban--events-schedule-refresh) #'ignore)
              ((symbol-function 'hermes-kanban--notify-event)
               (lambda (_tail notice) (push (plist-get notice :task-id) notices))))
      (hermes-kanban--events-handle-frame
       tail "{\"events\":[{\"id\":1,\"kind\":\"blocked\",\"task_id\":\"task\"}]}")
      (hermes-kanban--events-handle-frame
       tail "{\"events\":[{\"id\":1,\"kind\":\"blocked\",\"task_id\":\"task\"}],\"cursor\":1}")
      (should (equal notices '("task")))
      (should (= (hermes-kanban--events-tail-cursor tail) 1)))))

(ert-deftest hermes-kanban-events-stopped-tail-survives-board-readback ()
  "A delayed board readback cannot restart a stream stopped for invalid frames."
  (with-temp-buffer
    (hermes-kanban-mode)
    (setq hermes-instance '("test" . "http://test.invalid")
          hermes-kanban--slug "work"
          hermes-kanban--events-tail
          (hermes-kanban--events-tail-create
           :buffer (current-buffer) :slug "work" :instance hermes-instance
           :cursor 7 :parse-failures hermes-kanban--events-parse-failure-limit
           :active nil))
    (let ((tail hermes-kanban--events-tail) connected)
      (cl-letf (((symbol-function 'hermes-kanban--events-connect)
                 (lambda (_) (setq connected t))))
        (hermes-kanban--events-retarget "work" 99)
        (should (eq tail hermes-kanban--events-tail))
        (should-not connected)
        (should (= (hermes-kanban--events-tail-cursor tail) 7))
        (should (string-match-p "stopped" (hermes-kanban--live-indicator)))))))

(provide 'hermes-kanban-tests)
;;; hermes-kanban-tests.el ends here
