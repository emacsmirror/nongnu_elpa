;;; hermes-test-helpers.el --- Shared test helpers for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared setup and helper utilities for the hermes-el test suite.

;;; Code:

(require 'ert)

(require 'button)

(require 'cl-lib)

(require 'ewoc)

(require 'subr-x)

(require 'timer)

(require 'auth-source)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" root)))

(eval-and-compile
  (unless (fboundp 'keymap-set)
    (defun keymap-set (keymap key definition)
      (define-key keymap (kbd key) definition)))
  (unless (fboundp 'keymap-lookup)
    (defun keymap-lookup (keymap key &optional accept-default)
      (lookup-key keymap (kbd key) accept-default)))
  (unless (fboundp 'defvar-keymap)
    (defmacro defvar-keymap (name &rest args)
      (declare (indent 1))
      (let (doc parent bindings)
        (while (keywordp (car args))
          (pcase (pop args)
            (:doc (setq doc (pop args)))
            (:parent (setq parent (pop args)))
            (_ (pop args))))
        (while args
          (let ((key (pop args))
                (definition (pop args)))
            (push `(define-key map (kbd ,key) ,definition) bindings)))
        `(defvar ,name
           (let ((map (make-sparse-keymap)))
             ,@(when parent `((set-keymap-parent map ,parent)))
             ,@(nreverse bindings)
             map)
           ,doc)))))

(require 'keymap-popup)

(require 'hermes)

(require 'hermes-chat)

(require 'hermes-transport)
(require 'hermes-transport-cli)

(require 'hermes-dashboard-transport)

(require 'hermes-mcp)

(defun hermes-test--chat-buffer-name ()
  "Return a fresh chat buffer name for tests."
  (generate-new-buffer-name "*Hermes Chat Test*"))

(defun hermes-test--dashboard-buffer-name ()
  "Return a fresh dashboard buffer name for tests."
  (generate-new-buffer-name "*Hermes Dashboard Test*"))

(defmacro hermes-test-with-chat-buffer (&rest body)
  "Create a fresh Hermes chat buffer and run BODY in it.
The buffer is captured by object so teardown still kills it after a rename."
  (declare (indent 0) (debug t))
  `(let* ((hermes-dashboard-transport--model-options-cache nil)
          (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
          (hermes-chat-buffer-name (hermes-test--chat-buffer-name))
          (buffer (hermes-chat)))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(defmacro hermes-test-with-dashboard-buffer (&rest body)
  "Create a fresh Hermes dashboard buffer and run BODY in it."
  (declare (indent 0) (debug t))
  `(let ((hermes-dashboard-buffer-name (hermes-test--dashboard-buffer-name)))
     (unwind-protect
         (with-current-buffer (get-buffer-create hermes-dashboard-buffer-name)
           (hermes-dashboard-mode)
           (hermes-dashboard--render)
           ,@body)
       (when-let* ((buffer (get-buffer hermes-dashboard-buffer-name)))
         (kill-buffer buffer)))))

(defun hermes-test--dashboard-node-data (id)
  "Return dashboard node data for ID in the current dashboard buffer."
  (when-let* ((node (gethash id hermes-dashboard--nodes)))
    (ewoc-data node)))

(defun hermes-test--dashboard-stale-refresh-timers (&optional buffer)
  "Return dashboard stale-refresh timers, optionally for BUFFER."
  (let (timers)
    (dolist (timer timer-list (nreverse timers))
      (when (and (timerp timer)
                 (eq (timer--function timer)
                     #'hermes-dashboard--stale-refresh)
                 (or (null buffer)
                     (equal (timer--args timer) (list buffer))))
        (push timer timers)))))

(defun hermes-test--face-includes-p (value face)
  "Return non-nil if text face VALUE includes FACE."
  (or (eq value face)
      (and (listp value) (memq face value))))

(defun hermes-test--face-at-end-of (needle)
  "Return the face on the final character of NEEDLE in the current buffer."
  (goto-char (point-min))
  (search-forward needle)
  (get-text-property (1- (point)) 'face))

(defun hermes-test--should-have-face (needle face)
  "Assert that NEEDLE has FACE on its final character."
  (should (hermes-test--face-includes-p
           (hermes-test--face-at-end-of needle) face)))

(defun hermes-test--push-button-labeled (label)
  "Activate the text button ending at LABEL in the current buffer."
  (goto-char (point-min))
  (search-forward label)
  (let ((button (button-at (1- (point)))))
    (should button)
    (button-activate button)))

(defun hermes-test--header-line-string ()
  "Return the current chat header line as plain text."
  (substring-no-properties (hermes-chat--header-line)))

(defun hermes-test--count-buttons-labeled (label)
  "Return the number of buttons whose text is LABEL in the current buffer."
  (let ((count 0)
        (search (concat "[" label "]")))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward search nil t)
        (when (button-at (1- (point)))
          (setq count (1+ count)))))
    count))

(defun hermes-test--view-diff-content ()
  "Push the first View Diff link and return the diff buffer text."
  (hermes-test--push-button-labeled "View Diff")
  (with-current-buffer "*Hermes Diff*"
    (buffer-substring-no-properties (point-min) (point-max))))

(defun hermes-test--assistant-entry ()
  "Return the chat entry whose role is `assistant' (the agent reply)."
  (cl-find-if (lambda (entry) (eq (plist-get entry :role) 'assistant))
              (hermes-chat--entries)))

(defun hermes-test--last-assistant-entry ()
  "Return the last chat entry whose role is `assistant'."
  (car (last (cl-remove-if-not
              (lambda (entry) (eq (plist-get entry :role) 'assistant))
              (hermes-chat--entries)))))

(defun hermes-test--queued-contents ()
  "Return queued chat message contents in send order."
  (mapcar (lambda (entry) (plist-get entry :content))
          hermes-chat--queued-messages))

(defun hermes-test--dashboard-client ()
  "Return a fake dashboard transport client for chat integration tests."
  (make-hermes-dashboard-transport-client
   :websocket 'fake-websocket
   :pending (make-hash-table :test #'equal)
   :callback #'ignore))

(defun hermes-test--control-content-preserved-p (&rest candidates)
  "Return non-nil when a busy-control CANDIDATE is still recoverable."
  (or (cl-some (lambda (content) (member content candidates))
               (hermes-test--queued-contents))
      (member (hermes-chat-input-string) candidates)))

(defmacro hermes-test-with-dashboard-prompt-session (spec &rest body)
  "Create a chat using fake dashboard SPEC's client, then run BODY."
  (declare (indent 1) (debug t))
  (let ((client (car spec)))
    `(let ((,client (hermes-test--dashboard-client)))
     (cl-letf (((symbol-function 'hermes-transport-send)
                (lambda (&rest _args) (error "CLI fallback should not run")))
               ((symbol-function 'hermes-dashboard-transport-start)
                (lambda (&rest args)
                  (setf (hermes-dashboard-transport-client-callback ,client)
                        (plist-get args :callback))
                  ,client))
               ((symbol-function 'hermes-dashboard-transport-session-create)
                (lambda (_client &rest args)
                  (funcall (plist-get args :resolve)
                           '((session_id . "sid-prompt")
                             (stored_session_id . "sid-stored")))))
               ((symbol-function 'hermes-dashboard-transport-command-dispatch)
                (lambda (_client name arg &rest args)
                  (unless (equal (cons name arg) '("goal" . "status"))
                    (error "Unexpected command dispatch: %s %s" name arg))
                  (funcall (plist-get args :resolve)
                           '((type . "exec") (output . "No active goal.")))))
               ((symbol-function 'hermes-dashboard-transport-prompt-submit)
                (lambda (&rest _args) 'prompt-submitted)))
       (let ((hermes-transport-send-function #'hermes-transport-send))
         (hermes-test-with-chat-buffer
          (insert "trigger prompt")
          (hermes-chat-send)
          ,@body))))))

(defun hermes-test--emit-dashboard-prompt (client type payload)
  "Emit dashboard prompt event TYPE with PAYLOAD through CLIENT."
  (hermes-dashboard-transport--handle-frame
   client
   (hermes-dashboard-transport--encode-frame
    `((jsonrpc . "2.0")
      (method . "event")
      (params . ((type . ,type)
                 (session_id . "sid-prompt")
                 (payload . ,payload)))))))

(defun hermes-test--emit-dashboard-event (client type payload)
  "Emit dashboard event TYPE with PAYLOAD through CLIENT."
  (hermes-dashboard-transport--handle-frame
   client
   (hermes-dashboard-transport--encode-frame
    `((jsonrpc . "2.0")
      (method . "event")
      (params . ((type . ,type)
                 (session_id . "sid-active")
                 (payload . ,payload)))))))

(defun hermes-test--emit-dashboard-idle (client &optional session-id)
  "Emit authoritative idle state for CLIENT and optional SESSION-ID."
  (hermes-dashboard-transport--dispatch-event
   client
   (list :type 'status
         :event "session.info"
         :status "ready"
         :running nil
         :session-id (or session-id "sid-active"))))

(defun hermes-test--dashboard-events (&rest frames)
  "Return events emitted by handling each event FRAMES alist on a fresh client.
Each entry of FRAMES is a (TYPE . PAYLOAD-ALIST) cons turned into a JSON-RPC
event frame."
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (dolist (frame frames)
        (hermes-dashboard-transport--handle-frame
         client (hermes-dashboard-transport--encode-frame
                 `((jsonrpc . "2.0")
                   (method . "event")
                   (params . ((type . ,(car frame))
                              (session_id . "sid")
                              (payload . ,(cdr frame)))))))))
    (nreverse events)))

(defmacro hermes-test-with-cron-buffer (entries &rest body)
  "Create a cron buffer with ENTRIES and run BODY on its first row."
  (declare (indent 1) (debug t))
  `(unwind-protect
       (with-current-buffer (get-buffer-create "*Hermes Cron*")
         (hermes-cron-mode)
         (setq tabulated-list-entries ,entries)
         (tabulated-list-print t)
         (goto-char (point-min))
         (search-forward "nightly" nil t)
         (beginning-of-line)
         ,@body)
     (when (get-buffer "*Hermes Cron*")
       (kill-buffer "*Hermes Cron*"))))

(defun hermes-test--cron-entry (&optional state)
  "Return one rich cron tabulated-list entry with optional STATE."
  (list "j1" (vector "nightly" "0 0 * * *" (or state "scheduled")
                     "work" "telegram" "2026-01-01" "2026-01-02"
                     "do it")))

(defun hermes-test--tabulated-list-format-total-width (format)
  "Return FORMAT's display width including inter-column padding."
  (let ((total (max 0 (1- (length format)))))
    (dotimes (i (length format) total)
      (setq total (+ total (cadr (aref format i)))))))

(provide 'hermes-test-helpers)
;;; hermes-test-helpers.el ends here
