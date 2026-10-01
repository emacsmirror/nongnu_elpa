;;; hermes-chat-attachments.el --- Bounded gateway workspace attachments -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Explicitly write bounded local UTF-8 text/source into the gateway-host cwd,
;; verify bytes via managed Files, then ask the owning session for file.attach's
;; reference.  This is not terminal-backend filesystem equivalence, immutable
;; context or an atomic create: overwrite:false has a backend precheck race.
;; No prompt is sent, uploads are never replayed, and remote files are not
;; rolled back.  Keep literal bytes/draft/reference in native recovery views.
;; PDF extraction and arbitrary binaries are deliberately unsupported.

;;; Code:

(require 'hermes-chat-buffer)
(require 'hermes-dashboard-rpc)
(require 'hermes-buffer)
(require 'keymap-popup)
(require 'subr-x)

;; Files depends on the assembled chat; load it only when creating recovery.
(declare-function hermes-file-view-mode "hermes-files" ())
(declare-function hermes-file-save "hermes-files" (&optional filename))
(declare-function hermes-files-quit "hermes-files" ())
(defvar hermes-files--bytes)
(defvar hermes-files--status)

(defconst hermes-chat-attachments--max-bytes (* 2 1024 1024)
  "Hard local read and readback cap; HTTP retrieval is not streaming-bounded.")
(defconst hermes-chat-attachments--extensions
  '("txt" "md" "org" "el" "py" "c" "h" "cpp" "rs" "go" "js" "ts"
    "java" "kt" "rb" "sh" "json" "yaml" "yml" "toml" "csv" "xml" "html" "css")
  "Supported UTF-8 text/source suffixes; this is not a binary extractor.")
(defvar-local hermes-chat-attachments--operation nil "Exact pending operation.")
(defvar-local hermes-chat-attachments--recoveries nil "Retained recovery buffers.")
(put 'hermes-chat-attachments--recoveries 'permanent-local t)
(defvar-local hermes-chat-attachments--record nil "Literal recovery record.")
(put 'hermes-chat-attachments--record 'permanent-local t)

(defun hermes-chat-attachments--bytes-label (op)
  "Describe OP's retained byte completeness, not recovery of unread data."
  (if (plist-get op :prefix)
      (format "INCOMPLETE bounded prefix (%d bytes; rest not read)"
              (length (plist-get op :bytes)))
    "complete bytes retained"))

(keymap-popup-define hermes-chat-attachment-recovery-map
  "Keys for retained attachment bytes and literal draft."
  :parent special-mode-map
  :exit-key "<escape>"
  :group "Recovery"
  "s" ("Save NEW local bytes" hermes-chat-attachment-save)
  "q" ("Quit view" hermes-files-quit)
  "<escape>" ("Dismiss menu" keymap-popup-dismiss)
  "?" ("Help" hermes-chat-attachment-recovery-map-popup))

(put 'hermes-chat-attachment-recovery-map-popup 'command-modes '(hermes-file-view-mode))

(defun hermes-chat-attachments--ready-p ()
  "Return non-nil for a claimed, ready, attached dashboard chat."
  (let ((client hermes-chat--dashboard-client))
    (and (hermes-buffer--owned-p 'hermes-chat-mode)
         (hermes-dashboard-transport-client-p client)
         (hermes-dashboard-transport-client-ready-p client)
         (not (hermes-dashboard-transport-client-stopping-p client))
         (not (hermes-dashboard-transport-client-reconnecting-p client))
         hermes-chat--dashboard-session-ready-p
         (stringp hermes-chat--dashboard-active-session-id))))

(defun hermes-chat-attachments--destination ()
  "Return routing, session, cwd and connection evidence for this chat."
  (mapcar (lambda (value) (if (stringp value) (copy-sequence value) value))
          (list hermes-chat--lifecycle-generation hermes-chat--transport-generation
                (hermes-dashboard-transport-client-generation hermes-chat--dashboard-client)
                (hermes-dashboard-transport--api-client-base-url hermes-chat--dashboard-client)
                (hermes-instance-id hermes-instance) (hermes-instance-url hermes-instance)
                hermes-chat--profile hermes-chat--session-id
                hermes-chat--dashboard-active-session-id hermes-chat--working-directory)))

(defun hermes-chat-attachments--current-p (op)
  "Return non-nil while OP owns the captured chat and connection."
  (and (not (plist-get op :retired)) (buffer-live-p (plist-get op :buffer))
       (with-current-buffer (plist-get op :buffer)
         (and (eq op hermes-chat-attachments--operation)
              (hermes-chat-attachments--ready-p)
              (eq hermes-buffer--owner (plist-get op :claim))
              (eq hermes-chat--lifecycle-generation (plist-get op :lifetime))
              (eq hermes-chat--dashboard-client (plist-get op :client))
              (eq hermes-chat--input-marker (plist-get op :marker))
              (equal (hermes-chat-attachments--destination) (plist-get op :destination))))))

(defun hermes-chat-attachments--edited (beg end)
  "Mark the pending draft edited for composer text between BEG and END."
  (when (and hermes-chat-attachments--operation
             (marker-position hermes-chat--input-marker)
             (or (>= beg hermes-chat--input-marker) (> end hermes-chat--input-marker)))
    (setf (plist-get hermes-chat-attachments--operation :edited) t)))

(defun hermes-chat-attachments--finish (op state)
  "Retire OP locally with STATE, retaining bytes and draft for manual recovery."
  (unless (plist-get op :retired)
    (setf (plist-get op :retired) t (plist-get op :state) state)
    (when (buffer-live-p (plist-get op :buffer))
      (with-current-buffer (plist-get op :buffer)
        (when (eq op hermes-chat-attachments--operation)
          (setq hermes-chat-attachments--operation nil)
          (remove-hook 'before-change-functions #'hermes-chat-attachments--edited t)
          (dolist (hook '(kill-buffer-hook change-major-mode-hook
                         after-set-visited-file-name-hook hermes-chat-lifecycle-invalidation-hook))
            (remove-hook hook #'hermes-chat-attachment-cancel t)))))
    (hermes-dashboard-transport-cancel-owner-requests (plist-get op :client) op)
    (when (plist-get op :subscription)
      (hermes-dashboard-transport-unsubscribe (plist-get op :client) (plist-get op :subscription)))
    (when (timerp (plist-get op :timer)) (cancel-timer (plist-get op :timer))))
  (let ((view (plist-get op :recovery)))
    (when (and (buffer-live-p view)
               (with-current-buffer view
                 (and (eq (plist-get op :recovery-claim) hermes-buffer--owner)
                      (hermes-buffer--owned-p 'hermes-file-view-mode))))
      (with-current-buffer view
        (setq header-line-format
              (format "Attachment: %s%s | %s | s: save NEW local file | copy manually"
                      state (if (plist-get op :written) " (remote write may remain)" "")
                      (hermes-chat-attachments--bytes-label op)))))))

(defun hermes-chat-attachment-cancel ()
  "Cancel local attachment work, preserving recovery; never undo remote writes."
  (interactive nil hermes-chat-mode)
  (when hermes-chat-attachments--operation
    (hermes-chat-attachments--finish hermes-chat-attachments--operation 'cancelled)))

(defun hermes-chat-attachments--check (op)
  "Require OP's ownership or retire it without dispatching successor work."
  (unless (hermes-chat-attachments--current-p op)
    (hermes-chat-attachments--finish op 'owner-changed)
    (user-error "Attachment owner changed; no automatic retry")))

(defun hermes-chat-attachments--path-p (path)
  "Return non-nil for a bounded lexical absolute POSIX gateway PATH."
  (and (stringp path) (<= (length path) 4096) (string-prefix-p "/" path)
       (not (string-prefix-p "//" path))
       (not (string-match-p "[\0-\37\177\\\\]" path))
       (not (seq-some (lambda (part) (member part '("." ".."))) (split-string path "/" t)))))

(defun hermes-chat-attachments--begin ()
  "Capture attachment ownership before any recursive readers."
  (unless (hermes-chat-attachments--ready-p)
    (user-error "Connect and attach this dashboard chat before choosing a file"))
  (when hermes-chat-attachments--operation (user-error "Cancel the pending attachment first"))
  (when (>= (length (seq-filter #'buffer-live-p hermes-chat-attachments--recoveries)) 8)
    (user-error "Eight attachment recoveries retained; save and close unneeded views"))
  (let ((op (list :buffer (current-buffer) :claim hermes-buffer--owner
                  :lifetime hermes-chat--lifecycle-generation
                  :client hermes-chat--dashboard-client :destination (hermes-chat-attachments--destination)
                  :session (copy-sequence hermes-chat--dashboard-active-session-id)
                  :marker hermes-chat--input-marker :edited nil
                  :draft (save-restriction (widen) (hermes-chat-input-string))
                  :state 'choosing :retired nil :bytes nil :prefix nil :extension nil :path nil
                  :cwd nil :ref nil :written nil :recovery nil :recovery-claim nil
                  :subscription nil :timer nil)))
    (setq hermes-chat-attachments--operation op)
    (add-hook 'before-change-functions #'hermes-chat-attachments--edited nil t)
    (dolist (hook '(kill-buffer-hook change-major-mode-hook after-set-visited-file-name-hook
                   hermes-chat-lifecycle-invalidation-hook))
      (add-hook hook #'hermes-chat-attachment-cancel nil t))
    (setf (plist-get op :subscription)
          (hermes-dashboard-transport-subscribe
           (plist-get op :client) nil (lambda () (hermes-chat-attachments--finish op 'disconnected)))
          (plist-get op :timer)
          (run-at-time 120 nil (lambda () (hermes-chat-attachments--finish op 'timeout))))
    op))

(defun hermes-chat-attachments--read (op file)
  "Read bounded literal local FILE bytes under OP; retain them before validation."
  (hermes-chat-attachments--check op)
  (unless (and (stringp file) (not (file-remote-p file))
               (not (find-file-name-handler file 'insert-file-contents-literally)))
    (user-error "Choose an ordinary local file without filename handlers"))
  (let ((bytes (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally file nil 0 (1+ hermes-chat-attachments--max-bytes))
                 (buffer-string)))
        (extension (downcase (or (file-name-extension file) ""))))
    (hermes-chat-attachments--check op)
    (setf (plist-get op :bytes) bytes (plist-get op :extension) extension
          (plist-get op :prefix) (> (length bytes) hermes-chat-attachments--max-bytes))
    (hermes-chat-attachments--recover op)
    (unless (and (< 0 (length bytes)) (<= (length bytes) hermes-chat-attachments--max-bytes))
      (user-error "Attachment requires 1 byte through 2 MiB; %s"
                  (hermes-chat-attachments--bytes-label op)))
    (let ((text (decode-coding-string bytes 'utf-8-unix)))
      (unless (and (member extension hermes-chat-attachments--extensions)
                   (not (string-prefix-p "%PDF-" bytes))
                   ;; Emacs preserves invalid UTF-8 as raw-byte characters;
                   ;; a decode/encode roundtrip alone does not validate UTF-8.
                   (not (seq-some (lambda (char) (> char #x10ffff)) text))
                   (not (string-match-p "[\0-\10\16-\37\177-\237]" text))
                   (equal bytes (encode-coding-string text 'utf-8-unix)))
        (user-error "Only UTF-8 text/source supported; PDF/binary extraction unavailable")))))

(defun hermes-chat-attachments--recover (op &optional manual)
  "Keep OP's bytes and literal draft in a native inert Files viewer.
MANUAL means reopen retained local data without pending remote authority."
  (require 'hermes-files)
  (let ((view (generate-new-buffer "*Hermes Attachment Recovery*")))
    (setf (plist-get op :recovery) view)
    (with-current-buffer (plist-get op :buffer) (push view hermes-chat-attachments--recoveries))
    (with-current-buffer view
      (setq hermes-chat-attachments--record op)
      ;; Establish our claim before delayed native mode hooks can replace it.
      ;; Never adopt an empty successor viewer's claim after a mode hook.
      (delay-mode-hooks (hermes-file-view-mode))
      (hermes-buffer--claim 'hermes-file-view-mode)
      (setf (plist-get op :recovery-claim) hermes-buffer--owner)
      (let ((delay-mode-hooks nil)) (run-mode-hooks))
      (unless (and (eq (plist-get op :recovery-claim) hermes-buffer--owner)
                   (hermes-buffer--owned-p 'hermes-file-view-mode)
                   (string-empty-p (buffer-string)))
        (user-error "Recovery viewer changed during initialization"))
      (setq hermes-chat-attachments--record op)
      (use-local-map hermes-chat-attachment-recovery-map)
      (setq hermes-files--bytes (copy-sequence (plist-get op :bytes))
            hermes-files--status "Ready"
            header-line-format (concat "Attachment recovery | " (hermes-chat-attachments--bytes-label op)
                                       " | s: save NEW local bytes"))
      (let ((inhibit-read-only t) (tick (buffer-chars-modified-tick))
            (claim hermes-buffer--owner))
        (combine-change-calls (point) (point)
          (unless (and (eq claim hermes-buffer--owner)
                       (hermes-buffer--owned-p 'hermes-file-view-mode)
                       (= tick (buffer-chars-modified-tick)))
            (user-error "Recovery viewer changed before rendering"))
          (insert "Original literal draft (not sent):\n" (plist-get op :draft)
                  "\n\n" (hermes-chat-attachments--bytes-label op) "; save with s.\n"
                  "Uploads are not atomic; remote writes may remain. No automatic replay.\n"
                  (or (plist-get op :ref) "")))))
    (display-buffer view)
    (unless manual (hermes-chat-attachments--check op))))

(defun hermes-chat-attachment-save ()
  "Save retained attachment bytes to a new local file under this view's claim."
  (interactive nil hermes-file-view-mode)
  (unless (and (hermes-buffer--owned-p 'hermes-file-view-mode)
               hermes-chat-attachments--record)
    (user-error "Attachment recovery view is retired"))
  (let* ((view (current-buffer)) (claim hermes-buffer--owner)
         (record hermes-chat-attachments--record)
         (bytes (copy-sequence (plist-get record :bytes)))
         (file (read-file-name
                (concat "Save " (hermes-chat-attachments--bytes-label record) " to NEW local file: ")
                nil nil nil)))
    (unless (and (buffer-live-p view)
                 (with-current-buffer view
                   (and (eq claim hermes-buffer--owner)
                        (hermes-buffer--owned-p 'hermes-file-view-mode)
                        (eq record hermes-chat-attachments--record)
                        (equal bytes (plist-get record :bytes)))))
      (user-error "Attachment recovery changed during filename input"))
    (with-current-buffer view
      (setq hermes-files--bytes bytes)
      (hermes-file-save file)
      (message "Saved %s to %s" (hermes-chat-attachments--bytes-label record) file))))

(defun hermes-chat-attachment-recovery ()
  "Reopen the most recent retained attachment without touching retired notes.
Retained bytes and original draft survive view mode changes in memory only."
  (interactive nil hermes-chat-mode)
  (unless (hermes-buffer--owned-p 'hermes-chat-mode) (user-error "Chat is retired"))
  (when hermes-chat-attachments--operation (user-error "Cancel pending attachment first"))
  (when (>= (length (seq-filter #'buffer-live-p hermes-chat-attachments--recoveries)) 8)
    (user-error "Close an unneeded recovery view before reopening"))
  (let* ((view (seq-find (lambda (buffer)
                          (and (buffer-live-p buffer)
                               (buffer-local-value 'hermes-chat-attachments--record buffer)))
                        hermes-chat-attachments--recoveries))
         (op (and view (buffer-local-value 'hermes-chat-attachments--record view))))
    (unless op (user-error "No retained attachment recovery"))
    (hermes-chat-attachments--recover op t)))

(defun hermes-chat-attachments--rpc (op method params accept)
  "Dispatch METHOD with PARAMS for OP, then pass the exact result to ACCEPT."
  (hermes-chat-attachments--check op)
  (let ((hermes-dashboard-transport-dispatch-guard (lambda () (hermes-chat-attachments--current-p op)))
        (hermes-dashboard-transport-request-owner op))
    (hermes-dashboard-transport-request
     (plist-get op :client) method params
     (lambda (result) (hermes-chat-attachments--accept op accept result))
     (lambda (_) (hermes-chat-attachments--finish op 'failed-or-uncertain)))))

(defun hermes-chat-attachments--http (op method route query body accept)
  "Dispatch OP's owned METHOD to ROUTE with QUERY/BODY and call ACCEPT."
  (hermes-chat-attachments--check op)
  (hermes--promise-catch
   (hermes--promise-then
    (hermes-dashboard-transport-api-request-async
     method route :client (plist-get op :client) :query query :body body :timeout 60
     :secrets (and body (list (alist-get 'data_url body)))
     :current-p (lambda () (hermes-chat-attachments--current-p op)))
    (lambda (result) (hermes-chat-attachments--accept op accept result)))
   (lambda (_) (hermes-chat-attachments--finish op 'failed-or-uncertain))))

(defun hermes-chat-attachments--accept (op accept result)
  "Run ACCEPT for current OP and RESULT, settling errors and quits locally."
  (condition-case nil
      (progn (hermes-chat-attachments--check op) (funcall accept op result))
    ((error quit) (hermes-chat-attachments--finish op 'failed-or-uncertain))))

(defun hermes-chat-attachments--cwd (op result)
  "Validate OP's owning session cwd in RESULT and ask Files for policy admission."
  (let ((cwd (hermes-transport--get (hermes-transport--get result 'info) 'cwd)))
    (unless (and (equal (plist-get op :session) (hermes-transport--get result 'session_id))
                 (hermes-chat-attachments--path-p cwd))
      (error "No exact gateway workspace evidence"))
    (setf (plist-get op :cwd) (copy-sequence cwd))
    (hermes-chat-attachments--http op "GET" "/api/files" `((path . ,cwd)) nil
                                   #'hermes-chat-attachments--consent)))

(defun hermes-chat-attachments--consent (op result)
  "Require RESULT's policy-admitted directory and explicit OP write consent."
  (let* ((cwd (plist-get op :cwd))
         (path (concat (string-remove-suffix "/" cwd) "/hermes-attachment-"
                       (substring (secure-hash 'sha256 (format "%s-%s-%s" (float-time) (random) (gensym))) 0 32)
                       "." (plist-get op :extension))))
    (unless (and (equal cwd (hermes-transport--get result 'path))
                 (hermes-chat-attachments--path-p path))
      (error "Gateway workspace not admitted for managed Files"))
    (setf (plist-get op :path) path)
    (unless (yes-or-no-p
             (format "Attempt writing %d local bytes to GATEWAY-HOST %s at %s; upload checks write permission.  No terminal-backend mapping; overwrite:false is non-atomic, remote file remains and later context may change.  Proceed? "
                     (length (plist-get op :bytes)) (nth 3 (plist-get op :destination)) path))
      (user-error "Attachment cancelled"))
    (hermes-chat-attachments--check op)
    ;; Mark conservative uncertainty before asynchronous authentication/dispatch.
    (setf (plist-get op :written) t (plist-get op :state) 'uploading)
    (hermes-chat-attachments--http
     op "POST" "/api/files/upload" nil
     `((path . ,path) (overwrite . :false)
       (data_url . ,(concat "data:text/plain;base64," (base64-encode-string (plist-get op :bytes) t))))
     #'hermes-chat-attachments--uploaded)))

(defun hermes-chat-attachments--uploaded (op result)
  "Validate OP's upload RESULT before exact byte readback."
  (unless (and (hermes-transport--true-p (hermes-transport--get result 'ok))
               (equal (plist-get op :path) (hermes-transport--get result 'path)))
    (error "Attachment upload path mismatch"))
  (setf (plist-get op :state) 'readback)
  (hermes-chat-attachments--http op "GET" "/api/files/read"
                                 `((path . ,(plist-get op :path))) nil
                                 #'hermes-chat-attachments--readback))

(defun hermes-chat-attachments--readback (op result)
  "Require exact uploaded bytes in RESULT before OP's session-qualified attach."
  (unless (and (equal (plist-get op :path) (hermes-transport--get result 'path))
               (equal (plist-get op :bytes)
                      (hermes-transport-file-bytes result hermes-chat-attachments--max-bytes)))
    (error "Attachment readback mismatch"))
  (setf (plist-get op :state) 'attaching)
  (hermes-chat-attachments--rpc op "file.attach"
                               `((session_id . ,(plist-get op :session)) (path . ,(plist-get op :path)))
                               #'hermes-chat-attachments--attached))

(defun hermes-chat-attachments--attached (op result)
  "Validate OP's supported reference in RESULT; preserve newer draft edits."
  (let* ((path (plist-get op :path))
         (name (substring path (1+ (cl-position ?/ path :from-end t))))
         (ref (hermes-transport--get result 'ref_text)))
    (unless (and (eq t (hermes-transport--get result 'attached))
                 (equal path (hermes-transport--get result 'path))
                 (equal (concat "@file:" name) ref))
      (error "Attachment reference mismatch"))
    (setf (plist-get op :ref) (copy-sequence ref))
    (let ((view (plist-get op :recovery)))
      (when (and (buffer-live-p view)
                 (with-current-buffer view
                   (and (eq hermes-buffer--owner (plist-get op :recovery-claim))
                        (hermes-buffer--owned-p 'hermes-file-view-mode))))
        (with-current-buffer view
          (let ((inhibit-read-only t) (tick (buffer-chars-modified-tick)))
            (goto-char (point-max))
            (combine-change-calls (point) (point)
              (when (and (eq hermes-buffer--owner (plist-get op :recovery-claim))
                         (hermes-buffer--owned-p 'hermes-file-view-mode)
                         (= tick (buffer-chars-modified-tick)))
                (insert "\nReturned reference:\n" ref "\n")))))))
    ;; Recovery change hooks may cancel OP and start a replacement in the chat.
    ;; Recheck before touching any composer hook owned by that replacement.
    (hermes-chat-attachments--check op)
    (with-current-buffer (plist-get op :buffer)
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-max))
          (remove-hook 'before-change-functions #'hermes-chat-attachments--edited t)
          (let ((tick (buffer-chars-modified-tick)))
            (combine-change-calls (point) (point)
              (when (and (hermes-chat-attachments--current-p op) (not (plist-get op :edited))
                         (= tick (buffer-chars-modified-tick))
                         (equal (plist-get op :draft) (hermes-chat-input-string)))
                (insert (if (string-empty-p (plist-get op :draft)) "" "\n") ref)))))))
    (hermes-chat-attachments--finish op 'attached)
    (message "Attachment reference retained; review draft/recovery before Send. Remote file remains")))

;;;###autoload
(defun hermes-chat-attach-file (&optional file)
  "Choose bounded local FILE and consent to a gateway-workspace write.
Require an already attached ready chat.  Preserve bytes and original draft in
an inert recovery view; newer draft edits require manual reference copying.
Oversized reads retain an explicitly incomplete prefix, not the unread tail.
Only UTF-8 text/source is supported, not PDF/binary extraction.  Managed-files
policy and gateway-host cwd must agree; this does not map an execution backend.
Upload/readback/file.attach are non-atomic and never retried automatically.
No prompt is sent, remote rollback is not attempted, and context can change."
  (interactive nil hermes-chat-mode)
  (let ((op (hermes-chat-attachments--begin)) started)
    (unwind-protect
        (progn
          (unless file (setq file (read-file-name "Local text/source attachment: " nil nil t)))
          (hermes-chat-attachments--check op)
          (hermes-chat-attachments--read op file)
          (hermes-chat-attachments--rpc
           op "session.activate" `((session_id . ,(plist-get op :session)) (omit_messages . t))
           #'hermes-chat-attachments--cwd)
          (setq started t))
      (unless started (hermes-chat-attachments--finish op 'read-or-owner-failure)))))

(provide 'hermes-chat-attachments)
;;; hermes-chat-attachments.el ends here
