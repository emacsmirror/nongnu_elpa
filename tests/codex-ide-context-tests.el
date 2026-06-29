;;; codex-ide-context-tests.el --- ERT tests for codex-ide-context  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ERT tests for the Codex IDE context IPC provider.  All tests run under
;; `emacs -Q --batch' with no live sockets, no network, and no real Codex
;; process.  The process/filter path is exercised through the pure frame
;; parser rather than live connections.

;;; Code:

(require 'ert)
(require 'codex-ide-context)

(defvar native-comp-enable-subr-trampolines)

(defun codex-ide-context-test--request (request-id method params
                                                   &optional version)
  "Build a request plist matching the Codex wire shape."
  (append (list :type "request"
                :requestId request-id
                :sourceClientId "codex-tui"
                :version (if (null version) 0 version)
                :method method)
          (when params (list :params params))))

(defun codex-ide-context-test--call-with-socket-path (body)
  "Call BODY with a private temporary IPC directory and socket path."
  (let* ((directory (make-temp-file "codex-ipc-test-" t))
         (path (expand-file-name "ipc-test.sock" directory)))
    (unwind-protect
        (progn
          (set-file-modes directory #o700)
          (funcall body directory path))
      (ignore-errors
        (delete-directory directory t)))))

;;; Frame codec

(ert-deftest codex-ide-context-u32-le-bytes ()
  "Little-endian u32 encoding produces 4 bytes in increasing significance."
  (should (equal (codex-ide-context--u32-le-bytes 0)
                 (unibyte-string 0 0 0 0)))
  (should (equal (codex-ide-context--u32-le-bytes 1)
                 (unibyte-string 1 0 0 0)))
  (should (equal (codex-ide-context--u32-le-bytes 256)
                 (unibyte-string 0 1 0 0)))
  (should (equal (codex-ide-context--u32-le-bytes #x01020304)
                 (unibyte-string 4 3 2 1))))

(ert-deftest codex-ide-context-decode-length-little-endian ()
  "Length header decodes little-endian."
  (should (= (codex-ide-context--decode-length
              (unibyte-string 1 0 0 0)) 1))
  (should (= (codex-ide-context--decode-length
              (unibyte-string 0 1 0 0)) 256))
  (should (= (codex-ide-context--decode-length
              (unibyte-string 4 3 2 1)) #x01020304)))

(ert-deftest codex-ide-context-encode-frame-length-prefix ()
  "Frame starts with a 4-byte LE length prefix over the JSON payload."
  (let* ((message (list :type "request" :requestId "abc"))
         (frame (codex-ide-context--encode-frame message)))
    (should (stringp frame))
    (should (> (length frame) 4))
    (let ((declared (codex-ide-context--decode-length
                     (substring frame 0 4)))
          (payload-len (- (length frame) 4)))
      (should (= declared payload-len)))))

(ert-deftest codex-ide-context-encode-frame-empty-payload ()
  "Encoding nil yields a 4-byte JSON null payload."
  (let ((frame (codex-ide-context--encode-frame nil)))
    (should (= (codex-ide-context--decode-length (substring frame 0 4))
               4))))

(ert-deftest codex-ide-context-encode-decode-roundtrip ()
  "Encoding then decoding a frame recovers the original message plist."
  (let* ((message (list :type "request"
                        :requestId "req-1"
                        :method "ide-context"
                        :params (list :workspaceRoot "/repo")))
         (frame (codex-ide-context--encode-frame message))
         (payload (substring frame 4))
         (decoded (codex-ide-context--decode-payload payload)))
    (should (equal (plist-get decoded :type) "request"))
    (should (equal (plist-get decoded :requestId) "req-1"))
    (should (equal (plist-get decoded :method) "ide-context"))
    (should (equal (plist-get (plist-get decoded :params) :workspaceRoot)
                   "/repo"))))

;;; Protocol builders

(ert-deftest codex-ide-context-success-response-shape ()
  "Success response carries requestId, metadata, and result.ideContext."
  (let ((resp (codex-ide-context--success-response
               "req-1" '((activeFile)))))
    (should (equal (cdr (assoc "type" resp)) "response"))
    (should (equal (cdr (assoc "requestId" resp)) "req-1"))
    (should (equal (cdr (assoc "resultType" resp)) "success"))
    (should (equal (cdr (assoc "method" resp)) "ide-context"))
    (should (equal (cdr (assoc "handledByClientId" resp))
                   "codex-emacs"))
    (should (assoc "ideContext" (cdr (assoc "result" resp))))))

(ert-deftest codex-ide-context-error-response-shape ()
  "Error response carries resultType error and the error string."
  (let ((resp (codex-ide-context--error-response "req-2" "boom")))
    (should (equal (cdr (assoc "resultType" resp)) "error"))
    (should (equal (cdr (assoc "error" resp)) "boom"))))

(ert-deftest codex-ide-context-discovery-response-shape ()
  "Discovery response reports canHandle and echoes the requestId."
  (let ((resp (codex-ide-context--discovery-response "d-1" t)))
    (should (equal (cdr (assoc "type" resp))
                   "client-discovery-response"))
    (should (equal (cdr (assoc "requestId" resp)) "d-1"))
    (should (eq (cdr (assoc "canHandle"
                            (cdr (assoc "response" resp))))
                t))))

(ert-deftest codex-ide-context-discovery-response-uses-json-false ()
  "Discovery response encodes canHandle nil as JSON false."
  (let* ((resp (codex-ide-context--discovery-response "d-2" nil))
         (frame (codex-ide-context--encode-frame resp))
         (payload (decode-coding-string (substring frame 4) 'utf-8)))
    (should (string-match-p "\"canHandle\":false" payload))
    (should-not (string-match-p "\"canHandle\":\"false\"" payload))))

(ert-deftest codex-ide-context-unsupported-response-shape ()
  "Unsupported request yields a no-handler-for-request error."
  (let ((resp (codex-ide-context--unsupported-response
               (list :requestId "req-3"))))
    (should (equal (cdr (assoc "error" resp))
                   "no-handler-for-request"))
    (should (equal (cdr (assoc "requestId" resp)) "req-3"))))

;;; Dispatch

(ert-deftest codex-ide-context-handle-ide-context-request ()
  "An ide-context request dispatches to a success response."
  (let* ((request (codex-ide-context-test--request
                   "req-1" "ide-context"
                   (list :workspaceRoot "/repo")))
         (resp (codex-ide-context--handle-message request "/repo")))
    (should (equal (cdr (assoc "type" resp)) "response"))
    (should (equal (cdr (assoc "resultType" resp)) "success"))
    (should (assoc "ideContext" (cdr (assoc "result" resp))))))

(ert-deftest codex-ide-context-handle-unknown-request ()
  "An unknown method dispatches to a no-handler-for-request error."
  (let* ((request (codex-ide-context-test--request
                   "req-2" "some-other-method" nil))
         (resp (codex-ide-context--handle-message request "/repo")))
    (should (equal (cdr (assoc "resultType" resp)) "error"))
    (should (equal (cdr (assoc "error" resp))
                   "no-handler-for-request"))))

(ert-deftest codex-ide-context-handle-version-mismatch ()
  "An unsupported version dispatches to a request-version-mismatch error."
  (let* ((request (codex-ide-context-test--request
                   "req-version" "ide-context"
                   (list :workspaceRoot "/repo") 99))
         (resp (codex-ide-context--handle-message request "/repo")))
    (should (equal (cdr (assoc "resultType" resp)) "error"))
    (should (equal (cdr (assoc "error" resp))
                   "request-version-mismatch"))))

(ert-deftest codex-ide-context-handle-discovery-request ()
  "A client-discovery-request dispatches to a discovery response."
  (let* ((msg (list :type "client-discovery-request"
                    :requestId "d-1"))
         (resp (codex-ide-context--handle-message msg "/repo")))
    (should (equal (cdr (assoc "type" resp))
                   "client-discovery-response"))
    (should (eq (cdr (assoc "canHandle"
                            (cdr (assoc "response" resp))))
                t))))

(ert-deftest codex-ide-context-discovery-rejects-unsupported-request ()
  "Discovery reports false for an unsupported embedded request."
  (let* ((msg (list :type "client-discovery-request"
                    :requestId "d-unsupported"
                    :request (codex-ide-context-test--request
                              "req" "ide-context"
                              (list :workspaceRoot "/repo") 2)))
         (resp (codex-ide-context--handle-message msg "/repo")))
    (should (eq (cdr (assoc "canHandle"
                            (cdr (assoc "response" resp))))
                :json-false))))

(ert-deftest codex-ide-context-handle-broadcast-ignored ()
  "Broadcasts produce no response."
  (let ((resp (codex-ide-context--handle-message
               (list :type "broadcast" :method "something")
               "/repo")))
    (should (null resp))))

(ert-deftest codex-ide-context-handle-response-ignored ()
  "Stray responses are ignored."
  (should (null (codex-ide-context--handle-message
                 (list :type "response" :requestId "x")
                 "/repo"))))

;;; Context serialization

(ert-deftest codex-ide-context-region->range ()
  "Region range uses zero-based line and character positions."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird line")
    ;; Position at start of "second" (line index 1, char 0).
    (should (equal (codex-ide-context--region->range 12 18)
                   (list (cons "start" (list (cons "line" 1)
                                             (cons "character" 0)))
                         (cons "end" (list (cons "line" 1)
                                           (cons "character" 6))))))))

(ert-deftest codex-ide-context-relative-path ()
  "Paths under the workspace root are relativized."
  (should (equal (codex-ide-context--relative-path "/repo/src/lib.rs" "/repo")
                 "src/lib.rs"))
  ;; Outside the root stays absolute.
  (should (equal (codex-ide-context--relative-path "/etc/hosts" "/repo")
                 "/etc/hosts"))
  ;; Sibling prefixes are not treated as being inside the root.
  (should (equal (codex-ide-context--relative-path "/repo2/file.el" "/repo")
                 "/repo2/file.el"))
  ;; Nil root keeps the path as-is.
  (should (equal (codex-ide-context--relative-path "/repo/src/lib.rs" nil)
                 "/repo/src/lib.rs")))

(ert-deftest codex-ide-context-active-file-with-selection ()
  "An active file buffer with a region serializes selection and content."
  (let ((file (expand-file-name "test-active.el"
                                temporary-file-directory)))
    (write-region "(message \"hi\")\n(second line)\n" nil file)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert-file-contents file)
          ;; Select "second" on line 2.
          (forward-line 1)
          (forward-char 1)
          (let ((transient-mark-mode t))
            (push-mark (point) t t)
            (forward-char 6)
            (setq mark-active t)
            (let ((active (codex-ide-context--active-file
                           temporary-file-directory)))
              (should active)
              (should (equal (cdr (assoc "label" active)) "test-active.el"))
              (should (equal (cdr (assoc "activeSelectionContent" active))
                             "second"))
              (should (equal (cdr (assoc "path" active)) "test-active.el"))
              (should (equal (cdr (assoc "fsPath" active))
                             (expand-file-name file)))
              (should (alist-get "selection" active nil nil #'equal)))))
      (delete-file file))))

(ert-deftest codex-ide-context-active-file-no-selection ()
  "A file buffer with no active region reports point as a collapsed range."
  (let ((file (expand-file-name "test-nosel.el"
                                temporary-file-directory)))
    (write-region "hello\n" nil file)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((active (codex-ide-context--active-file
                         temporary-file-directory)))
            (should active)
            (should (equal (cdr (assoc "activeSelectionContent" active)) ""))
            (should (vectorp (cdr (assoc "selections" active))))
            ;; selection start == end at point.
            (let ((start (cdr (assoc "start" (cdr (assoc "selection" active)))))
                  (end (cdr (assoc "end" (cdr (assoc "selection" active))))))
              (should (equal start end)))))
      (delete-file file))))

(ert-deftest codex-ide-context-selection-truncation-multibyte ()
  "Selection truncation counts characters without splitting multibyte text."
  (let ((file (expand-file-name "test-selection-multibyte.el"
                                temporary-file-directory))
        (codex-ide-context-selection-content-limit 3))
    (write-region "αβγδε\n" nil file)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert-file-contents file)
          (let ((transient-mark-mode t))
            (goto-char (point-min))
            (push-mark (point) t t)
            (forward-char 5)
            (setq mark-active t)
            (let* ((active (codex-ide-context--active-file
                            temporary-file-directory))
                   (content (cdr (assoc "activeSelectionContent" active))))
              (should (equal content "αβγ"))
              (should (= (length content) 3)))))
      (delete-file file))))

(ert-deftest codex-ide-context-selection-truncation-under-limit ()
  "Selection content shorter than the limit is preserved unchanged."
  (let ((file (expand-file-name "test-selection-under-limit.el"
                                temporary-file-directory))
        (codex-ide-context-selection-content-limit 20))
    (write-region "λx. x\n" nil file)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert-file-contents file)
          (let ((transient-mark-mode t))
            (goto-char (point-min))
            (push-mark (point) t t)
            (forward-char 5)
            (setq mark-active t)
            (let ((active (codex-ide-context--active-file
                           temporary-file-directory)))
              (should (equal (cdr (assoc "activeSelectionContent" active))
                             "λx. x")))))
      (delete-file file))))

(ert-deftest codex-ide-context-collect-no-selection-empty-vector ()
  "Collect reports empty selection content and vector selections at point."
  (let ((file (expand-file-name "test-collect-nosel.el"
                                temporary-file-directory)))
    (write-region "hello\n" nil file)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert-file-contents file)
          (goto-char (point-min))
          (let* ((context (codex-ide-context--collect
                           temporary-file-directory (current-buffer)))
                 (active (cdr (assoc "activeFile" context)))
                 (selection (cdr (assoc "selection" active)))
                 (start (cdr (assoc "start" selection)))
                 (end (cdr (assoc "end" selection))))
            (should (equal (cdr (assoc "activeSelectionContent" active))
                           ""))
            (should (vectorp (cdr (assoc "selections" active))))
            (should (equal start end))))
      (delete-file file))))

(ert-deftest codex-ide-context-selection-from-narrowed-buffer ()
  "Selection collection works when the active buffer is narrowed."
  (let ((file (expand-file-name "test-selection-narrowed.el"
                                temporary-file-directory)))
    (write-region "hidden\nvisible text\nhidden\n" nil file)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert-file-contents file)
          (forward-line 1)
          (let ((beg (point))
                (end (line-end-position)))
            (narrow-to-region beg end)
            (let ((transient-mark-mode t))
              (goto-char (point-min))
              (push-mark (point) t t)
              (forward-char 7)
              (setq mark-active t)
              (let ((active (codex-ide-context--active-file
                             temporary-file-directory)))
                (should (equal (cdr (assoc "activeSelectionContent" active))
                               "visible"))))))
      (delete-file file))))

(ert-deftest codex-ide-context-active-file-non-file-buffer ()
  "A buffer visiting no file returns nil for active-file."
  (with-temp-buffer
    (insert "not a file")
    (should (null (codex-ide-context--active-file "/repo")))))

(ert-deftest codex-ide-context-selected-project-file-wins-over-tracked ()
  "The selected project file takes precedence over a tracked source buffer."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-context-root-" t)))
         (tracked-file (expand-file-name "tracked.el" root))
         (selected-file (expand-file-name "selected.el" root))
         (codex-ide-context--source-buffers (make-hash-table :test 'equal))
         tracked selected)
    (unwind-protect
        (progn
          (write-region "tracked\n" nil tracked-file)
          (write-region "selected\n" nil selected-file)
          (setq tracked (find-file-noselect tracked-file))
          (setq selected (find-file-noselect selected-file))
          (codex-ide-context-record-source-buffer root tracked)
          (save-window-excursion
            (switch-to-buffer selected)
            (should (eq (codex-ide-context--resolve-source-buffer root)
                        selected))))
      (when (buffer-live-p tracked)
        (kill-buffer tracked))
      (when (buffer-live-p selected)
        (kill-buffer selected))
      (delete-directory root t))))

(ert-deftest codex-ide-context-tracked-source-used-from-non-file-buffer ()
  "A tracked source buffer is used when the selected buffer is not a file."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-context-tracked-root-" t)))
         (source-file (expand-file-name "source.el" root))
         (codex-ide-context--source-buffers (make-hash-table :test 'equal))
         source scratch)
    (unwind-protect
        (progn
          (write-region "source\n" nil source-file)
          (setq source (find-file-noselect source-file))
          (setq scratch (get-buffer-create " *codex-context-non-file*"))
          (codex-ide-context-record-source-buffer root source)
          (save-window-excursion
            (switch-to-buffer scratch)
            (should (eq (codex-ide-context--resolve-source-buffer root)
                        source))))
      (when (buffer-live-p source)
        (kill-buffer source))
      (when (buffer-live-p scratch)
        (kill-buffer scratch))
      (delete-directory root t))))

(ert-deftest codex-ide-context-stale-tracked-source-is-ignored ()
  "Killed tracked buffers are not used as active IDE context."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-context-stale-root-" t)))
         (source-file (expand-file-name "source.el" root))
         (codex-ide-context--source-buffers (make-hash-table :test 'equal))
         source scratch)
    (unwind-protect
        (progn
          (write-region "source\n" nil source-file)
          (setq source (find-file-noselect source-file))
          (setq scratch (get-buffer-create " *codex-context-stale*"))
          (codex-ide-context-record-source-buffer root source)
          (kill-buffer source)
          (save-window-excursion
            (switch-to-buffer scratch)
            (should-not (codex-ide-context--resolve-source-buffer root))))
      (when (buffer-live-p source)
        (kill-buffer source))
      (when (buffer-live-p scratch)
        (kill-buffer scratch))
      (delete-directory root t))))

(ert-deftest codex-ide-context-outside-root-tracked-source-is-ignored ()
  "Tracked buffers outside the requested root are not used."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-context-root-" t)))
         (outside-root (file-name-as-directory
                        (make-temp-file "codex-context-outside-" t)))
         (outside-file (expand-file-name "outside.el" outside-root))
         (codex-ide-context--source-buffers (make-hash-table :test 'equal))
         outside scratch)
    (unwind-protect
        (progn
          (write-region "outside\n" nil outside-file)
          (setq outside (find-file-noselect outside-file))
          (setq scratch (get-buffer-create " *codex-context-outside*"))
          (puthash (codex-ide-context--normalize-root root)
                   outside codex-ide-context--source-buffers)
          (save-window-excursion
            (switch-to-buffer scratch)
            (should-not (codex-ide-context--resolve-source-buffer root))))
      (when (buffer-live-p outside)
        (kill-buffer outside))
      (when (buffer-live-p scratch)
        (kill-buffer scratch))
      (delete-directory root t)
      (delete-directory outside-root t))))

(ert-deftest codex-ide-context-request-uses-resolved-source-buffer ()
  "An ide-context request serializes the resolved active source buffer."
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-context-request-root-" t)))
         (source-file (expand-file-name "source.el" root))
         (codex-ide-context--source-buffers (make-hash-table :test 'equal))
         source scratch)
    (unwind-protect
        (progn
          (write-region "source\n" nil source-file)
          (setq source (find-file-noselect source-file))
          (setq scratch (get-buffer-create " *codex-context-request*"))
          (codex-ide-context-record-source-buffer root source)
          (save-window-excursion
            (switch-to-buffer scratch)
            (let* ((request (codex-ide-context-test--request
                             "req-ctx" "ide-context"
                             (list :workspaceRoot root)))
                   (response (codex-ide-context--handle-message request root))
                   (result (cdr (assoc "result" response)))
                   (context (cdr (assoc "ideContext" result)))
                   (active (cdr (assoc "activeFile" context))))
              (should (equal (cdr (assoc "path" active)) "source.el")))))
      (when (buffer-live-p source)
        (kill-buffer source))
      (when (buffer-live-p scratch)
        (kill-buffer scratch))
      (delete-directory root t))))

(ert-deftest codex-ide-context-open-tabs-filter ()
  "Open tabs only include file-visiting buffers under the root."
  (let* ((root (make-temp-file "codex-tabs-root-" t))
         (file-a (expand-file-name "a.el" root))
         (file-b (expand-file-name "b.el" root))
         (outside (expand-file-name "codex-outside.el"
                                    temporary-file-directory)))
    (unwind-protect
        (progn
          (write-region "aaa\n" nil file-a)
          (write-region "bbb\n" nil file-b)
          (write-region "out\n" nil outside)
          (let (buf-a buf-b buf-out buf-scratch)
            (unwind-protect
                (progn
                  (setq buf-a (find-file-noselect file-a))
                  (setq buf-b (find-file-noselect file-b))
                  (setq buf-out (find-file-noselect outside))
                  (setq buf-scratch (get-buffer-create "*codex-test-scratch*"))
                  (let ((tabs (codex-ide-context--open-tabs root)))
                    (should (consp tabs))
                    (should (= (length tabs) 2))
                    (should-not (cl-member outside tabs
                                           :test (lambda (path desc)
                                                   (equal path
                                                          (cdr (assoc "path" desc))))))
                    (dolist (desc tabs)
                      (should (assoc "label" desc))
                      (should (assoc "path" desc))
                      (should (file-name-absolute-p
                               (cdr (assoc "fsPath" desc)))))))
              (when buf-a (kill-buffer buf-a))
              (when buf-b (kill-buffer buf-b))
              (when buf-out (kill-buffer buf-out))
              (when buf-scratch (kill-buffer buf-scratch)))))
      (ignore-errors (delete-directory root t))
      (ignore-errors (delete-file outside)))))

(ert-deftest codex-ide-context-open-tabs-scoped-empty ()
  "Open tabs returns no buffers when WORKSPACE-ROOT has no open files."
  (let* ((root (make-temp-file "codex-tabs-empty-root-" t))
         (outside (expand-file-name "codex-outside-only.el"
                                    temporary-file-directory)))
    (unwind-protect
        (progn
          (write-region "out\n" nil outside)
          (let ((buf (find-file-noselect outside)))
            (unwind-protect
                (should (null (codex-ide-context--open-tabs root)))
              (kill-buffer buf))))
      (ignore-errors (delete-directory root t))
      (ignore-errors (delete-file outside)))))

(ert-deftest codex-ide-context-collect-shape ()
  "Collect returns an ideContext alist with activeFile and openTabs."
  (let ((file (expand-file-name "collect-test.el"
                                temporary-file-directory)))
    (write-region "content\n" nil file)
    (unwind-protect
        (let (buf)
          (unwind-protect
              (progn
                (setq buf (find-file-noselect file))
                (with-current-buffer buf
                  (let ((ctx (codex-ide-context--collect
                              temporary-file-directory buf)))
                    (should (vectorp (alist-get "openTabs" ctx nil nil #'equal)))
                    (should (> (length (alist-get "openTabs" ctx nil nil #'equal))
                               0))
                    (should (alist-get "activeFile" ctx nil nil #'equal)))))
            (when buf (kill-buffer buf))))
      (delete-file file))))

;;; Frame parser (multi-frame and partial)

(ert-deftest codex-ide-context-parse-frames-single ()
  "A single complete frame decodes to one message."
  (let* ((msg (list :type "broadcast" :method "x"))
         (frame (codex-ide-context--encode-frame msg))
         (result (codex-ide-context--parse-frames frame nil)))
    (should (= (length (car result)) 1))
    (should (equal (plist-get (car (car result)) :type) "broadcast"))))

(ert-deftest codex-ide-context-parse-frames-partial-payload ()
  "A frame whose payload has not fully arrived yields no message."
  (let* ((msg (list :type "broadcast" :method "x"))
         (frame (codex-ide-context--encode-frame msg))
         (partial (substring frame 0 6)))
    (let ((result (codex-ide-context--parse-frames partial nil)))
      (should (null (car result)))
      ;; Header is consumed; tail keeps the two arrived payload bytes and
      ;; tail length records the full expected payload length.
      (should (= (length (car (cdr result))) 2))
      (should (= (cdr (cdr result))
                 (codex-ide-context--decode-length
                  (substring frame 0 4)))))))

(ert-deftest codex-ide-context-parse-frames-partial-header ()
  "Fewer than 4 bytes yields no length and no message."
  (let ((result (codex-ide-context--parse-frames
                 (unibyte-string 1 0) nil)))
    (should (null (car result)))
    (should (= (length (car (cdr result))) 2))))

(ert-deftest codex-ide-context-parse-frames-two-frames ()
  "Two concatenated frames decode to two messages."
  (let* ((msg1 (list :type "broadcast" :method "a"))
         (msg2 (list :type "broadcast" :method "b"))
         (frame (concat (codex-ide-context--encode-frame msg1)
                        (codex-ide-context--encode-frame msg2))))
    (let ((result (codex-ide-context--parse-frames frame nil)))
      (should (= (length (car result)) 2))
      (should (equal (plist-get (nth 0 (car result)) :method) "a"))
      (should (equal (plist-get (nth 1 (car result)) :method) "b")))))

(ert-deftest codex-ide-context-parse-frames-too-large ()
  "An oversized declared length signals a dedicated condition."
  (let ((oversized (concat (codex-ide-context--u32-le-bytes
                            (1+ codex-ide-context-max-frame-size))
                           (unibyte-string 0 0 0 0))))
    (should-error (codex-ide-context--parse-frames oversized nil)
                  :type 'codex-ide-context-frame-too-large)))

;;; Broadcast push

(ert-deftest codex-ide-context-broadcast-no-clients ()
  "Broadcasting with no connected clients is a no-op that returns zero."
  (let ((codex-ide-context--clients (make-hash-table :test 'eq)))
    (should (= (codex-ide-context--broadcast '(("type" . "broadcast")))
               0))))

(ert-deftest codex-ide-context-broadcast-with-stub-clients ()
  "Broadcast sends one encoded frame to each live client."
  (let ((codex-ide-context--clients (make-hash-table :test 'eq))
        sent)
    (puthash 'client-a nil codex-ide-context--clients)
    (puthash 'dead-client nil codex-ide-context--clients)
    (puthash 'client-b nil codex-ide-context--clients)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (proc) (not (eq proc 'dead-client))))
              ((symbol-function 'process-send-string)
               (lambda (proc frame)
                 (push (cons proc frame) sent))))
      (should (= (codex-ide-context--broadcast
                  '(("type" . "broadcast")
                    ("method" . "ide-context")))
                 2))
      (should (= (length sent) 2))
      (should (assoc 'client-a sent))
      (should (assoc 'client-b sent))
      (let* ((frame (cdr (assoc 'client-a sent)))
             (payload (substring frame 4))
             (decoded (codex-ide-context--decode-payload payload)))
        (should (equal (plist-get decoded :type) "broadcast"))
        (should (equal (plist-get decoded :method) "ide-context"))))))

(ert-deftest codex-ide-context-selection-broadcast-shape ()
  "Selection broadcast wraps collected context in an ide-context broadcast."
  (let ((file (expand-file-name "test-selection-broadcast.el"
                                temporary-file-directory))
        captured)
    (write-region "selected\n" nil file)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert-file-contents file)
          (let ((transient-mark-mode t))
            (goto-char (point-min))
            (push-mark (point) t t)
            (forward-char 8)
            (setq mark-active t)
            (cl-letf (((symbol-function 'codex-ide-context--broadcast)
                       (lambda (message)
                         (setq captured message)
                         2)))
              (should (= (codex-ide-context--selection-broadcast
                          temporary-file-directory (current-buffer))
                         2)))
            (should (equal (cdr (assoc "type" captured)) "broadcast"))
            (should (equal (cdr (assoc "method" captured)) "ide-context"))
            (let* ((params (cdr (assoc "params" captured)))
                   (context (cdr (assoc "ideContext" params)))
                   (active (cdr (assoc "activeFile" context))))
              (should (equal (cdr (assoc "activeSelectionContent" active))
                             "selected")))))
      (delete-file file))))

(ert-deftest codex-ide-context-send-selection-fallback-copies-region ()
  "Manual selection push copies the region when no client is connected."
  (let ((kill-ring nil)
        (interprogram-cut-function nil))
    (with-temp-buffer
      (insert "copy me")
      (let ((transient-mark-mode t))
        (goto-char (point-min))
        (push-mark (point) t t)
        (goto-char (point-max))
        (setq mark-active t)
        (cl-letf (((symbol-function 'codex-ide-context--selection-broadcast)
                   (lambda (_workspace-root &optional _buffer) 0)))
          (codex-ide-send-selection temporary-file-directory))
        (should (equal (car kill-ring) "copy me"))))))

(ert-deftest codex-ide-context-send-selection-broadcasts-current-buffer ()
  "Manual selection push broadcasts the command's current buffer."
  (let ((file (expand-file-name "test-send-selection-current.el"
                                temporary-file-directory))
        captured)
    (write-region "current\n" nil file)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert-file-contents file)
          (let ((transient-mark-mode t))
            (goto-char (point-min))
            (push-mark (point) t t)
            (forward-char 7)
            (setq mark-active t)
            (cl-letf (((symbol-function 'codex-ide-context--broadcast)
                       (lambda (message)
                         (setq captured message)
                         1)))
              (codex-ide-send-selection temporary-file-directory))
            (let* ((params (cdr (assoc "params" captured)))
                   (context (cdr (assoc "ideContext" params)))
                   (active (cdr (assoc "activeFile" context))))
              (should (equal (cdr (assoc "activeSelectionContent" active))
                             "current")))))
      (delete-file file))))

(ert-deftest codex-ide-context-docstring-selection-limit-says-characters ()
  "Selection content limit documentation matches character truncation."
  (let ((doc (documentation-property
              'codex-ide-context-selection-content-limit
              'variable-documentation)))
    (should (string-match-p "characters" doc))
    (should-not (string-match-p (rx word-start "bytes" word-end) doc))))

;;; Lifecycle

(ert-deftest codex-ide-context-start-server-idempotent ()
  "Starting an already live provider returns the existing server."
  (codex-ide-context-test--call-with-socket-path
   (lambda (directory _path)
     (let ((codex-ide-context--server nil)
           (codex-ide-context--owned-socket-path nil)
           (codex-ide-context--owned-socket-identity nil)
           (codex-ide-context-socket-directory directory)
           (created 0)
           network-args)
       (cl-letf (((symbol-function 'make-network-process)
                  (lambda (&rest args)
                    (setq network-args args)
                    (setq created (1+ created))
                    (write-region "" nil (plist-get args :service)
                                  nil 'silent)
                    'server))
                 ((symbol-function 'process-live-p)
                  (lambda (proc) (eq proc 'server)))
                 ((symbol-function 'codex-ide-debug)
                  (lambda (&rest _args) nil)))
         (should (eq (codex-ide-context--start-server) 'server))
         (should (eq (codex-ide-context--start-server) 'server))
         (should (eq (plist-get network-args :coding) 'binary))
         (should codex-ide-context--owned-socket-identity)
         (should (= created 1)))))))

(ert-deftest codex-ide-context-start-server-replaces-orphan-process ()
  "Starting removes a live server process with lost socket ownership."
  (codex-ide-context-test--call-with-socket-path
   (lambda (directory _path)
     (let* ((codex-ide-context--server 'orphan)
            (codex-ide-context--owned-socket-path nil)
            (codex-ide-context--owned-socket-identity nil)
            (codex-ide-context-socket-directory directory)
            (path (codex-ide-context--socket-path))
            (created 0)
            deleted-processes)
       (cl-letf (((symbol-function 'make-network-process)
                  (lambda (&rest args)
                    (setq created (1+ created))
                    (write-region "" nil (plist-get args :service)
                                  nil 'silent)
                    'server))
                 ((symbol-function 'process-live-p)
                  (lambda (proc) (memq proc '(orphan server))))
                 ((symbol-function 'delete-process)
                  (lambda (proc)
                    (push proc deleted-processes)))
                 ((symbol-function 'codex-ide-debug)
                  (lambda (&rest _args) nil)))
         (should (eq (codex-ide-context--start-server) 'server))
         (should (equal deleted-processes '(orphan)))
         (should (equal codex-ide-context--owned-socket-path path))
         (should codex-ide-context--owned-socket-identity)
         (should (= created 1)))))))

(ert-deftest codex-ide-context-stop-server-idempotent-cleans-clients ()
  "Stopping deletes live clients, clears state, and tolerates repeats."
  (let* ((path "/tmp/codex-ipc/ipc-test.sock")
         (codex-ide-context--server 'server)
         (codex-ide-context--owned-socket-path path)
         (codex-ide-context--owned-socket-identity '(10 20))
         (codex-ide-context--clients (make-hash-table :test 'eq))
         (native-comp-enable-subr-trampolines nil)
         deleted-processes deleted-files)
    (puthash 'client-a nil codex-ide-context--clients)
    (puthash 'dead-client nil codex-ide-context--clients)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (proc) (memq proc '(server client-a))))
              ((symbol-function 'delete-process)
               (lambda (proc)
                 (push proc deleted-processes)))
              ((symbol-function 'codex-ide-context--socket-path)
               (lambda () path))
              ((symbol-function 'file-exists-p)
               (lambda (_path) t))
              ((symbol-function 'codex-ide-context--socket-file-p)
               (lambda (_path) t))
              ((symbol-function 'codex-ide-context--socket-identity)
               (lambda (_path) '(10 20)))
              ((symbol-function 'delete-file)
               (lambda (file)
                 (push file deleted-files)))
              ((symbol-function 'codex-ide-debug)
               (lambda (&rest _args) nil)))
      (codex-ide-context--stop-server)
      (codex-ide-context--stop-server)
      (should (memq 'client-a deleted-processes))
      (should (memq 'server deleted-processes))
      (should-not (memq 'dead-client deleted-processes))
      (should (= (hash-table-count codex-ide-context--clients) 0))
      (should-not codex-ide-context--server)
      (should-not codex-ide-context--owned-socket-path)
      (should-not codex-ide-context--owned-socket-identity)
      (should (equal deleted-files (list path))))))

(ert-deftest codex-ide-context-stop-server-keeps-replaced-socket ()
  "Stopping does not remove a socket that replaced the owned one."
  (let* ((path "/tmp/codex-ipc/ipc-test.sock")
         (codex-ide-context--server nil)
         (codex-ide-context--owned-socket-path path)
         (codex-ide-context--owned-socket-identity '(10 20))
         (codex-ide-context--clients (make-hash-table :test 'eq))
         (native-comp-enable-subr-trampolines nil)
         deleted)
    (cl-letf (((symbol-function 'codex-ide-context--socket-path)
               (lambda () path))
              ((symbol-function 'file-exists-p)
               (lambda (_path) t))
              ((symbol-function 'codex-ide-context--socket-identity)
               (lambda (_path) '(30 40)))
              ((symbol-function 'codex-ide-context--socket-file-p)
               (lambda (_path) t))
              ((symbol-function 'delete-file)
               (lambda (file)
                 (setq deleted file)))
              ((symbol-function 'codex-ide-debug)
               (lambda (&rest _args) nil)))
      (codex-ide-context--stop-server)
      (should-not deleted)
      (should-not codex-ide-context--owned-socket-path)
      (should-not codex-ide-context--owned-socket-identity))))

(ert-deftest codex-ide-context-mode-owns-hooks-and-tracked-buffers ()
  "The global mode installs tracking and clears tracked source buffers."
  (let ((codex-ide-context-mode nil)
        (codex-ide-context--source-buffers (make-hash-table :test 'equal))
        (window-selection-change-functions nil)
        (kill-emacs-hook nil)
        starts stops records)
    (cl-letf (((symbol-function 'codex-ide-context--start-server)
               (lambda ()
                 (push :start starts)
                 'server))
              ((symbol-function 'codex-ide-context--stop-server)
               (lambda ()
                 (push :stop stops)))
              ((symbol-function 'codex-ide-context-record-source-buffer)
               (lambda (&rest _args)
                 (push :record records))))
      (codex-ide-context-mode 1)
      (should codex-ide-context-mode)
      (should (memq #'codex-ide-context--record-window-selection
                    window-selection-change-functions))
      (should (memq #'codex-ide-context--cleanup-on-exit
                    kill-emacs-hook))
      (puthash "/repo/" 'buffer codex-ide-context--source-buffers)
      (codex-ide-context-mode -1)
      (should-not codex-ide-context-mode)
      (should-not (memq #'codex-ide-context--record-window-selection
                        window-selection-change-functions))
      (should-not (memq #'codex-ide-context--cleanup-on-exit
                        kill-emacs-hook))
      (should (= (hash-table-count codex-ide-context--source-buffers) 0))
      (should (= (length starts) 1))
      (should (= (length stops) 1))
      (should (= (length records) 1)))))

;;; Socket ownership

(ert-deftest codex-ide-context-prepare-socket-refuses-live-provider ()
  "A live socket owned by another provider is left in place."
  (codex-ide-context-test--call-with-socket-path
   (lambda (directory path)
     (write-region "" nil path nil 'silent)
     (cl-letf (((symbol-function 'codex-ide-context--socket-state)
               (lambda (_path) 'live))
               ((symbol-function 'delete-file)
                (lambda (_file)
                  (error "should not delete live socket"))))
       (should-error
        (codex-ide-context--prepare-socket-path directory path)
        :type 'user-error)
       (should (file-exists-p path))))))

(ert-deftest codex-ide-context-prepare-socket-deletes-stale-socket ()
  "A stale socket in the owned private directory is removed before bind."
  (codex-ide-context-test--call-with-socket-path
   (lambda (directory path)
     (write-region "" nil path nil 'silent)
     (cl-letf (((symbol-function 'codex-ide-context--socket-state)
               (lambda (_path) 'stale))
              ((symbol-function 'codex-ide-context--socket-file-p)
               (lambda (_path) t)))
       (codex-ide-context--prepare-socket-path directory path)
       (should-not (file-exists-p path))))))

(ert-deftest codex-ide-context-prepare-socket-refuses-nonsocket-path ()
  "A regular file at the socket path is never removed as stale."
  (codex-ide-context-test--call-with-socket-path
   (lambda (directory path)
     (write-region "" nil path nil 'silent)
     (cl-letf (((symbol-function 'codex-ide-context--socket-state)
                (lambda (_path) 'stale)))
       (should-error
        (codex-ide-context--prepare-socket-path directory path)
        :type 'user-error)
       (should (file-exists-p path))))))

(ert-deftest codex-ide-context-prepare-socket-refuses-unknown-socket-state ()
  "Ambiguous socket probe errors are not treated as stale."
  (codex-ide-context-test--call-with-socket-path
   (lambda (directory path)
     (write-region "" nil path nil 'silent)
     (cl-letf (((symbol-function 'codex-ide-context--socket-state)
               (lambda (_path) 'unknown))
               ((symbol-function 'delete-file)
                (lambda (_file)
                  (error "should not delete unknown socket"))))
       (should-error
        (codex-ide-context--prepare-socket-path directory path)
        :type 'user-error)
       (should (file-exists-p path))))))

;;; Socket path

(ert-deftest codex-ide-context-socket-path-shape ()
  "Socket path has the documented ipc-<uid>.sock basename."
  (let ((codex-ide-context-socket-directory "/tmp/codex-ipc"))
    (should (string-suffix-p
             (format "ipc-%d.sock" (user-uid))
             (codex-ide-context--socket-path)))))

(provide 'codex-ide-context-tests)

;;; codex-ide-context-tests.el ends here
