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

(defun codex-ide-context-test--request (request-id method params)
  "Build a request plist matching the Codex wire shape."
  (append (list :type "request"
                :requestId request-id
                :sourceClientId "codex-tui"
                :version 0
                :method method)
          (when params (list :params params))))

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
  "Success response carries requestId, resultType, and result.ideContext."
  (let ((resp (codex-ide-context--success-response
               "req-1" '((activeFile)))))
    (should (equal (cdr (assoc "type" resp)) "response"))
    (should (equal (cdr (assoc "requestId" resp)) "req-1"))
    (should (equal (cdr (assoc "resultType" resp)) "success"))
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
                      (should (assoc "path" desc)))))
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

;;; Socket path

(ert-deftest codex-ide-context-socket-path-shape ()
  "Socket path has the documented ipc-<uid>.sock basename."
  (let ((codex-ide-context-socket-directory "/tmp/codex-ipc"))
    (should (string-suffix-p
             (format "ipc-%d.sock" (user-uid))
             (codex-ide-context--socket-path)))))

(provide 'codex-ide-context-tests)

;;; codex-ide-context-tests.el ends here
