;;; codex-ide-context.el --- Codex IDE context IPC provider  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, codex, tools, ide
;; URL: https://git.thanosapollo.org/emacs-codex

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

;; Codex IDE context IPC provider.  Emacs listens on a Unix socket and
;; answers `ide-context' requests from the Codex TUI (`/ide' command) so
;; Codex can read the active file, selection, and open tabs of this Emacs
;; session.
;;
;; The protocol is binary length-prefixed JSON: a little-endian u32 length
;; prefix followed by a UTF-8 JSON payload.  See the Codex TUI source
;; (`ide_context/ipc.rs') for the authoritative frame format.
;;
;; Usage:
;;   M-x codex-ide-context-start   Start the IPC provider
;;   M-x codex-ide-context-stop    Stop the IPC provider
;;   M-x codex-ide-context-status  Report provider state

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'codex-ide-debug)

;;; Customization

(defcustom codex-ide-context-socket-directory
  "/tmp/codex-ipc"
  "Directory holding the Codex IDE context Unix socket.
The socket is created as `ipc-<uid>.sock' inside this directory.
Defaults to \"/tmp/codex-ipc\", matching the path the Codex TUI connects
to by default.  The directory is created with mode 0700 before the
socket is opened."
  :type 'directory
  :group 'codex-ide)

(defcustom codex-ide-context-max-frame-size (* 256 1024 1024)
  "Maximum accepted IPC frame size in bytes.
Frames larger than this are rejected before parsing."
  :type 'integer
  :group 'codex-ide)

(defcustom codex-ide-context-open-tabs-limit 100
  "Maximum number of open tabs serialized in a context response."
  :type 'integer
  :group 'codex-ide)

(defcustom codex-ide-context-selection-content-limit 65536
  "Maximum bytes of selected text included as `activeSelectionContent'."
  :type 'integer
  :group 'codex-ide)

;;; Constants

(defconst codex-ide-context--source-client-id "codex-emacs"
  "Client identifier Emacs advertises in discovery responses.")

(defconst codex-ide-context--supported-version 0
  "IDE context protocol version Emacs implements.")

;;; Variables

(defvar codex-ide-context--server nil
  "The listening server process, or nil when stopped.")

(defvar codex-ide-context--clients (make-hash-table :test 'eq)
  "Hash table mapping client processes to their receive accumulators.
Each value is a plist with `:pending' (unibyte string buffer) and
`:length' (expected payload length once the header arrived, or nil).")

(define-error 'codex-ide-context-frame-too-large
  "Codex IDE context frame too large")

;;; Frame codec (pure)

(defun codex-ide-context--u32-le-bytes (n)
  "Return a 4-byte unibyte string encoding N as a little-endian u32."
  (unibyte-string (logand n 255)
                  (logand (ash n -8) 255)
                  (logand (ash n -16) 255)
                  (logand (ash n -24) 255)))

(defun codex-ide-context--decode-length (bytes)
  "Decode a little-endian u32 from the first 4 bytes of BYTES.
BYTES must be at least 4 bytes long."
  (+ (aref bytes 0)
     (ash (aref bytes 1) 8)
     (ash (aref bytes 2) 16)
     (ash (aref bytes 3) 24)))

(defun codex-ide-context--encode-frame (message)
  "Return a unibyte string frame encoding MESSAGE.
MESSAGE is a plist or alist suitable for `json-encode'.  The frame is
a little-endian u32 length prefix followed by the UTF-8 JSON payload."
  (let* ((payload (encode-coding-string (json-encode message) 'utf-8))
         (length (length payload)))
    (concat (codex-ide-context--u32-le-bytes length) payload)))

(defun codex-ide-context--decode-payload (bytes)
  "Decode BYTES (a unibyte string) as JSON into a plist."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-false nil)
        (json-null nil))
    (json-read-from-string
     (decode-coding-string bytes 'utf-8))))

;;; Protocol builders (pure)

(defun codex-ide-context--success-response (request-id ide-context)
  "Build a success response alist for REQUEST-ID wrapping IDE-CONTEXT."
  (list (cons "type" "response")
        (cons "requestId" request-id)
        (cons "resultType" "success")
        (cons "result"
              (list (cons "ideContext" ide-context)))))

(defun codex-ide-context--error-response (request-id error)
  "Build an error response alist for REQUEST-ID with ERROR string."
  (list (cons "type" "response")
        (cons "requestId" request-id)
        (cons "resultType" "error")
        (cons "error" error)))

(defun codex-ide-context--discovery-response (request-id can-handle)
  "Build a client-discovery response alist for REQUEST-ID.
CAN-HANDLE is non-nil when Emacs can serve `/ide' requests."
  (list (cons "type" "client-discovery-response")
        (cons "requestId" request-id)
        (cons "response"
              (list (cons "canHandle" (if can-handle t :false))))))

(defun codex-ide-context--unsupported-response (message)
  "Build an error response for an unsupported inbound request MESSAGE."
  (codex-ide-context--error-response
   (plist-get message :requestId)
   "no-handler-for-request"))

;;; Dispatch (pure)

(defun codex-ide-context--handle-message (message workspace-root)
  "Dispatch on MESSAGE plist, returning a response alist or nil.
WORKSPACE-ROOT is the root to scope context collection to.  Returning nil
means the message needs no reply (broadcast, stray response, etc.)."
  (pcase (plist-get message :type)
    ("request"
     (if (equal (plist-get message :method) "ide-context")
         (codex-ide-context--success-response
          (plist-get message :requestId)
          (codex-ide-context--collect workspace-root))
       (codex-ide-context--unsupported-response message)))
    ("client-discovery-request"
     (codex-ide-context--discovery-response
      (plist-get message :requestId) t))
    ((or "broadcast" "response" "client-discovery-response") nil)
    (_ nil)))

;;; Context serialization

(defun codex-ide-context--line-character (pos)
  "Return ((line . L) (character . C)) for buffer position POS.
Line and character are zero-based, matching the Codex/LSP convention."
  (let ((line (1- (line-number-at-pos pos)))
        (character (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char pos)
                       (- (point) (line-beginning-position))))))
    (list (cons "line" line)
          (cons "character" character))))

(defun codex-ide-context--region->range (beg end)
  "Return a Codex range alist for buffer region BEG..END.
Range shape: ((start . pos) (end . pos)) where each pos is a
line/character alist.  Collapsed regions produce start==end."
  (list (cons "start" (codex-ide-context--line-character beg))
        (cons "end" (codex-ide-context--line-character end))))

(defun codex-ide-context--relative-path (path workspace-root)
  "Return PATH made relative to WORKSPACE-ROOT when possible.
Falls back to the absolute PATH when WORKSPACE-ROOT is nil or PATH is
outside it."
  (let ((file (and (file-name-absolute-p path)
                   (file-truename path)))
        (root (and workspace-root
                   (file-name-as-directory
                    (file-truename workspace-root)))))
    (if (and file root (string-prefix-p root file))
        (file-relative-name path workspace-root)
      path)))

(defun codex-ide-context--buffer->file-descriptor (buffer workspace-root)
  "Return a ((label . L) (path . P)) alist for BUFFER, or nil.
Returns nil when BUFFER is not visiting a file.  PATH is relative to
WORKSPACE-ROOT when the file lives under it."
  (when-let ((file (buffer-file-name buffer)))
    (let ((rel (codex-ide-context--relative-path file workspace-root)))
      (list (cons "label" (file-name-nondirectory file))
            (cons "path" rel)))))

(defun codex-ide-context--selected-buffer ()
  "Return the buffer currently selected by the active Emacs window."
  (window-buffer (selected-window)))

(defun codex-ide-context--active-file (workspace-root &optional buffer)
  "Return the activeFile alist for BUFFER (default `current-buffer').
WORKSPACE-ROOT is the project root used to compute the relative
file path.  Includes selection and `activeSelectionContent' when a
region is active.  Returns nil when BUFFER is not visiting a file."
  (let* ((buf (or buffer (current-buffer)))
         (descriptor (and (or (not workspace-root)
                              (codex-ide-context--buffer-under-root-p
                               buf workspace-root))
                          (codex-ide-context--buffer->file-descriptor
                           buf workspace-root))))
    (when descriptor
      (if-let (((buffer-live-p buf))
               (beg (with-current-buffer buf
                      (and (region-active-p) (region-beginning))))
               (end (with-current-buffer buf
                      (and (region-active-p) (region-end)))))
          (let* ((content (with-current-buffer buf
                            (buffer-substring-no-properties beg end)))
                 (trimmed (if (> (length content)
                                 codex-ide-context-selection-content-limit)
                              (substring content 0
                                         codex-ide-context-selection-content-limit)
                            content))
                 (range (with-current-buffer buf
                          (codex-ide-context--region->range beg end))))
            (append descriptor
                    (list (cons "selection" range)
                          (cons "activeSelectionContent" trimmed)
                          (cons "selections"
                                (list range)))))
        (let* ((pos (with-current-buffer buf (point)))
               (point-range (with-current-buffer buf
                              (codex-ide-context--region->range pos pos))))
          (append descriptor
                  (list (cons "selection" point-range)
                        (cons "activeSelectionContent" "")
                        (cons "selections" []))))))))

(defun codex-ide-context--buffer-under-root-p (buffer root)
  "Return non-nil when BUFFER visits a file under ROOT."
  (when-let ((file (buffer-file-name buffer)))
    (string-prefix-p (file-name-as-directory (file-truename root))
                     (file-truename file))))

(defun codex-ide-context--open-tabs (workspace-root)
  "Return a list of file-descriptor alists for open file buffers.
When WORKSPACE-ROOT is non-nil, only buffers visiting files under that
root are included.  Without WORKSPACE-ROOT, all file-visiting buffers
are included.  The list is capped at
`codex-ide-context-open-tabs-limit' entries and deduplicated by path."
  (let* ((root (and workspace-root (file-truename workspace-root)))
         (file-buffers (cl-remove-if-not #'buffer-file-name (buffer-list)))
         (root-buffers (and root
                            (cl-remove-if-not
                             (lambda (buf)
                               (codex-ide-context--buffer-under-root-p
                                buf root))
                             file-buffers)))
         (chosen (if root root-buffers file-buffers))
         (seen (make-hash-table :test 'equal))
         (deduped nil))
    (dolist (buf chosen)
      (when-let ((desc (codex-ide-context--buffer->file-descriptor
                        buf workspace-root))
                 (path (cdr (assoc "path" desc))))
        (unless (gethash path seen)
          (puthash path t seen)
          (push desc deduped))))
    (let ((tabs (nreverse deduped)))
      (cl-subseq tabs 0
                 (min (length tabs) codex-ide-context-open-tabs-limit)))))

(defun codex-ide-context--collect (workspace-root &optional buffer)
  "Build the `ideContext' alist for WORKSPACE-ROOT.
BUFFER (default selected window buffer) is the active buffer to serialize."
  (let ((active (codex-ide-context--active-file
                 workspace-root
                 (or buffer (codex-ide-context--selected-buffer)))))
    (delq nil
          (list (when active (cons "activeFile" active))
                (cons "openTabs" (vconcat (codex-ide-context--open-tabs
                                            workspace-root)))))))

;;; Socket / process (boundary)

(defun codex-ide-context--socket-path ()
  "Return the absolute path of the IPC socket."
  (expand-file-name (format "ipc-%d.sock" (user-uid))
                    codex-ide-context-socket-directory))

(defun codex-ide-context--ensure-socket-directory (directory)
  "Create DIRECTORY with safe permissions for the IPC socket.
Codex requires the parent directory to be owned by the current user and
not writable by group or other users."
  (make-directory directory t)
  (set-file-modes directory #o700))

(defun codex-ide-context--running-p ()
  "Return non-nil when the IPC server is listening."
  (and codex-ide-context--server
       (process-live-p codex-ide-context--server)))

(defun codex-ide-context--start-server ()
  "Start the IPC server on the Codex IDE context socket.
Returns the server process.  Signals an error when the socket directory
cannot be secured or the server is already running."
  (when (codex-ide-context--running-p)
    (user-error "Codex IDE context provider is already running"))
  (let ((dir codex-ide-context-socket-directory)
        (path (codex-ide-context--socket-path)))
    (codex-ide-context--ensure-socket-directory dir)
    (when (file-exists-p path)
      (delete-file path))
    (let ((server (make-network-process
                   :name "codex-ide-context"
                   :buffer nil
                   :family 'local
                   :service path
                   :server t
                   :noquery t
                   :filter #'codex-ide-context--filter
                   :sentinel #'codex-ide-context--sentinel)))
      (setq codex-ide-context--server server)
      (set-file-modes path #o600)
      (codex-ide-debug "Codex IPC listening on %s" path)
      server)))

(defun codex-ide-context--stop-server ()
  "Stop the IPC server and release the socket."
  (when codex-ide-context--server
    (ignore-errors (delete-process codex-ide-context--server))
    (setq codex-ide-context--server nil))
  (clrhash codex-ide-context--clients)
  (when-let ((path (codex-ide-context--socket-path)))
    (when (file-exists-p path)
      (ignore-errors (delete-file path))))
  (codex-ide-debug "Codex IPC provider stopped"))

(defun codex-ide-context--client-state (proc)
  "Return the accumulator plist for client PROC, creating it if needed."
  (or (gethash proc codex-ide-context--clients)
      (let ((state (list :pending (unibyte-string)
                         :length nil)))
        (puthash proc state codex-ide-context--clients)
        state)))

(defun codex-ide-context--parse-frames (pending length)
  "Extract complete frames from a connection accumulator.
PENDING is the accumulated unibyte string; LENGTH is the expected payload
length once the 4-byte header has arrived, or nil.  Returns a list of
decoded message plists plus a cons (REMAINING-PENDING . REMAINING-LENGTH)
for the unparsed tail.  Signals `codex-ide-context-frame-too-large' when
a declared payload length exceeds the configured maximum."
  (let ((messages nil)
        (buf pending)
        (len length))
    (catch 'incomplete
      (while t
        ;; Read the 4-byte length header if not yet known.
        (unless len
          (if (< (length buf) 4)
              (throw 'incomplete nil))
          (setq len (codex-ide-context--decode-length
                     (substring buf 0 4))
                buf (substring buf 4)))
        (when (> len codex-ide-context-max-frame-size)
          (signal 'codex-ide-context-frame-too-large (list len)))
        ;; Wait for the full payload.
        (if (< (length buf) len)
            (throw 'incomplete nil))
        (push (codex-ide-context--decode-payload
               (substring buf 0 len))
              messages)
        (setq buf (substring buf len)
              len nil)))
    (cons (nreverse messages) (cons buf len))))

(defun codex-ide-context--filter (proc string)
  "Process filter: accumulate STRING for PROC and dispatch complete frames."
  (let* ((state (codex-ide-context--client-state proc))
         (pending (concat (plist-get state :pending)
                          (string-to-unibyte string)))
         (len (plist-get state :length)))
    (pcase-let ((`(,messages . (,tail . ,tail-len))
                 (condition-case err
                     (codex-ide-context--parse-frames pending len)
                   (codex-ide-context-frame-too-large
                    (codex-ide-debug "Codex IPC frame too large, dropping client")
                    (ignore-errors (delete-process proc))
                    nil)
                   (error
                    (codex-ide-debug "Codex IPC parse error: %S" err)
                    (ignore-errors (delete-process proc))
                    nil))))
      (if messages
          (progn
            (plist-put state :pending tail)
            (plist-put state :length tail-len)
            (dolist (msg messages)
              (codex-ide-context--handle-connection-message proc msg)))
        (progn
          (plist-put state :pending pending)
          (plist-put state :length len))))))

(defun codex-ide-context--handle-connection-message (proc message)
  "Handle one decoded MESSAGE from client PROC."
  (let* ((workspace-root (plist-get (plist-get message :params)
                                    :workspaceRoot))
         (response (codex-ide-context--handle-message
                    message workspace-root)))
    (when response
      (process-send-string
       proc (codex-ide-context--encode-frame response)))))

(defun codex-ide-context--sentinel (proc event)
  "Sentinel: clean up client PROC on EVENT."
  (codex-ide-debug "Codex IPC client event: %s" (string-trim event))
  (unless (process-live-p proc)
    (remhash proc codex-ide-context--clients)))

;;; Commands

;;;###autoload
(defun codex-ide-context-start ()
  "Start the Codex IDE context IPC provider."
  (interactive)
  (codex-ide-context--start-server)
  (codex-ide-log "Codex IDE context provider started on %s"
                 (codex-ide-context--socket-path)))

;;;###autoload
(defun codex-ide-context-stop ()
  "Stop the Codex IDE context IPC provider."
  (interactive)
  (codex-ide-context--stop-server)
  (codex-ide-log "Codex IDE context provider stopped"))

;;;###autoload
(defun codex-ide-context-status ()
  "Report whether the Codex IDE context provider is running."
  (interactive)
  (if (codex-ide-context--running-p)
      (codex-ide-log "Codex IDE context provider is running on %s"
                     (codex-ide-context--socket-path))
    (codex-ide-log "Codex IDE context provider is not running")))

(provide 'codex-ide-context)

;;; codex-ide-context.el ends here
