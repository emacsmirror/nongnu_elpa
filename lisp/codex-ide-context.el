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
;;   M-x codex-ide-context-mode    Toggle the IPC provider
;;   M-x codex-ide-send-selection  Push current selection context

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
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
  "Maximum characters of selected text included as `activeSelectionContent'."
  :type 'integer
  :group 'codex-ide)

(defcustom codex-ide-context-auto-start t
  "When non-nil, start the IDE context provider for new Codex sessions."
  :type 'boolean
  :group 'codex-ide)

;;; Constants

(defconst codex-ide-context--source-client-id "codex-emacs"
  "Client identifier Emacs advertises in discovery responses.")

(defconst codex-ide-context--supported-version 0
  "IDE context protocol version Emacs implements.")

;;; Variables

(defvar codex-ide-context--server nil
  "The listening server process, or nil when stopped.")

(defvar codex-ide-context--owned-socket-path nil
  "Socket path owned by the current Emacs provider instance.")

(defvar codex-ide-context--owned-socket-identity nil
  "File identity for the socket owned by this provider instance.")

(defvar codex-ide-context--clients (make-hash-table :test 'eq)
  "Hash table mapping client processes to their receive accumulators.
Each value is a plist with `:pending' (unibyte string buffer) and
`:length' (expected payload length once the header arrived, or nil).")

(defvar codex-ide-context--source-buffers (make-hash-table :test 'equal)
  "Hash table mapping project roots to the latest source buffer.")

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
        (json-false :json-false)
        (json-null nil))
    (json-read-from-string
     (decode-coding-string bytes 'utf-8))))

;;; Protocol builders (pure)

(defun codex-ide-context--success-response (request-id ide-context)
  "Build a success response alist for REQUEST-ID wrapping IDE-CONTEXT."
  (list (cons "type" "response")
        (cons "requestId" request-id)
        (cons "resultType" "success")
        (cons "method" "ide-context")
        (cons "handledByClientId" codex-ide-context--source-client-id)
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
              (list (cons "canHandle" (if can-handle t :json-false))))))

(defun codex-ide-context--unsupported-response (message)
  "Build an error response for an unsupported inbound request MESSAGE."
  (codex-ide-context--error-response
   (plist-get message :requestId)
   "no-handler-for-request"))

(defun codex-ide-context--version-mismatch-response (message)
  "Build a version-mismatch error response for inbound request MESSAGE."
  (codex-ide-context--error-response
   (plist-get message :requestId)
   "request-version-mismatch"))

(defun codex-ide-context--request-supported-p (message)
  "Return non-nil when MESSAGE is an IDE context request Emacs supports."
  (and (equal (plist-get message :method) "ide-context")
       (equal (plist-get message :version)
              codex-ide-context--supported-version)))

(defun codex-ide-context--discovery-can-handle-p (message)
  "Return non-nil when discovery MESSAGE describes a supported request."
  (if-let* ((request (plist-get message :request)))
      (codex-ide-context--request-supported-p request)
    t))

;;; Dispatch (pure)

(defun codex-ide-context--handle-message (message workspace-root)
  "Dispatch on MESSAGE plist, returning a response alist or nil.
WORKSPACE-ROOT is the root to scope context collection to.  Returning nil
means the message needs no reply (broadcast, stray response, etc.)."
  (pcase (plist-get message :type)
    ("request"
     (cond
      ((not (equal (plist-get message :method) "ide-context"))
       (codex-ide-context--unsupported-response message))
      ((not (equal (plist-get message :version)
                   codex-ide-context--supported-version))
       (codex-ide-context--version-mismatch-response message))
      (t
       (codex-ide-context--success-response
        (plist-get message :requestId)
        (codex-ide-context--collect
         workspace-root
         (codex-ide-context--resolve-source-buffer workspace-root))))))
    ("client-discovery-request"
     (codex-ide-context--discovery-response
      (plist-get message :requestId)
      (codex-ide-context--discovery-can-handle-p message)))
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
  "Return a Codex file descriptor alist for BUFFER, or nil.
Returns nil when BUFFER is not visiting a file.  PATH is relative to
WORKSPACE-ROOT when the file lives under it.  FSPATH is always absolute."
  (and-let* ((file (buffer-file-name buffer)))
    (let ((rel (codex-ide-context--relative-path file workspace-root))
          (abs (expand-file-name file)))
      (list (cons "label" (file-name-nondirectory file))
            (cons "path" rel)
            (cons "fsPath" abs)))))

(defun codex-ide-context--selected-buffer ()
  "Return the buffer currently selected by the active Emacs window."
  (window-buffer (selected-window)))

(defun codex-ide-context--normalize-root (root)
  "Return ROOT as an expanded directory name, or nil."
  (and root (file-name-as-directory (expand-file-name root))))

(defun codex-ide-context--buffer-project-root (buffer)
  "Return BUFFER's project root, or nil when BUFFER is not a file buffer."
  (when (and (buffer-live-p buffer) (buffer-file-name buffer))
    (with-current-buffer buffer
      (codex-ide-context--normalize-root
       (if-let* ((project (project-current nil)))
           (project-root project)
         default-directory)))))

(defun codex-ide-context--source-buffer-p (buffer workspace-root)
  "Return non-nil when BUFFER is a live file buffer under WORKSPACE-ROOT."
  (and (buffer-live-p buffer)
       (buffer-file-name buffer)
       (or (not workspace-root)
           (codex-ide-context--buffer-under-root-p buffer workspace-root))))

(defun codex-ide-context-record-source-buffer (&optional workspace-root buffer)
  "Record BUFFER as the latest source buffer for WORKSPACE-ROOT.
When BUFFER is nil, use the selected window buffer.  When
WORKSPACE-ROOT is nil, derive it from BUFFER's project.  Return the
recorded buffer, or nil when BUFFER is not a project file buffer."
  (let* ((buf (or buffer (codex-ide-context--selected-buffer)))
         (root (codex-ide-context--normalize-root
                (or workspace-root
                    (codex-ide-context--buffer-project-root buf)))))
    (when (and root (codex-ide-context--source-buffer-p buf root))
      (puthash root buf codex-ide-context--source-buffers)
      buf)))

(defun codex-ide-context--record-window-selection (&rest _args)
  "Record the selected file buffer after a window-selection change."
  (codex-ide-context-record-source-buffer))

(defun codex-ide-context--tracked-source-buffer (workspace-root)
  "Return the tracked live source buffer for WORKSPACE-ROOT, or nil."
  (let* ((root (codex-ide-context--normalize-root workspace-root))
         (buffer (and root (gethash root codex-ide-context--source-buffers))))
    (and (codex-ide-context--source-buffer-p buffer root) buffer)))

(defun codex-ide-context--resolve-source-buffer (workspace-root)
  "Return the best source buffer for WORKSPACE-ROOT.
The selected project file wins.  If the selected buffer is not a project
file, fall back to the latest tracked live project file."
  (let* ((root (codex-ide-context--normalize-root workspace-root))
         (selected (codex-ide-context--selected-buffer)))
    (or (and (codex-ide-context--source-buffer-p selected root) selected)
        (codex-ide-context--tracked-source-buffer root))))

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
      (if-let* (((buffer-live-p buf))
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
  (and-let* ((file (buffer-file-name buffer)))
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
      (when-let* ((desc (codex-ide-context--buffer->file-descriptor
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
BUFFER is the active buffer to serialize."
  (let ((active (and buffer
                     (codex-ide-context--active-file workspace-root buffer))))
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
  (unless (equal (nth 2 (file-attributes directory 'integer))
                 (user-uid))
    (user-error "Codex IPC directory is not owned by the current user: %s"
                directory))
  (set-file-modes directory #o700)
  (unless (codex-ide-context--owned-private-directory-p directory)
    (user-error "Codex IPC directory is not private: %s" directory)))

(defun codex-ide-context--owned-private-directory-p (directory)
  "Return non-nil when DIRECTORY is owned by this user and mode 0700."
  (and-let* ((attrs (file-attributes directory 'integer))
             (modes (file-modes directory)))
    (and (eq (car attrs) t)
         (equal (nth 2 attrs) (user-uid))
         (= (logand modes #o777) #o700))))

(defun codex-ide-context--socket-file-p (path)
  "Return non-nil when PATH's file type is a Unix socket."
  (and-let* ((attrs (file-attributes path 'integer))
             (modes (file-attribute-modes attrs)))
    (and (> (length modes) 0)
         (eq (aref modes 0) ?s))))

(defun codex-ide-context--socket-identity (path)
  "Return the inode and device identity for PATH, or nil."
  (and-let* ((attrs (file-attributes path 'integer)))
    (list (file-attribute-inode-number attrs)
          (file-attribute-device-number attrs))))

(defun codex-ide-context--running-p ()
  "Return non-nil when the IPC server is listening."
  (and codex-ide-context--server
       (process-live-p codex-ide-context--server)
       (equal codex-ide-context--owned-socket-path
              (codex-ide-context--socket-path))
       codex-ide-context--owned-socket-identity
       (equal codex-ide-context--owned-socket-identity
              (codex-ide-context--socket-identity
               codex-ide-context--owned-socket-path))))

(defun codex-ide-context--delete-server-process ()
  "Delete the live IPC server process without touching socket files."
  (when codex-ide-context--server
    (ignore-errors (delete-process codex-ide-context--server))
    (setq codex-ide-context--server nil)))

(defun codex-ide-context--stale-socket-error-p (error)
  "Return non-nil when ERROR means PATH is a stale socket."
  (string-match-p
   (regexp-opt '("Connection refused" "No such file or directory"))
   (error-message-string error)))

(defun codex-ide-context--socket-state (path)
  "Return `live', `stale', or `unknown' for the socket at PATH."
  (let (probe)
    (unwind-protect
        (condition-case err
            (progn
              (setq probe
                    (make-network-process
                     :name "codex-ide-context-probe"
                     :buffer nil
                     :family 'local
                     :service path
                     :noquery t
                     :coding 'binary))
              (if (process-live-p probe) 'live 'unknown))
          (file-error
           (if (codex-ide-context--stale-socket-error-p err)
               'stale
             'unknown))
          (error 'unknown))
      (when (process-live-p probe)
        (delete-process probe)))))

(defun codex-ide-context--prepare-socket-path (directory path)
  "Prepare PATH in DIRECTORY before binding the IPC server."
  (when (file-exists-p path)
    (pcase (codex-ide-context--socket-state path)
      ('live
       (user-error "Another Codex IDE provider is using %s" path))
      ('stale
       (if (and (codex-ide-context--owned-private-directory-p directory)
                (codex-ide-context--socket-file-p path))
           (delete-file path)
         (user-error "Refusing to remove non-stale Codex IPC path: %s"
                     path)))
      (_
       (user-error "Cannot determine whether Codex IPC socket is stale: %s"
                   path)))))

(defun codex-ide-context--start-server ()
  "Start the IPC server on the Codex IDE context socket.
Return the server process.  Signals an error when the socket directory
cannot be secured or another live provider owns the socket."
  (if (codex-ide-context--running-p)
      codex-ide-context--server
    (let ((dir codex-ide-context-socket-directory)
          (path (codex-ide-context--socket-path)))
      (codex-ide-context--delete-server-process)
      (codex-ide-context--ensure-socket-directory dir)
      (codex-ide-context--prepare-socket-path dir path)
      (let ((server (make-network-process
                     :name "codex-ide-context"
                     :buffer nil
                     :family 'local
                     :service path
                     :server t
                     :noquery t
                     :coding 'binary
                     :filter #'codex-ide-context--filter
                     :sentinel #'codex-ide-context--sentinel)))
        (setq codex-ide-context--server server
              codex-ide-context--owned-socket-path path
              codex-ide-context--owned-socket-identity
              (codex-ide-context--socket-identity path))
        (set-file-modes path #o600)
        (codex-ide-debug "Codex IPC listening on %s" path)
        server))))

(defun codex-ide-context--delete-clients ()
  "Delete live IPC client processes and clear client state."
  (maphash
   (lambda (proc _state)
     (when (process-live-p proc)
       (ignore-errors (delete-process proc))))
   codex-ide-context--clients)
  (clrhash codex-ide-context--clients))

(defun codex-ide-context--delete-owned-socket ()
  "Delete the socket file owned by this provider instance."
  (when-let* ((path codex-ide-context--owned-socket-path)
              (identity codex-ide-context--owned-socket-identity))
    (when (and (equal path (codex-ide-context--socket-path))
               (file-exists-p path)
               (equal identity
                      (codex-ide-context--socket-identity path))
               (codex-ide-context--socket-file-p path))
      (ignore-errors (delete-file path))))
  (setq codex-ide-context--owned-socket-path nil
        codex-ide-context--owned-socket-identity nil))

(defun codex-ide-context--stop-server ()
  "Stop the IPC server and release the socket."
  (codex-ide-context--delete-clients)
  (codex-ide-context--delete-server-process)
  (codex-ide-context--delete-owned-socket)
  (codex-ide-debug "Codex IPC provider stopped"))

(defun codex-ide-context--client-state (proc)
  "Return the accumulator plist for client PROC, creating it if needed."
  (set-process-coding-system proc 'binary 'binary)
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

(defun codex-ide-context--broadcast (message)
  "Send MESSAGE as an IPC frame to all connected Codex clients.
Return the number of live clients the frame was sent to."
  (let ((frame (codex-ide-context--encode-frame message))
        (sent 0))
    (maphash
     (lambda (proc _state)
       (when (process-live-p proc)
         (when (ignore-errors
                 (process-send-string proc frame)
                 t)
           (setq sent (1+ sent)))))
     codex-ide-context--clients)
    sent))

(defun codex-ide-context--selection-broadcast (workspace-root &optional buffer)
  "Broadcast the current IDE context for WORKSPACE-ROOT.
BUFFER defaults to the selected window buffer.  Return the number of
clients reached."
  (codex-ide-context--broadcast
   (list (cons "type" "broadcast")
         (cons "method" "ide-context")
         (cons "params"
               (list (cons "ideContext"
                           (codex-ide-context--collect workspace-root
                                                       (or buffer
                                                           (codex-ide-context--selected-buffer)))))))))

(defun codex-ide-context--sentinel (proc event)
  "Sentinel: clean up client PROC on EVENT."
  (codex-ide-debug "Codex IPC client event: %s" (string-trim event))
  (when (eq proc codex-ide-context--server)
    (setq codex-ide-context--server nil))
  (unless (process-live-p proc)
    (remhash proc codex-ide-context--clients)))

;;; Commands

(defun codex-ide-context--install-tracking ()
  "Install source-buffer tracking hooks for the provider."
  (add-hook 'window-selection-change-functions
            #'codex-ide-context--record-window-selection)
  (add-hook 'kill-emacs-hook
            #'codex-ide-context--cleanup-on-exit)
  (codex-ide-context-record-source-buffer))

(defun codex-ide-context--uninstall-tracking ()
  "Remove source-buffer tracking hooks for the provider."
  (remove-hook 'window-selection-change-functions
               #'codex-ide-context--record-window-selection)
  (remove-hook 'kill-emacs-hook
               #'codex-ide-context--cleanup-on-exit))

(defun codex-ide-context--enable ()
  "Enable the Codex IDE context provider."
  (codex-ide-context--start-server)
  (codex-ide-context--install-tracking))

(defun codex-ide-context--disable ()
  "Disable the Codex IDE context provider."
  (codex-ide-context--uninstall-tracking)
  (codex-ide-context--stop-server)
  (clrhash codex-ide-context--source-buffers))

(defun codex-ide-context--cleanup-on-exit ()
  "Clean up the IPC provider while Emacs is exiting."
  (codex-ide-context--disable))

;;;###autoload
(define-minor-mode codex-ide-context-mode
  "Toggle the global Codex IDE context provider."
  :global t
  :group 'codex-ide
  (if codex-ide-context-mode
      (condition-case err
          (codex-ide-context--enable)
        (error
         (setq codex-ide-context-mode nil)
         (codex-ide-context--disable)
         (signal (car err) (cdr err))))
    (codex-ide-context--disable)))

(defun codex-ide-context-ensure-server ()
  "Ensure the Codex IDE context IPC provider is running.
Return the listening process."
  (codex-ide-context-mode 1)
  codex-ide-context--server)

;;;###autoload
(defun codex-ide-context-start ()
  "Start the Codex IDE context IPC provider."
  (interactive)
  (codex-ide-context-mode 1)
  (codex-ide-log "Codex IDE context provider started on %s"
                 (codex-ide-context--socket-path)))

;;;###autoload
(defun codex-ide-context-stop ()
  "Stop the Codex IDE context IPC provider."
  (interactive)
  (codex-ide-context-mode -1)
  (codex-ide-log "Codex IDE context provider stopped"))

;;;###autoload
(defun codex-ide-context-status ()
  "Report whether the Codex IDE context provider is running."
  (interactive)
  (if (codex-ide-context--running-p)
      (codex-ide-log "Codex IDE context provider is running on %s"
                     (codex-ide-context--socket-path))
    (codex-ide-log "Codex IDE context provider is not running")))

;;;###autoload
(defun codex-ide-send-selection (&optional workspace-root)
  "Compatibility command for pushing context to connected Codex clients.
WORKSPACE-ROOT defaults to `default-directory'.  If no client is
connected, copy the active selection to the kill ring when present and
tell the user to use `/ide' in the Codex TUI."
  (interactive)
  (let* ((buffer (current-buffer))
         (sent (codex-ide-context--selection-broadcast
                (or workspace-root default-directory)
                buffer))
         (selection (and (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning) (region-end)))))
    (if (> sent 0)
        (codex-ide-log "Sent selection context to %d Codex client(s)" sent)
      (when selection
        (kill-new selection))
      (codex-ide-log
       (if selection
           (concat "No Codex clients connected; copied selection.  "
                   "Type /ide in Codex TUI to pull context")
         "No Codex clients connected; type /ide in Codex TUI to pull context")))))

(provide 'codex-ide-context)

;;; codex-ide-context.el ends here
