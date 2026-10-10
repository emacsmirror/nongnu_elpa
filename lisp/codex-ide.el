;;; codex-ide.el --- Run Codex CLI in a terminal  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Maintainer: Thanos Apollo <public@thanosapollo.org>
;; Version: 0.1.4
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.2") (keymap-popup "0.4.0"))
;; Keywords: ai, codex, tools, terminal
;; URL: https://git.thanosapollo.org/emacs-codex-ide

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

;; Run the Codex CLI through Eat, optional vterm, or native Neomacs terminals.
;; This is a terminal-first integration: live Codex sessions are grouped by project root
;; and displayed through a configurable buffer display function, with prompt
;; sending, session cycling, and resume.
;;
;; Usage:
;;   M-x codex-ide              Start or toggle Codex for the current project
;;   C-u M-x codex-ide          Start another Codex session
;;   M-x codex-ide-resume-last  Resume the most recent Codex session
;;   M-x codex-ide-resume       Pick a saved Codex session id and resume it
;;   M-x codex-ide-new-session  Start another Codex session
;;   M-x codex-ide-toggle       Cycle project Codex sessions
;;   M-x codex-ide-send-prompt  Send a prompt from the minibuffer
;;   M-x codex-ide-stop         Stop the active Codex session for this project
;;   M-x codex-ide-list-project-sessions  Switch project Codex sessions
;;   M-x codex-ide-list-sessions  Switch to any live Codex session
;;   M-x codex-ide-menu         Popup menu of all commands

;;; Code:

(require 'compat)
(require 'cl-lib)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'codex-ide-context)
(require 'codex-ide-debug)
(require 'codex-ide-mcp)
(require 'codex-ide-term)

(declare-function codex-ide-term-vterm--settle-buffer "codex-ide-term-vterm" ())

(autoload 'codex-ide-menu "codex-ide-menu" nil t)

;;; Customization

(defgroup codex-ide nil
  "Run Codex CLI inside Emacs through a terminal backend."
  :group 'tools
  :prefix "codex-ide-")

(defcustom codex-ide-cli-path "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'codex-ide)

(defcustom codex-ide-sessions-directory
  (expand-file-name "~/.codex/sessions")
  "Directory containing Codex saved-session rollout files."
  :type 'directory
  :group 'codex-ide)

(defcustom codex-ide-resume-session-scan-limit 200
  "Maximum number of matching saved sessions offered for resume.
Non-positive values include all matches.  Files from other projects and
duplicate session IDs do not count toward this limit."
  :type 'integer
  :group 'codex-ide)

(defcustom codex-ide-display-buffer-function #'pop-to-buffer
  "Function used to display the Codex terminal buffer.
Use `display-buffer-alist' to configure placement.  Focus is controlled
by the calling command, independently of this function.
The function is called with the Codex buffer and must make it visible.
When it returns a live window, that window is used for terminal dimension
sync; otherwise Codex uses any live window showing the buffer."
  :type 'function
  :group 'codex-ide)

(defcustom codex-ide-ask-for-approval nil
  "When to ask for human approval before Codex executes commands.
When nil (default), do not pass `--ask-for-approval' so that Codex's
own `config.toml' policy decides.  Otherwise pass the chosen policy."
  :type '(choice (const :tag "Config decides (nil)" nil)
                 (const :tag "on-request" on-request)
                 (const :tag "never" never))
  :group 'codex-ide)

(defcustom codex-ide-config-overrides nil
  "Alist of Codex TOML config overrides, emitted as `-c key=value' pairs.
Keys are dotted TOML paths, values are strings.
Example: ((\"model\" . \"o3\")
          (\"sandbox_permissions\" . \"[\\\"disk-full-read-access\\\"]\"))"
  :type '(alist :key-type (string :tag "Key")
                :value-type (string :tag "Value"))
  :group 'codex-ide)

(defcustom codex-ide-cli-extra-args nil
  "Extra arguments appended verbatim to the Codex command.
Escape hatch for flags not yet modeled by a defcustom.
Native automatic MCP registration refuses profile, config, cwd and remote
options here.  Use `codex-ide-config-overrides' for session settings, select
the project directory in Emacs, or disable `codex-ide-mcp-enabled'."
  :type '(repeat string)
  :group 'codex-ide)

(defcustom codex-ide-no-alt-screen nil
  "When non-nil, pass `--no-alt-screen' for inline TUI mode."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-yolo nil
  "When non-nil, pass `--yolo' for full control without approvals or sandbox."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-buffer-name-function #'codex-ide--default-buffer-name
  "Function called with the working directory to produce a base buffer name.
Codex IDE adds a root identity when another project already uses that name."
  :type 'function
  :group 'codex-ide)

(defcustom codex-ide-kill-buffer-on-exit 'success
  "When to kill a terminal buffer after Codex exits.
Nil retains all output, t kills on every exit, and `success' kills only
on a clean exit with status zero.  Retained buffers show an exit notice."
  :type '(choice (const :tag "Keep all" nil)
                 (const :tag "Kill all" t)
                 (const :tag "Kill on success" success))
  :group 'codex-ide)

;;; Variables

(defvar codex-ide-prompt-history nil
  "Minibuffer history of prompts sent to Codex.")

(defvar codex-ide--cli-cache nil
  "Last CLI probe, with :path, :state and :version fields.")

(defvar codex-ide--rollout-cache (make-hash-table :test 'equal)
  "Rollout metadata by filename, retaining modification time and size.")

(defvar codex-ide--working-directory nil
  "Dynamically bound exact directory for an explicitly resumed session.")

(defvar codex-ide--recovery-pending t
  "Whether session recovery is needed after loading the package.")
(setq codex-ide--recovery-pending t)

(defvar codex-ide--cli-available nil
  "Whether the Codex CLI was detected.")

(defvar codex-ide--sessions (make-hash-table :test 'equal)
  "Hash table mapping project roots to live Codex session records.")

(defvar codex-ide--active-session-ids (make-hash-table :test 'equal)
  "Hash table mapping project roots to active Codex session ids.")

(defvar codex-ide--cleanup-in-progress nil
  "Cleanup targets currently being torn down, as (ROOT ID) lists.")

(defvar-local codex-ide--saved-mode-line-process nil
  "Original local binding and value of `mode-line-process' while enabled.")

(defvar-local codex-ide--session-label nil
  "Optional display label of this live session.")

(defvar-local codex-ide--session-naming-base nil
  "Unlabelled naming base retained when a session is renamed.")

(defvar-local codex-ide--session-root nil
  "Project root for the Codex session in the current buffer.")

(defvar-local codex-ide--session-id nil
  "Numeric Codex session id for the current buffer.")

(defvar-keymap codex-ide-mode-map
  :doc "Keymap for Codex terminal buffers."
  "S-<return>" #'codex-ide-insert-newline
  "C-c C-k" #'codex-ide-send-escape
  "C-c C-j" #'codex-ide-return-live)

(define-minor-mode codex-ide-mode
  "Minor mode for terminal buffers running a Codex CLI session."
  :interactive nil
  :lighter nil
  (if codex-ide-mode
      (progn
        (unless codex-ide--saved-mode-line-process
          (setq codex-ide--saved-mode-line-process
                (cons (local-variable-p 'mode-line-process) mode-line-process)))
        (setq-local mode-line-process
                    '(:eval (codex-ide--mode-line)))
        (add-hook 'kill-buffer-hook
                  #'codex-ide--cleanup-current-buffer-session nil t)
        (add-hook 'change-major-mode-hook
                  #'codex-ide--cleanup-before-major-mode-change nil t))
    (when codex-ide--saved-mode-line-process
      (if (car codex-ide--saved-mode-line-process)
          (setq-local mode-line-process (cdr codex-ide--saved-mode-line-process))
        (kill-local-variable 'mode-line-process))
      (setq codex-ide--saved-mode-line-process nil))
    (remove-hook 'kill-buffer-hook
                 #'codex-ide--cleanup-current-buffer-session t)
    (remove-hook 'change-major-mode-hook
                 #'codex-ide--cleanup-before-major-mode-change t)))

;;; Helpers (pure / mostly pure)

(defun codex-ide--get-working-directory ()
  "Return the working directory for a Codex session.
Prefers the current project root; falls back to `default-directory'."
  (expand-file-name
   (or codex-ide--working-directory
       (if-let* ((project (project-current)))
           (project-root project)
         default-directory))))

(defun codex-ide--default-buffer-name (directory)
  "Return the buffer name for DIRECTORY, as `*codex[<basename>]*'."
  (let* ((root (file-name-as-directory (expand-file-name directory)))
         (name (file-name-nondirectory (directory-file-name root))))
    (format "*codex[%s]*" name)))

(defun codex-ide--identified-buffer-name (base-name root)
  "Return BASE-NAME with a stable identity for ROOT."
  (let ((identity (substring (secure-hash 'sha1 root) 0 8)))
    (cond
     ((string-suffix-p "]*" base-name)
      (concat (substring base-name 0 -2) ":" identity "]*"))
     ((string-suffix-p "*" base-name)
      (concat (substring base-name 0 -1) ":" identity "*"))
     (t
      (format "%s:%s" base-name identity)))))

(defun codex-ide--same-root-p (left right)
  "Return non-nil when LEFT and RIGHT name the same normalized root."
  (and left right
       (equal (file-name-as-directory (expand-file-name left))
              (file-name-as-directory (expand-file-name right)))))

(defun codex-ide--buffer-name-conflict-p (name root)
  "Return non-nil when NAME is occupied by a buffer outside ROOT."
  (when-let* ((buffer (get-buffer name)))
    (let ((owner (buffer-local-value 'codex-ide--session-root buffer)))
      (not (codex-ide--same-root-p owner root)))))

(defun codex-ide--buffer-for-session (root &optional session-id)
  "Return a live session buffer for ROOT and optional SESSION-ID."
  (cl-find-if
   (lambda (buffer)
     (with-current-buffer buffer
       (and (codex-ide--same-root-p codex-ide--session-root root)
            (integerp codex-ide--session-id)
            (or (not session-id)
                (equal codex-ide--session-id session-id)))))
   (buffer-list)))

(defun codex-ide--session-base-name (buffer)
  "Return the unindexed base name established by session BUFFER."
  (with-current-buffer buffer
    (or codex-ide--session-naming-base
        (let* ((name (buffer-name buffer))
               (suffix (format "<%d>" codex-ide--session-id))
               (starred-suffix (concat suffix "*")))
	  (cond
	   ((= codex-ide--session-id 1) name)
	   ((string-suffix-p starred-suffix name)
            (concat (string-remove-suffix starred-suffix name) "*"))
	   ((string-suffix-p suffix name)
            (string-remove-suffix suffix name)))))))

(defun codex-ide--established-buffer-name (root)
  "Return the base name already used by a live session for ROOT."
  (when-let* ((buffer (codex-ide--buffer-for-session root)))
    (codex-ide--session-base-name buffer)))

(defun codex-ide--indexed-buffer-name (base-name session-id)
  "Return BASE-NAME indexed for SESSION-ID."
  (if (= session-id 1)
      base-name
    (let ((suffix (format "<%d>" session-id)))
      (if (string-suffix-p "*" base-name)
          (concat (substring base-name 0 -1) suffix "*")
        (concat base-name suffix)))))

(defun codex-ide--get-buffer-name (&optional directory session-id)
  "Return the buffer name for DIRECTORY and SESSION-ID.
DIRECTORY defaults to the current project and SESSION-ID defaults to 1.
Add a stable root identity when another project occupies the base name."
  (let* ((directory (or directory (codex-ide--get-working-directory)))
         (root (file-name-as-directory (expand-file-name directory)))
         (id (or session-id 1))
         (base (or (codex-ide--established-buffer-name root)
                   (funcall codex-ide-buffer-name-function directory)))
         (name (if (or (codex-ide--buffer-name-conflict-p base root)
                       (codex-ide--buffer-name-conflict-p
                        (codex-ide--indexed-buffer-name base id) root))
                   (codex-ide--identified-buffer-name base root)
                 base)))
    (codex-ide--indexed-buffer-name name id)))

(defun codex-ide--make-session (root id buffer process)
  "Return a Codex session record for ROOT, ID, BUFFER, and PROCESS.
PROCESS is an Emacs process or an exact native Neomacs terminal resource."
  (list :root root :id id :buffer buffer :process process :awaiting-exit nil))

(defun codex-ide--raw-sessions ()
  "Return all session records without refreshing the table."
  (let (sessions)
    (maphash (lambda (_root root-sessions)
               (setq sessions (append root-sessions sessions)))
             codex-ide--sessions)
    sessions))

(defun codex-ide--session-live-p (session)
  "Return non-nil when SESSION owns a live terminal resource."
  (let ((buffer (plist-get session :buffer))
        (process (plist-get session :process)))
    (codex-ide-term--process-live-p process buffer)))

(defun codex-ide--session-by-id (root session-id)
  "Return the session for ROOT and SESSION-ID."
  (cl-find session-id (gethash root codex-ide--sessions)
           :key (lambda (session) (plist-get session :id))
           :test #'=))

(defun codex-ide--session-by-buffer (buffer)
  "Return the registered session for BUFFER, if any."
  (cl-find buffer (codex-ide--raw-sessions)
           :key (lambda (session) (plist-get session :buffer))
           :test #'eq))

(defun codex-ide--project-sessions (root)
  "Return live Codex sessions for ROOT."
  (cl-remove-if-not #'codex-ide--session-live-p
                    (gethash root codex-ide--sessions)))

(defun codex-ide--sorted-project-sessions (root)
  "Return live Codex sessions for ROOT sorted by numeric id."
  (sort (copy-sequence (codex-ide--project-sessions root))
        (lambda (left right)
          (< (plist-get left :id) (plist-get right :id)))))

(defun codex-ide--sync-active-session (root sessions)
  "Keep ROOT's active session id valid for SESSIONS."
  (let ((active-id (gethash root codex-ide--active-session-ids)))
    (if-let* ((active (and active-id
                           (cl-find active-id sessions
                                    :key (lambda (session)
                                           (plist-get session :id))
                                    :test #'=))))
        (puthash root (plist-get active :id) codex-ide--active-session-ids)
      (if-let* ((fallback (car sessions)))
          (puthash root (plist-get fallback :id) codex-ide--active-session-ids)
        (remhash root codex-ide--active-session-ids)))))

(defun codex-ide--activate-session (session)
  "Mark SESSION as active for its project root."
  (puthash (plist-get session :root)
           (plist-get session :id)
           codex-ide--active-session-ids)
  session)

(defun codex-ide--store-session (session)
  "Store SESSION in the live session table."
  (let* ((root (plist-get session :root))
         (previous (codex-ide--session-by-id root (plist-get session :id))))
    (when (and previous (not (equal previous session)))
      (codex-ide-diff--cancel-requests (plist-get previous :buffer)
                                       (plist-get previous :process)))
    (puthash root (cons session (gethash root codex-ide--sessions))
             codex-ide--sessions)))

(defun codex-ide--remember-session (session)
  "Store SESSION and mark it active."
  (codex-ide--store-session session)
  (codex-ide--activate-session session))

(defun codex-ide--remove-session (root session-id)
  "Remove SESSION-ID from ROOT's live session table."
  (let* ((remaining
          (cl-remove-if (lambda (session)
                          (= (plist-get session :id) session-id))
                        (gethash root codex-ide--sessions))))
    (if remaining
        (puthash root remaining codex-ide--sessions)
      (remhash root codex-ide--sessions))
    (codex-ide--sync-active-session root remaining)))

(defun codex-ide--next-session-id (root)
  "Return the lowest unused live session id for ROOT."
  (let ((ids (mapcar (lambda (session) (plist-get session :id))
                     (gethash root codex-ide--sessions))))
    (cl-loop for id from 1
             unless (memq id ids)
             return id)))

(defun codex-ide--buffer-session (&optional buffer)
  "Return the Codex session owned by BUFFER, if any."
  (with-current-buffer (or buffer (current-buffer))
    (and-let* ((root codex-ide--session-root)
               (session-id codex-ide--session-id)
               (session (codex-ide--session-by-id root session-id)))
      (and (codex-ide--session-live-p session) session))))

(defun codex-ide--active-session (&optional directory)
  "Return the active live Codex session for DIRECTORY."
  (let ((root (or directory (codex-ide--get-working-directory))))
    (codex-ide--ensure-session-registry root)
    (or (and-let* ((session (codex-ide--buffer-session)))
          (and (equal root (plist-get session :root))
               (codex-ide--activate-session session)))
        (and-let* ((session-id (gethash root codex-ide--active-session-ids))
                   (session (codex-ide--session-by-id root session-id)))
          (and (codex-ide--session-live-p session) session))
        (and-let* ((session (car (codex-ide--project-sessions root))))
          (codex-ide--activate-session session)))))

(defun codex-ide--build-command (&optional resume-last session-id)
  "Return (PROGRAM . ARGS) for invoking the Codex CLI.
RESUME-LAST non-nil adds \"resume\" \"--last\".
SESSION-ID non-nil adds \"resume\" SESSION-ID and takes precedence over
RESUME-LAST when both are non-nil.  The result is always a cons; argument
folding is pure and does not touch the shell."
  (let ((args nil))
    ;; Config overrides come first, before any subcommand, matching how
    ;; `codex' parses top-level `-c' pairs.
    (dolist (pair codex-ide-config-overrides)
      (setq args (nconc args (list "-c"
                                   (format "%s=%s" (car pair) (cdr pair))))))
    (when codex-ide-yolo
      (setq args (nconc args (list "--yolo"))))
    ;; Resume subcommand (mutually exclusive shapes).
    (cond
     (session-id
      (setq args (nconc args (list "resume" session-id))))
     (resume-last
      (setq args (nconc args (list "resume" "--last")))))
    (when codex-ide-ask-for-approval
      (setq args (nconc args (list "--ask-for-approval"
                                   (symbol-name codex-ide-ask-for-approval)))))
    (when codex-ide-no-alt-screen
      (setq args (nconc args (list "--no-alt-screen"))))
    (when codex-ide-cli-extra-args
      (setq args (nconc args codex-ide-cli-extra-args)))
    (cons codex-ide-cli-path args)))

(defun codex-ide--normalize-directory (directory)
  "Return DIRECTORY as an expanded directory name with trailing slash."
  (file-name-as-directory (expand-file-name directory)))

(defun codex-ide--json-field (object name)
  "Return field NAME from JSON OBJECT alist or plist."
  (cond
   ((null object) nil)
   ((hash-table-p object)
    (or (gethash name object)
        (gethash (intern name) object)))
   ((and (listp object) (keywordp (car-safe object)))
    (plist-get object (intern (concat ":" name))))
   ((listp object)
    (or (cdr (assoc name object))
        (cdr (assq (intern name) object))))))

(defun codex-ide--rollout-first-line (file)
  "Read the first UTF-8 line from FILE, bounded to one MiB.
Signal a user error when the first line exceeds the bound."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((limit (* 1024 1024))
          (offset 0)
          end done)
      (while (not done)
        (goto-char (point-max))
        (let ((count (cadr (insert-file-contents-literally
                            file nil offset (min (+ offset 8192)
                                                 (1+ limit))))))
          (goto-char (1+ offset))
          (setq end (search-forward "\n" nil t)
                offset (+ offset count)
                done (or end (= count 0)))
          (when (> (if end (- end 2) offset) limit)
            (user-error "Rollout metadata exceeds %s bytes: %s" limit file))))
      (decode-coding-string
       (buffer-substring-no-properties (point-min)
                                       (if end (1- end) (point-max)))
       'utf-8))))

(defun codex-ide--parse-rollout-session-meta (file)
  "Return session_meta payload alist from rollout FILE, or nil.
Signal a user error when the metadata line exceeds one MiB."
  (when (and (stringp file) (file-readable-p file))
    (when-let* ((line (codex-ide--rollout-first-line file))
                (parsed (ignore-errors (json-parse-string
                                        line
                                        :object-type 'alist
                                        :array-type 'list
                                        :null-object nil
                                        :false-object nil)))
                ((equal (codex-ide--json-field parsed "type")
                        "session_meta")))
      (codex-ide--json-field parsed "payload"))))

(defun codex-ide--rollout-session-meta (file)
  "Return FILE's metadata, cached until modification time or size changes."
  (when-let* ((attrs (file-attributes file)))
    (let* ((stamp (list (file-attribute-modification-time attrs)
                        (file-attribute-size attrs)))
           (cached (gethash file codex-ide--rollout-cache)))
      (unless (equal stamp (car cached))
        (setq cached (cons stamp (codex-ide--parse-rollout-session-meta file)))
        (puthash file cached codex-ide--rollout-cache))
      (cdr cached))))

(defun codex-ide--saved-sessions ()
  "Discover saved sessions newest first, retaining IDs and recorded paths."
  (let ((seen (make-hash-table :test 'equal)) records)
    (dolist (file (codex-ide--newest-rollout-files codex-ide-sessions-directory nil))
      (when-let* ((meta (codex-ide--rollout-session-meta file))
                  (id (or (codex-ide--json-field meta "session_id")
                          (codex-ide--json-field meta "id")))
                  ((and (stringp id) (not (string-empty-p (string-trim id)))))
                  ((not (gethash id seen))))
        (puthash id t seen)
        (push (list :id id :cwd (let ((cwd (codex-ide--json-field meta "cwd")))
                                 (and (stringp cwd) (not (string-empty-p cwd)) cwd))
                    :date (or (let ((date (codex-ide--json-field meta "timestamp")))
                                (and (stringp date) date))
                              (format-time-string "%Y-%m-%d %H:%M"
                                                  (file-attribute-modification-time
                                                   (file-attributes file)))))
              records)))
    (nreverse records)))

(defun codex-ide--limit-saved-sessions (records)
  "Apply the configured resume limit to RECORDS."
  (if (and (> codex-ide-resume-session-scan-limit 0)
           (> (length records) codex-ide-resume-session-scan-limit))
      (seq-take records codex-ide-resume-session-scan-limit)
    records))

(defun codex-ide--saved-session-note (record)
  "Return the date and abbreviated path for saved RECORD."
  (concat "  " (plist-get record :date) "  "
          (if-let* ((cwd (plist-get record :cwd)))
              (abbreviate-file-name cwd)
            "(unknown directory)")))

(defun codex-ide--completion-table (candidates metadata)
  "Return a completion table for CANDIDATES with METADATA."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        (cons 'metadata metadata)
      (complete-with-action action candidates string predicate))))

(defun codex-ide--newest-rollout-files (directory limit)
  "Return up to LIMIT newest *.jsonl files under DIRECTORY.
A nil or non-positive LIMIT includes all files."
  (when (and (stringp directory) (file-directory-p directory))
    (let* ((dated (cl-loop for file in (directory-files-recursively
                                        directory "\\.jsonl\\'")
                           for attrs = (file-attributes file)
                           when attrs
                           collect (cons file
                                         (file-attribute-modification-time
                                          attrs))))
           (files (mapcar #'car (sort dated (lambda (a b)
                                             (time-less-p (cdr b) (cdr a)))))))
      (if (and (integerp limit) (> limit 0) (> (length files) limit))
          (seq-subseq files 0 limit)
        files))))

(defun codex-ide--saved-session-candidates (&optional directory)
  "Return newest saved (ID . ANNOTATION) pairs matching DIRECTORY."
  (mapcar (lambda (record)
            (cons (plist-get record :id) (codex-ide--saved-session-note record)))
          (codex-ide--limit-saved-sessions
           (seq-filter (lambda (record)
                         (or (null directory)
                             (codex-ide--same-root-p
                              directory (plist-get record :cwd))))
                       (codex-ide--saved-sessions)))))

(defun codex-ide--read-saved-session-id (&optional directory details)
  "Read a saved session ID for DIRECTORY; with DETAILS return its record."
  (let* ((root (or directory (codex-ide--get-working-directory)))
         (records (codex-ide--saved-sessions))
         (project (seq-filter (lambda (record)
                                (codex-ide--same-root-p root (plist-get record :cwd)))
                              records))
         (choices (codex-ide--limit-saved-sessions (or project records)))
         (ids (mapcar (lambda (record) (plist-get record :id)) choices))
         (table (codex-ide--completion-table
                 ids `((display-sort-function . identity)
                       (cycle-sort-function . identity)
                       (annotation-function .
                        ,(lambda (id)
                           (codex-ide--saved-session-note
                            (seq-find (lambda (record)
                                        (equal id (plist-get record :id))) choices))))))))
    (unless ids
      (user-error "No saved Codex sessions found under %s" codex-ide-sessions-directory))
    (let* ((id (completing-read (if project "Resume Codex session: "
                                 "Resume Codex session (all projects): ")
                               table nil t nil nil (car ids)))
           (record (seq-find (lambda (record) (equal id (plist-get record :id))) choices)))
      (unless record (user-error "Invalid Codex session selection"))
      (if details
          (if (stringp (plist-get record :cwd)) record
            (user-error "Saved Codex session has no working directory"))
        id))))

(defun codex-ide--session-config-overrides ()
  "Return Codex config overrides for a new session.
Includes user-provided `codex-ide-config-overrides' and any
session-local overrides needed by enabled integration helpers."
  (append codex-ide-config-overrides
          (when codex-ide-mcp-enabled
            (if (featurep 'neomacs)
                (codex-ide-mcp-neo--config-overrides
                 (codex-ide-mcp-ensure-server) codex-ide-config-overrides)
              (codex-ide-mcp-config-overrides (codex-ide-mcp-ensure-server))))))

;;; CLI detection

(defun codex-ide--detect-cli ()
  "Probe the configured executable once and cache availability and version."
  (let* ((path (or (executable-find codex-ide-cli-path) codex-ide-cli-path))
         (version nil)
         (available
          (condition-case nil
              (with-temp-buffer
                (prog1 (eq (call-process path nil t nil "--version") 0)
                  (setq version (string-trim (buffer-string)))))
            (error nil))))
    (setq codex-ide--cli-cache
          (list :path path :state (if available 'available 'unavailable)
                :version version)
          codex-ide--cli-available available)))

(defun codex-ide--invalidate-cli-cache ()
  "Forget cached CLI availability so the next check redetects."
  (setq codex-ide--cli-available nil
        codex-ide--cli-cache nil))

(defun codex-ide--ensure-cli ()
  "Return cached availability, probing when the executable path changes."
  (unless (equal (plist-get codex-ide--cli-cache :path)
                 (or (executable-find codex-ide-cli-path) codex-ide-cli-path))
    (codex-ide--detect-cli))
  codex-ide--cli-available)

;;; Process lifecycle

(defun codex-ide--cleanup-dead-sessions ()
  "Remove dead records that no longer await terminal settlement."
  (maphash (lambda (root sessions)
             (let* ((live-sessions
                     (cl-remove-if-not #'codex-ide--session-live-p sessions))
                    (owned-sessions
                     (cl-remove-if-not
                      (lambda (session)
                        (or (memq session live-sessions)
                            (and (plist-get session :awaiting-exit)
                                 (buffer-live-p (plist-get session :buffer))
                                 (with-current-buffer (plist-get session :buffer)
                                   (and codex-ide-mode
                                        (equal root codex-ide--session-root)
                                        (eql (plist-get session :id)
                                             codex-ide--session-id))))))
                      sessions)))
               ;; Backend exit hooks may query the registry before our callback.
               ;; Keep their exact owner until output and exit policy settle.
               (if owned-sessions
                   (puthash root owned-sessions codex-ide--sessions)
                 (remhash root codex-ide--sessions))
               (codex-ide--sync-active-session root live-sessions)))
           codex-ide--sessions))

(defun codex-ide--available-session-id-p (root session-id)
  "Return non-nil when SESSION-ID can be used for ROOT."
  (and (integerp session-id)
       (> session-id 0)
       (not (codex-ide--session-by-id root session-id))))

(defun codex-ide--process-command-fragments (process)
  "Return strings that may describe PROCESS's command."
  (let ((command (and (processp process) (process-command process)))
        (recorded (if (processp process)
                      (process-get process 'codex-ide--command)
                    (plist-get process :command))))
    (append (cl-remove-if-not #'stringp command)
            (cl-remove-if-not #'stringp recorded))))

(defun codex-ide--command-fragment-invokes-p (fragment program)
  "Return non-nil when FRAGMENT invokes PROGRAM."
  (let* ((name (file-name-nondirectory program))
         (regexp (format "\\(?:\\`\\|[[:space:]'\"()]\\|/\\)%s\\(?:\\'\\|[[:space:]'\"()]\\)"
                         (regexp-quote name))))
    (or (equal (file-name-nondirectory fragment) name)
        (string-match-p regexp fragment))))

(defun codex-ide--command-invokes-codex-p (fragments)
  "Return non-nil when any string in FRAGMENTS invokes Codex."
  (let ((programs (delete-dups (list codex-ide-cli-path "codex"))))
    (cl-some (lambda (fragment)
               (cl-some (lambda (program)
                          (codex-ide--command-fragment-invokes-p
                           fragment program))
                        programs))
             fragments)))

(defun codex-ide--codex-process-p (process)
  "Return non-nil when PROCESS appears to be a Codex terminal process."
  (and process
       (codex-ide-term--process-live-p process (codex-ide-term--process-buffer process))
       (codex-ide--command-invokes-codex-p
        (codex-ide--process-command-fragments process))))

(defun codex-ide--recoverable-buffer-p (buffer)
  "Return non-nil when BUFFER is a live Codex terminal buffer."
  (and (buffer-live-p buffer)
       (buffer-local-value 'codex-ide-mode buffer)
       (and-let* ((process (codex-ide-term--process buffer)))
         (codex-ide--codex-process-p process))))

(defun codex-ide--recovered-session-id (root buffer)
  "Return the session id to use when recovering BUFFER for ROOT."
  (let ((session-id (buffer-local-value 'codex-ide--session-id buffer)))
    (if (codex-ide--available-session-id-p root session-id)
        session-id
      (codex-ide--next-session-id root))))

(defun codex-ide--active-session-live-p (root)
  "Return non-nil when ROOT has a live active session."
  (and-let* ((session-id (gethash root codex-ide--active-session-ids))
             (session (codex-ide--session-by-id root session-id)))
    (codex-ide--session-live-p session)))

(defun codex-ide--recover-live-session (buffer)
  "Register BUFFER as a live Codex session when needed."
  (or (codex-ide--session-by-buffer buffer)
      (let* ((process (codex-ide-term--process buffer))
             (root (with-current-buffer buffer
                     (or codex-ide--session-root (codex-ide--get-working-directory))))
             (session-id (codex-ide--recovered-session-id root buffer))
             (session (codex-ide--make-session
                       root session-id buffer process)))
        (codex-ide--store-session session)
        (codex-ide--setup-session session)
        (unless (codex-ide--active-session-live-p root)
          (codex-ide--activate-session session))
        session)))

(defun codex-ide--recover-live-sessions ()
  "Register live Codex terminal buffers missing session records."
  (setq codex-ide--recovery-pending nil)
  (codex-ide--cleanup-dead-sessions)
  (mapc #'codex-ide--recover-live-session
        (cl-remove-if-not #'codex-ide--recoverable-buffer-p
                          (buffer-list)))
  (codex-ide--cleanup-dead-sessions))

(defun codex-ide--ensure-session-registry (&optional root)
  "Recover after reload or when ROOT has no registered sessions."
  (codex-ide--cleanup-dead-sessions)
  (when (or codex-ide--recovery-pending
            (and root (null (gethash root codex-ide--sessions)))
            (and codex-ide-mode codex-ide--session-root
                 (not (codex-ide--session-by-buffer (current-buffer)))))
    (codex-ide--recover-live-sessions)))

(defun codex-ide--mode-line ()
  "Return a concise session label and terminal backend."
  (list " " (propertize
             (format "Codex:%s" (or codex-ide--session-label
                                    codex-ide--session-id "?"))
             'face 'mode-line-emphasis)
        (propertize (format "/%s" codex-ide-term--backend) 'face 'shadow)))

;;; IDE context

(defun codex-ide--record-source-buffer (&optional directory buffer)
  "Record BUFFER as source context for DIRECTORY."
  (codex-ide-context-record-source-buffer
   (or directory (codex-ide--get-working-directory))
   (or buffer (current-buffer))))

(defun codex-ide--maybe-ensure-context-server ()
  "Start the IDE context provider when auto-start is enabled on GNU Emacs."
  (when (and codex-ide-context-auto-start (not (featurep 'neomacs)))
    (codex-ide-context-mode 1)))

;;; Session selection

(defun codex-ide--all-sessions ()
  "Return all live Codex session records."
  (codex-ide--ensure-session-registry)
  (cl-remove-if-not #'codex-ide--session-live-p (codex-ide--raw-sessions)))

(defun codex-ide--session-candidates (&optional directory)
  "Return (BUFFER-NAME . SESSION) pairs for live Codex sessions.
When DIRECTORY is non-nil, return only sessions for that project root."
  (let* ((sessions (if directory
                       (progn
                         (codex-ide--ensure-session-registry directory)
                         (codex-ide--project-sessions directory))
                     (codex-ide--all-sessions)))
         (candidates
          (mapcar (lambda (session)
                    (cons (buffer-name (plist-get session :buffer))
                          session))
                  sessions)))
    (sort candidates (lambda (a b) (string< (car a) (car b))))))

(defun codex-ide--session-annotation-function (candidates)
  "Return a completion `:annotation-function' over CANDIDATES."
  (lambda (buffer-name)
    (and-let* ((session (cdr (assoc buffer-name candidates))))
      (concat "  " (propertize (abbreviate-file-name
                                (plist-get session :root))
                               'face 'shadow)))))

(defun codex-ide--read-session (&optional directory default-session)
  "Read a live Codex session with completion.
When DIRECTORY is non-nil, offer only sessions for that project root.
DEFAULT-SESSION, when non-nil, is the initially selected session."
  (let ((candidates (codex-ide--session-candidates directory)))
    (unless candidates
      (user-error "No Codex sessions"))
    (let* ((default-name (and-let* ((buffer (plist-get default-session
                                                       :buffer))
                                    (name (buffer-name buffer)))
                           (and (assoc name candidates) name)))
           (completion-extra-properties
            (list :annotation-function
                  (codex-ide--session-annotation-function candidates)))
           (choice (completing-read "Codex session: "
                                    (codex-ide--completion-table
                                     candidates '((category . buffer))) nil t
                                    nil nil default-name)))
      (or (cdr (assoc choice candidates))
          (user-error "No Codex session selected")))))

(defun codex-ide--default-target-session (root sessions)
  "Return the default target session for ROOT from SESSIONS."
  (or (and-let* ((session-id (gethash root codex-ide--active-session-ids))
                 (session (codex-ide--session-by-id root session-id)))
        (and (memq session sessions) session))
      (car sessions)))

(defun codex-ide--target-session (&optional directory)
  "Return the target Codex session for DIRECTORY.
When the current buffer owns a live session for the project, use it
directly.  Otherwise prompt when the project has more than one live
session."
  (let ((root (or directory (codex-ide--get-working-directory))))
    (codex-ide--ensure-session-registry root)
    (let ((sessions (codex-ide--sorted-project-sessions root))
          (own (codex-ide--buffer-session)))
      (cond
       ((and own
             (equal root (plist-get own :root))
             (memq own sessions))
        (codex-ide--activate-session own))
       ((null sessions)
        (user-error "No Codex session for this project"))
       ((null (cdr sessions))
        (codex-ide--activate-session (car sessions)))
       (t
        (codex-ide--activate-session
         (codex-ide--read-session
          root (codex-ide--default-target-session root sessions))))))))

(defun codex-ide--switch-to-session (session)
  "Switch to SESSION's Codex terminal buffer."
  (unless (codex-ide--session-live-p session)
    (user-error "No live Codex session"))
  (codex-ide--activate-session session)
  (codex-ide--display-buffer (plist-get session :buffer) t))

(defun codex-ide--display-result-window (buffer result)
  "Return a live display window for BUFFER from display RESULT."
  (cond
   ((and (windowp result) (window-live-p result)) result)
   ((bufferp result) (get-buffer-window result (selected-frame)))
   (t (get-buffer-window buffer (selected-frame)))))

(defun codex-ide--display-window (buffer)
  "Display BUFFER and return its window."
  (codex-ide--display-result-window
   buffer
   (funcall codex-ide-display-buffer-function buffer)))

(cl-defun codex-ide--display-buffer (buffer &optional (select t))
  "Display BUFFER through the configured policy and synchronize its size.
SELECT defaults to t and focuses its window, including another frame when the
configured policy chooses one.  Nil preserves the selected window."
  (let* ((origin (selected-window))
         (window (or (get-buffer-window buffer (selected-frame))
                     (save-selected-window
                       (codex-ide--display-window buffer)))))
    (when (window-live-p window)
      (when select
        (when-let* ((session (codex-ide--buffer-session buffer)))
          (codex-ide--activate-session session))
        (unless (eq (window-frame window) (selected-frame))
          (select-frame-set-input-focus (window-frame window)))
        (select-window window))
      (unless select (select-window origin))
      (codex-ide-term--sync-dimensions buffer window)
      window)))

(defun codex-ide--hide-window (window)
  "Hide WINDOW and restore the layout preceding its display."
  (when (window-live-p window)
    (quit-restore-window window 'bury)))

(defun codex-ide--hide-displayed-buffer (buffer)
  "Hide every live window showing BUFFER."
  (dolist (window (get-buffer-window-list buffer nil (selected-frame)))
    (codex-ide--hide-window window)))

(defun codex-ide--session-visible-p (session)
  "Return non-nil when SESSION has a visible window."
  (get-buffer-window (plist-get session :buffer) (selected-frame)))

(defun codex-ide--selected-project-session (root sessions)
  "Return the selected project session for ROOT from SESSIONS."
  (and-let* ((session (codex-ide--buffer-session
                       (window-buffer (selected-window)))))
    (and (equal root (plist-get session :root))
         (memq session sessions)
         session)))

(defun codex-ide--visible-project-session (root sessions)
  "Return the visible project session to cycle from for ROOT in SESSIONS."
  (or (codex-ide--selected-project-session root sessions)
      (cl-find-if #'codex-ide--session-visible-p sessions)))

(defun codex-ide--hide-project-session-windows (sessions)
  "Hide visible windows for SESSIONS without stopping them."
  (dolist (session sessions)
    (codex-ide--hide-displayed-buffer (plist-get session :buffer))))

(defun codex-ide--active-or-first-session (sessions active-id)
  "Return ACTIVE-ID's session from SESSIONS, falling back to the first."
  (or (and active-id
           (cl-find active-id sessions
                    :key (lambda (session) (plist-get session :id))
                    :test #'=))
      (car sessions)))

(defun codex-ide--next-session (session sessions)
  "Return the session after SESSION in SESSIONS, or nil."
  (let ((position (cl-position session sessions :test #'eq)))
    (when (and position (< position (1- (length sessions))))
      (nth (1+ position) sessions))))

(defun codex-ide--cycle-project-session (root sessions active-id)
  "Cycle ROOT's visible Codex window through SESSIONS.
ACTIVE-ID is the session that was active before session recovery."
  (if-let* ((visible (codex-ide--visible-project-session root sessions)))
      (progn
        (codex-ide--hide-project-session-windows sessions)
        (if-let* ((next (codex-ide--next-session visible sessions)))
            (codex-ide--switch-to-session next)
          (codex-ide--activate-session (car sessions))
          (codex-ide-debug "Codex windows hidden")))
    (codex-ide--switch-to-session
     (codex-ide--active-or-first-session sessions active-id))))

(defun codex-ide--toggle-existing-window (buffer)
  "Show or hide the window showing BUFFER.
Used when a session is already running."
  (let ((window (get-buffer-window buffer (selected-frame))))
    (if window
        (progn
          (codex-ide--hide-displayed-buffer buffer)
          (codex-ide-debug "Codex window hidden"))
      (codex-ide--display-buffer buffer t)
      (codex-ide-debug "Codex window shown"))))

(defun codex-ide--cleanup-target-from-buffer (buffer)
  "Return BUFFER's cleanup target as (ROOT ID), or nil."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and codex-ide--session-root
              codex-ide--session-id
              (list codex-ide--session-root codex-ide--session-id)))))

(defun codex-ide--cleanup-target (directory session-id)
  "Resolve a cleanup target from DIRECTORY and SESSION-ID."
  (cond
   ((and (stringp directory) session-id)
    (list directory session-id))
   ((bufferp directory)
    (codex-ide--cleanup-target-from-buffer directory))
   ((processp directory)
    (codex-ide--cleanup-target-from-buffer (process-buffer directory)))
   (t
    (codex-ide--cleanup-target-from-buffer (current-buffer)))))

(defun codex-ide--cleanup-on-exit
    (&optional directory session-id expected-buffer expected-process
               preserve-buffer &rest _ignored)
  "Clean up the Codex session state for DIRECTORY and SESSION-ID.
When EXPECTED-BUFFER or EXPECTED-PROCESS is non-nil, clean up only while
the registered session still owns those exact resources.  PRESERVE-BUFFER
leaves the buffer alive after stopping its process and removing its record.
Guard reentry for this target while allowing other sessions to clean up."
  (when-let* ((target (codex-ide--cleanup-target directory session-id))
              (directory (car target))
              (session-id (cadr target)))
    (unless (member target codex-ide--cleanup-in-progress)
      (let* ((codex-ide--cleanup-in-progress
              (cons target codex-ide--cleanup-in-progress))
             (session (codex-ide--session-by-id directory session-id))
             (buffer (or (plist-get session :buffer)
                         (codex-ide--buffer-for-session directory session-id)
                         (get-buffer
                          (codex-ide--get-buffer-name directory session-id))))
             (process (or (plist-get session :process)
                          (and (buffer-live-p buffer)
                               (codex-ide-term--process buffer)))))
        (when (and (or (not expected-buffer) (eq buffer expected-buffer))
                   (or (not expected-process) (eq process expected-process)))
          (when buffer (codex-ide-diff--cancel-requests buffer process))
          (codex-ide--remove-session directory session-id)
          (codex-ide-term--stop-process process buffer)
          (when (and (not preserve-buffer) (buffer-live-p buffer))
            (let ((kill-buffer-query-functions nil))
              (kill-buffer buffer)))
          (codex-ide-debug "Cleaned up Codex session %s for %s"
                           session-id
                           (file-name-nondirectory
                            (directory-file-name directory))))))))

(defun codex-ide--cleanup-current-buffer-session ()
  "Clean up the Codex session owned by the current buffer."
  (when (and codex-ide--session-root codex-ide--session-id)
    (codex-ide--cleanup-on-exit
     codex-ide--session-root codex-ide--session-id
     (current-buffer) (codex-ide-term--process (current-buffer)) t)))

(defun codex-ide--cleanup-before-major-mode-change ()
  "Stop the current Codex session before replacing its major mode.
Reject vterm mode changes without disturbing its process or session."
  ;; vterm forbids mode replacement to protect its native terminal object.
  ;; Our hook runs first, so reject before any destructive cleanup.
  (when (derived-mode-p 'vterm-mode)
    (user-error "You cannot change major mode in vterm buffers"))
  (when (and codex-ide--session-root codex-ide--session-id)
    (codex-ide--cleanup-on-exit
     codex-ide--session-root codex-ide--session-id
     (current-buffer) (codex-ide-term--process (current-buffer)) t)))

(defun codex-ide--finish-session (root id buffer process success event)
  "Retire ROOT's ID with exact BUFFER and PROCESS ownership.
SUCCESS is non-nil only for a clean exit; EVENT describes the outcome."
  (let ((session (codex-ide--session-by-id root id)))
    (when (and session (eq buffer (plist-get session :buffer))
               (eq process (plist-get session :process)))
      (let ((keep (not (or (eq codex-ide-kill-buffer-on-exit t)
                           (and success
                                (eq codex-ide-kill-buffer-on-exit 'success))))))
        (codex-ide--cleanup-on-exit root id buffer process keep)
        (when (and keep (buffer-live-p buffer)
                   (not (codex-ide--session-by-buffer buffer))
                   (equal root (buffer-local-value 'codex-ide--session-root buffer))
                   (equal id (buffer-local-value 'codex-ide--session-id buffer))
                   (buffer-local-value 'codex-ide-mode buffer))
          (with-current-buffer buffer
            (when (eq codex-ide-term--backend 'vterm)
              (require 'codex-ide-term-vterm)
              (codex-ide-term-vterm--settle-buffer))
            (codex-ide-mode -1)
            ;; Retain vterm's major mode without its dead terminal input map.
            (use-local-map special-mode-map)
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (propertize (format "\n[Codex exited: %s]\n"
                                          (string-trim event))
                                  'face (if success 'success 'error))))
            (setq buffer-read-only t)))))))

(defun codex-ide--make-process-sentinel
    (directory session-id &optional original buffer)
  "Return the process sentinel for DIRECTORY and SESSION-ID.
ORIGINAL is the backend sentinel being replaced.  It runs first so the
terminal backend can finish its own teardown before Codex kills BUFFER."
  (lambda (proc event)
    (codex-ide-debug "Codex process event: %s" (string-trim event))
    (when (functionp original)
      ;; A failing backend sentinel must not block Codex session cleanup.
      (condition-case err
          (funcall original proc event)
        (error
         (codex-ide-debug "Chained sentinel failed: %s"
                          (error-message-string err)))))
    (when (memq (process-status proc) '(exit signal))
      (codex-ide--finish-session
       directory session-id
       (or buffer (and (processp proc) (process-buffer proc))) proc
       (and (eq (process-status proc) 'exit)
            (zerop (process-exit-status proc))) event))))

(defun codex-ide--make-env ()
  "Return the list of \"KEY=VALUE\" env vars for a Codex session."
  (list "TERM_PROGRAM=emacs"))

(defun codex-ide--setup-session (session)
  "Install process and buffer-local state for SESSION."
  (let ((buffer (plist-get session :buffer))
        (process (plist-get session :process))
        (directory (plist-get session :root))
        (session-id (plist-get session :id)))
    (when (processp process)
      (set-process-query-on-exit-flag process nil))
    ;; Recover/setup can re-enter for the same live process; wrap once so
    ;; Exit cleanup and the original backend sentinel each run once.
    (when (and (processp process)
               (not (process-get process 'codex-ide--sentinel-installed)))
      (set-process-sentinel
       process (codex-ide--make-process-sentinel
                directory session-id (process-sentinel process) buffer))
      (process-put process 'codex-ide--sentinel-installed t))
    (with-current-buffer buffer
      (setq-local codex-ide--session-root directory)
      (setq-local codex-ide--session-id session-id)
      (codex-ide-term--configure-buffer)
      (unless codex-ide-mode
        (codex-ide-mode 1)))
    (setf (plist-get session :awaiting-exit) t)
    session))

(defun codex-ide--create-session (emacs-session-id &optional resume-last
                                                   codex-session-id)
  "Create a Codex terminal session for the current project.
EMACS-SESSION-ID identifies the live Emacs-managed session.  RESUME-LAST
and CODEX-SESSION-ID are forwarded to `codex-ide--build-command'.
Returns a session record."
  (let* ((working-dir (codex-ide--get-working-directory))
         (buffer-name (generate-new-buffer-name
                       (codex-ide--get-buffer-name working-dir emacs-session-id)))
         (codex-ide-config-overrides
          (let ((default-directory working-dir))
            (codex-ide--session-config-overrides)))
         (cmd (codex-ide--build-command resume-last codex-session-id))
         (program (car cmd))
         (args (cdr cmd))
         (env (codex-ide--make-env))
         (buffer (codex-ide-term--prepare-buffer buffer-name working-dir)))
    (codex-ide-debug "Starting Codex: %s %s"
                     program (string-join args " "))
    (codex-ide-debug "Working directory: %s" working-dir)
    (condition-case err
        (progn
          (unless (codex-ide--display-buffer buffer)
            (error "Codex display function did not make %s visible"
                   (buffer-name buffer)))
          (let ((process (codex-ide-term--make-process
                          buffer program args env)))
            ;; Native launch replaces the placeholder with its own buffer.
            ;; Retain that exact owner for failures after the spawn returns.
            (setq buffer (codex-ide-term--process-buffer process))
            (when (processp process)
              (process-put process 'codex-ide--command (cons program args)))
            (codex-ide--make-session working-dir emacs-session-id buffer process)))
      (error
       (when (buffer-live-p buffer)
         ;; Startup may fail before setup disables process exit queries.
         (when-let* ((process (codex-ide-term--process buffer)))
           (codex-ide-term--stop-process process buffer))
         (kill-buffer buffer))
       (signal (car err) (cdr err))))))

(defun codex-ide--start-session (&optional resume-last codex-session-id
                                           new-session)
  "Start or focus a Codex session for the current project.
If RESUME-LAST is non-nil, resume the most recent session.  When
CODEX-SESSION-ID is given, resume that specific saved session.  If a live
session exists, toggle its window unless NEW-SESSION is non-nil."
  (unless (codex-ide--ensure-cli)
    (user-error "Codex CLI not available.  Install it and ensure it is in PATH"))
  (let ((working-dir (codex-ide--get-working-directory))
        (new-session (or new-session resume-last codex-session-id)))
    (codex-ide--ensure-session-registry working-dir)
    (let ((existing-session (and (not new-session)
                                 (codex-ide--active-session working-dir))))
      (codex-ide--record-source-buffer working-dir)
      (if existing-session
          (codex-ide--toggle-existing-window
           (plist-get existing-session :buffer))
        (condition-case err
            (codex-ide--maybe-ensure-context-server)
          (error (codex-ide-log "Context server unavailable: %s"
                                (error-message-string err))))
        (let* ((emacs-session-id (codex-ide--next-session-id working-dir))
               (session (codex-ide--create-session emacs-session-id
                                                   resume-last
                                                   codex-session-id))
               (buffer (plist-get session :buffer))
               (process (plist-get session :process)))
          (unless (and buffer process)
            (error "Failed to create Codex session"))
          (codex-ide--remember-session session)
          (condition-case err
              (codex-ide--setup-session session)
            (error
             (codex-ide--cleanup-on-exit working-dir emacs-session-id
                                         buffer process)
             (signal (car err) (cdr err))))
          (codex-ide-log "Codex started in %s"
                         (file-name-nondirectory
                          (directory-file-name working-dir))))))))

;;; Commands

;;;###autoload
(defun codex-ide (&optional prefix)
  "Start Codex for the current project, or toggle its active window.
With PREFIX, start another Codex session for the current project."
  (interactive "P")
  (codex-ide--start-session nil nil prefix))

;;;###autoload
(defun codex-ide-new-session ()
  "Start another Codex session for the current project."
  (interactive)
  (codex-ide--start-session nil nil t))

;;;###autoload
(defun codex-ide-resume-last ()
  "Resume the most recent Codex session for the current project."
  (interactive)
  (codex-ide--start-session t nil t))

;;;###autoload
(defun codex-ide-resume ()
  "Resume a saved Codex session chosen from known rollout ids.
Uses `codex resume <session-id>'.  Prefer `codex-ide-resume-last' for
the most recent session without a picker."
  (interactive)
  (let* ((saved (codex-ide--read-saved-session-id nil t))
         (codex-ide--working-directory (plist-get saved :cwd)))
    (codex-ide--start-session nil (plist-get saved :id) t)))

;;;###autoload
(defun codex-ide-stop ()
  "Stop the active Codex session for the current project.
Other live sessions under the same project root are left running.
Use `codex-ide-list-project-sessions' or `codex-ide-toggle' to switch
before stopping a different session."
  (interactive)
  (let* ((working-dir (codex-ide--get-working-directory))
         (session (codex-ide--active-session working-dir)))
    (if-let* ((buffer (plist-get session :buffer)))
        (when (or (not (called-interactively-p 'interactive))
                  (y-or-n-p (format "Stop Codex session %s? " (buffer-name buffer))))
          (kill-buffer buffer)
          (codex-ide-log "Stopping active Codex session in %s..."
                         (file-name-nondirectory
                          (directory-file-name working-dir))))
      (codex-ide-log "No active Codex session is running in this directory"))))

(defun codex-ide--panel-state (&optional root sessions)
  "Return this tab's remembered panels, optionally storing ROOT's SESSIONS."
  (require 'tab-bar)
  (let* ((tab (assq 'current-tab (tab-bar-tabs)))
         (live (codex-ide--raw-sessions))
         (entries
          (cl-loop for (key . saved) in (alist-get 'codex-ide-panels (cdr tab))
                   for retained = (cl-remove-if-not
                                   (lambda (session)
                                     (and (memq session live)
                                          (codex-ide--session-live-p session))) saved)
                   when (and retained (not (equal root key)))
                   collect (cons key retained)))
         (entries (if (and root sessions) (cons (cons root sessions) entries) entries)))
    ;; Tab cloning can share parameter cells: replace rather than mutate them.
    (setcdr tab (cons (cons 'codex-ide-panels entries)
                      (assq-delete-all 'codex-ide-panels (copy-sequence (cdr tab)))))
    entries))

(defun codex-ide--panel-windows (session)
  "Return SESSION's visible windows in the selected frame's current tab."
  (get-buffer-window-list (plist-get session :buffer) nil (selected-frame)))

(defun codex-ide--show-panel-session (session)
  "Show SESSION in the current frame without displacing another session."
  (let* ((buffer (plist-get session :buffer))
         (before (window-list))
         (owners (mapcar (lambda (window)
                           (cons window (codex-ide--session-by-buffer
                                         (window-buffer window)))) before))
         (saved (current-window-configuration))
         (slots (mapcar (lambda (window)
                          (when (eq (window-parameter window 'window-side) 'right)
                            (window-parameter window 'window-slot))) before))
         (slot (cl-loop for number from 0 unless (memq number slots) return number))
         (display-buffer-base-action
          `((display-buffer-in-side-window)
            (side . right) (slot . ,slot) (window-width . 0.4)))
         (window (codex-ide--display-buffer buffer nil)))
    ;; Honor user reuse rules, but not at the expense of another session panel.
    (unless (and window (let ((owner (cdr (assq window owners))))
                          (or (null owner) (eq owner session))))
      (set-window-configuration saved)
      (user-error "No room for another Codex session panel"))
    (codex-ide-term--sync-dimensions buffer window)))

;;;###autoload
(defun codex-ide-show-project-sessions ()
  "Show every live project session in the current tab without selecting it."
  (interactive)
  (let ((root (codex-ide--get-working-directory)))
    (codex-ide--ensure-session-registry root)
    (let ((sessions (codex-ide--sorted-project-sessions root)))
      (unless sessions (user-error "No Codex session for this project"))
      (codex-ide--panel-state)
      (mapc #'codex-ide--show-panel-session sessions))))

;;;###autoload
(defun codex-ide-toggle-panel ()
  "Hide or restore the project's session windows in the current tab.
Remember the visible set when hiding.  Restore that set when showing,
falling back to the active session.  Other tabs and projects are unaffected."
  (interactive)
  (let ((root (codex-ide--get-working-directory)))
    (codex-ide--ensure-session-registry root)
    (let* ((sessions (codex-ide--sorted-project-sessions root))
           (visible (cl-remove-if-not #'codex-ide--panel-windows sessions))
           (saved (cdr (assoc root (codex-ide--panel-state)))))
      (unless sessions (user-error "No Codex session for this project"))
      (if visible
          (let* ((buffers (mapcar (lambda (session) (plist-get session :buffer)) sessions))
                 (fallback (or (cl-find-if (lambda (buffer)
                                             (and (not (memq buffer buffers))
                                                  (not (string-prefix-p " " (buffer-name buffer)))))
                                           (buffer-list))
                               (get-buffer-create "*scratch*"))))
            (codex-ide--panel-state root visible)
            (dolist (session visible)
              (dolist (window (codex-ide--panel-windows session))
                (if (eq window (window-main-window))
                    (set-window-buffer window fallback)
                  (delete-window window)))))
        (mapc #'codex-ide--show-panel-session
              (or saved (list (codex-ide--active-or-first-session
                               sessions (gethash root codex-ide--active-session-ids)))))
        (codex-ide--panel-state root nil)))))

;;;###autoload
(defun codex-ide-cycle-session ()
  "Cycle live Codex sessions for the current project."
  (interactive)
  (let* ((working-dir (codex-ide--get-working-directory))
         (active-id (gethash working-dir codex-ide--active-session-ids)))
    (codex-ide--record-source-buffer working-dir)
    (codex-ide--recover-live-sessions)
    (let ((sessions (codex-ide--sorted-project-sessions working-dir)))
      (if sessions
          (codex-ide--cycle-project-session working-dir sessions active-id)
        (user-error "No Codex session for this project")))))

;;;###autoload
(defun codex-ide-switch-to-buffer ()
  "Switch to the Codex buffer for the current project.
If it is not visible, display it with `codex-ide-display-buffer-function'."
  (interactive)
  (let ((working-dir (codex-ide--get-working-directory)))
    (codex-ide--record-source-buffer working-dir)
    (if-let* ((session (codex-ide--active-session working-dir)))
        (codex-ide--display-buffer (plist-get session :buffer))
      (user-error
       "No Codex session for this project.  Use M-x codex-ide to start one"))))

;;;###autoload
(defun codex-ide-switch-project-session ()
  "Switch to a live Codex terminal session for the current project."
  (interactive)
  (let* ((origin (current-buffer))
         (working-dir (codex-ide--get-working-directory))
         (session (codex-ide--read-session working-dir)))
    (codex-ide--record-source-buffer working-dir origin)
    (codex-ide--switch-to-session session)))

;;;###autoload
(defun codex-ide-switch-session ()
  "Switch to any live Codex terminal session."
  (interactive)
  (let* ((origin (current-buffer))
         (session (codex-ide--read-session))
         (directory (plist-get session :root)))
    (codex-ide--record-source-buffer directory origin)
    (codex-ide--switch-to-session session)))

;;;###autoload
(define-obsolete-function-alias 'codex-ide-toggle
  #'codex-ide-cycle-session "0.1.5")

;;;###autoload
(define-obsolete-function-alias 'codex-ide-list-project-sessions
  #'codex-ide-switch-project-session "0.1.5")

(defun codex-ide--set-session-label (session label)
  "Rename SESSION using LABEL without changing its identity."
  (let* ((buffer (plist-get session :buffer))
         (root (plist-get session :root))
         (id (plist-get session :id)))
    (unless (and (codex-ide--session-live-p session)
                 (eq session (codex-ide--session-by-id root id)))
      (user-error "Codex session is no longer current"))
    (when (or (string-match-p "[[:cntrl:]]" label)
              (cl-some
               (lambda (other)
                 (and (not (eq session other))
                      (equal label (buffer-local-value
                                    'codex-ide--session-label
                                    (plist-get other :buffer)))))
               (codex-ide--project-sessions root)))
      (user-error "Invalid or already used Codex session label"))
    (with-current-buffer buffer
      (let* ((base (codex-ide--get-buffer-name root 1))
             (name (if (string-empty-p label)
                       (codex-ide--indexed-buffer-name base id)
                     (concat (string-remove-suffix "*" base) ":" label "*"))))
	(rename-buffer name t)
	(setq codex-ide--session-naming-base base
              codex-ide--session-label (unless (string-empty-p label) label))
        (when (eq codex-ide-term--backend 'neo-term)
          (setf (plist-get codex-ide-term-neo--resource :name) (buffer-name)))))))

;;;###autoload
(defun codex-ide-rename-session (&optional label)
  "Set the selected live session's LABEL; empty input clears it.
Interactively, choose the target session before prompting for its label.
Labels do not rename saved Codex conversations."
  (interactive)
  (let* ((session (codex-ide--target-session))
         (current (buffer-local-value 'codex-ide--session-label
                                      (plist-get session :buffer)))
         (label (or label (read-string "Session label (empty clears): " current))))
    (codex-ide--set-session-label session (string-trim label))))

(defun codex-ide--source-attachment ()
  "Snapshot the region or current line with its source and line range."
  (let* ((region (use-region-p))
         (beg (if region (region-beginning) (line-beginning-position)))
         (end (if region (region-end) (line-end-position)))
         (source (if (and buffer-file-name
                          (progn (codex-ide-term--validate-draft buffer-file-name) t))
                     (file-relative-name buffer-file-name
                                         (codex-ide--get-working-directory))
                   (buffer-name)))
         (text (buffer-substring-no-properties beg end))
         (draft (format "Source: %s (lines %d-%d)\n%s" source
                        (line-number-at-pos beg t)
                        (line-number-at-pos (max beg (1- end)) t) text)))
    (codex-ide-term--validate-draft draft)
    draft))

;;;###autoload
(defun codex-ide-attach-source ()
  "Attach the region or current line to one Codex session's draft.
Snapshot source text and location before choosing the project session.
Insert a literal bracketed paste without submitting or changing the kill
ring.  Reject terminal controls except TAB and LF, and drafts over 1 MiB."
  (interactive)
  (let* ((draft (codex-ide--source-attachment))
         (session (codex-ide--target-session))
         (root (plist-get session :root))
         (id (plist-get session :id))
         (buffer (plist-get session :buffer))
         (process (plist-get session :process)))
    (unless (and (codex-ide--session-live-p session)
                 (eq session (codex-ide--session-by-id root id))
                 (buffer-local-value 'codex-ide-mode buffer)
                 (equal root (buffer-local-value 'codex-ide--session-root buffer))
                 (equal id (buffer-local-value 'codex-ide--session-id buffer)))
      (user-error "Codex session is no longer current"))
    (with-current-buffer buffer
      (codex-ide-term--paste-draft process draft))
    (codex-ide--display-buffer buffer nil)))

(defun codex-ide--input-owner (session)
  "Capture and validate SESSION's exact terminal input owner."
  (let* ((buffer (plist-get session :buffer))
         (owner (append (list :record session
                              :mode (and (buffer-live-p buffer)
                                         (buffer-local-value 'major-mode buffer))
                              :backend (and (buffer-live-p buffer)
                                            (buffer-local-value
                                             'codex-ide-term--backend buffer)))
                        (copy-sequence session))))
    (codex-ide--validate-input-owner owner)
    owner))

(defun codex-ide--validate-input-owner (owner)
  "Reject stale terminal input OWNER and return its buffer."
  (let ((session (plist-get owner :record))
        (root (plist-get owner :root))
        (id (plist-get owner :id))
        (buffer (plist-get owner :buffer))
        (process (plist-get owner :process)))
    (unless (and (eq session (codex-ide--session-by-id root id))
                 (equal root (plist-get session :root))
                 (equal id (plist-get session :id))
                 (eq buffer (plist-get session :buffer))
                 (eq process (plist-get session :process))
                 (buffer-live-p buffer)
                 (codex-ide-term--process-live-p process buffer)
                 (eq process (codex-ide-term--process buffer))
                 (with-current-buffer buffer
                   (and codex-ide-mode
                        (derived-mode-p 'eat-mode 'vterm-mode 'neo-term-mode)
                        (eq major-mode (plist-get owner :mode))
                        (eq codex-ide-term--backend (plist-get owner :backend))
                        (equal root codex-ide--session-root)
                        (equal id codex-ide--session-id))))
      (user-error "Codex session is no longer current"))
    buffer))

(defun codex-ide--submit-input (owner)
  "Submit input only to the still-current terminal OWNER."
  (codex-ide--validate-input-owner owner)
  ;; Give the TUI a chance to consume the preceding input, not a readiness
  ;; guarantee.  The event loop can retire or replace the captured owner.
  (sit-for 0.1)
  (with-current-buffer (codex-ide--validate-input-owner owner)
    (codex-ide-term--send-return)))

;;;###autoload
(defun codex-ide-send-prompt (&optional prompt)
  "Send PROMPT to the Codex terminal for the current project.
Interactively, read PROMPT from the minibuffer.
Paste it literally, then submit once.  Reject terminal controls except
TAB and LF, and prompts over 1 MiB of UTF-8 text."
  (interactive)
  (let ((working-dir (codex-ide--get-working-directory))
        (origin (current-buffer)))
    (codex-ide--record-source-buffer working-dir origin)
    (let* ((session (codex-ide--target-session working-dir))
           (owner (codex-ide--input-owner session))
           (text (or prompt (read-string "Codex prompt: " nil
                                         'codex-ide-prompt-history))))
      (unless (string-empty-p text)
        (with-current-buffer (codex-ide--validate-input-owner owner)
          (codex-ide-term--paste-draft (plist-get owner :process) text))
        (codex-ide--submit-input owner)
        (codex-ide-debug "Sent prompt: %s" text)))))

;;;###autoload
(defun codex-ide-send-escape ()
  "Send ESC to the Codex terminal for the current project."
  (interactive)
  (let* ((session (codex-ide--target-session))
         (buffer (plist-get session :buffer)))
    (with-current-buffer buffer
      (codex-ide-term--send-escape))))

;;;###autoload
(defun codex-ide-return-live ()
  "Restore terminal input and follow the live Codex frame."
  (interactive)
  (codex-ide-term--return-live))

;;;###autoload
(defun codex-ide-insert-newline ()
  "Send a literal LF (Ctrl+J) to insert a newline in the Codex prompt."
  (interactive)
  (let* ((session (codex-ide--target-session))
         (owner (codex-ide--input-owner session)))
    (with-current-buffer (codex-ide--validate-input-owner owner)
      (codex-ide-term--send-string "\n"))))

;;;###autoload
(defun codex-ide-check-status ()
  "Check whether the Codex CLI is available and report its version."
  (interactive)
  (codex-ide--detect-cli)
  (if codex-ide--cli-available
      (codex-ide-log "Codex CLI version: %s"
                     (plist-get codex-ide--cli-cache :version))
    (codex-ide-log "Codex CLI is not installed.")))

(provide 'codex-ide)

;;; codex-ide.el ends here
