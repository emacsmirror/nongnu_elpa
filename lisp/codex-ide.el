;;; codex-ide.el --- Run Codex CLI in an Emacs terminal  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.2") (keymap-popup "0.3.1") (vterm "0.0.2"))
;; Keywords: ai, codex, tools, terminal
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

;; Run the Codex CLI inside Emacs through vterm.  This is a terminal-first
;; integration: live Codex sessions are grouped by project root and displayed
;; through a configurable buffer display function, with prompt sending, session
;; cycling, and resume.
;;
;; Usage:
;;   M-x codex-ide              Start or toggle Codex for the current project
;;   C-u M-x codex-ide          Start another Codex session
;;   M-x codex-ide-resume-last  Resume the most recent Codex session
;;   M-x codex-ide-new-session  Start another Codex session
;;   M-x codex-ide-toggle       Cycle project Codex sessions
;;   M-x codex-ide-send-prompt  Send a prompt from the minibuffer
;;   M-x codex-ide-stop         Stop the session for the current project
;;   M-x codex-ide-list-project-sessions  Switch project Codex sessions
;;   M-x codex-ide-list-sessions  Switch to any live Codex session
;;   M-x codex-ide-menu         Popup menu of all commands

;;; Code:

(require 'compat)
(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'codex-ide-context)
(require 'codex-ide-debug)
(require 'codex-ide-mcp)
(require 'codex-ide-term)

(autoload 'codex-ide-menu "codex-ide-menu" nil t)

;;; Customization

(defgroup codex-ide nil
  "Run Codex CLI inside Emacs through vterm."
  :group 'tools
  :prefix "codex-ide-")

(defcustom codex-ide-cli-path "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'codex-ide)

(defcustom codex-ide-display-buffer-function #'pop-to-buffer-same-window
  "Function used to display the Codex terminal buffer.
The function is called with the Codex buffer and should display it.  When it
returns a live window, that window is used for terminal dimension sync;
otherwise Codex falls back to any live window already showing the buffer."
  :type 'function
  :group 'codex-ide)

(defcustom codex-ide-ask-for-approval nil
  "When to ask for human approval before Codex executes commands.
When nil (default), do not pass `--ask-for-approval' so that Codex's
own `config.toml' policy decides.  Otherwise pass the chosen policy."
  :type '(choice (const :tag "Config decides (nil)" nil)
                 (const :tag "untrusted" untrusted)
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
Escape hatch for flags not yet modeled by a defcustom."
  :type '(repeat string)
  :group 'codex-ide)

(defcustom codex-ide-no-alt-screen nil
  "When non-nil, pass `--no-alt-screen' for inline TUI mode."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-buffer-name-function #'codex-ide--default-buffer-name
  "Function called with the working directory to produce a buffer name."
  :type 'function
  :group 'codex-ide)

;;; Variables

(defvar codex-ide--cli-available nil
  "Whether the Codex CLI was detected.")

(defvar codex-ide--sessions (make-hash-table :test 'equal)
  "Hash table mapping project roots to live Codex session records.")

(defvar codex-ide--active-session-ids (make-hash-table :test 'equal)
  "Hash table mapping project roots to active Codex session ids.")

(defvar codex-ide--last-accessed-buffer nil
  "The most recently displayed Codex buffer.")

(defvar codex-ide--cleanup-in-progress nil
  "Reentrancy guard for `codex-ide--cleanup-on-exit'.")

(defvar-local codex-ide--session-root nil
  "Project root for the Codex session in the current buffer.")

(defvar-local codex-ide--session-id nil
  "Numeric Codex session id for the current buffer.")

;;; Helpers (pure / mostly pure)

(defun codex-ide--get-working-directory ()
  "Return the working directory for a Codex session.
Prefers the current project root; falls back to `default-directory'."
  (expand-file-name
   (if-let* ((project (project-current)))
       (project-root project)
     default-directory)))

(defun codex-ide--default-buffer-name (directory)
  "Return the buffer name for DIRECTORY, as `*codex[<basename>]*'."
  (format "*codex[%s]*"
          (file-name-nondirectory (directory-file-name directory))))

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
DIRECTORY defaults to the current project and SESSION-ID defaults to 1."
  (codex-ide--indexed-buffer-name
   (funcall codex-ide-buffer-name-function
            (or directory (codex-ide--get-working-directory)))
   (or session-id 1)))

(defun codex-ide--make-session (root id buffer process)
  "Return a Codex session record for ROOT, ID, BUFFER, and PROCESS."
  (list :root root :id id :buffer buffer :process process))

(defun codex-ide--raw-sessions ()
  "Return all session records without refreshing the table."
  (let (sessions)
    (maphash (lambda (_root root-sessions)
               (setq sessions (append root-sessions sessions)))
             codex-ide--sessions)
    sessions))

(defun codex-ide--session-live-p (session)
  "Return non-nil when SESSION owns a live terminal process."
  (let ((buffer (plist-get session :buffer))
        (process (plist-get session :process)))
    (and (buffer-live-p buffer)
         (process-live-p process)
         (eq (get-buffer-process buffer) process))))

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
  (let ((root (plist-get session :root)))
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
  (codex-ide--recover-live-sessions)
  (let ((root (or directory (codex-ide--get-working-directory))))
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

(defun codex-ide--session-config-overrides ()
  "Return Codex config overrides for a new session.
Includes user-provided `codex-ide-config-overrides' and any
session-local overrides needed by enabled integration helpers."
  (append codex-ide-config-overrides
          (when codex-ide-mcp-enabled
            (codex-ide-mcp-config-overrides
             (codex-ide-mcp-ensure-server)))))

;;; CLI detection

(defun codex-ide--detect-cli ()
  "Detect whether the Codex CLI is available and cache the result."
  (setq codex-ide--cli-available
        (condition-case nil
            (eq (call-process codex-ide-cli-path nil nil nil "--version") 0)
          (error nil))))

(defun codex-ide--ensure-cli ()
  "Return non-nil if the Codex CLI is available, detecting if needed."
  (unless codex-ide--cli-available
    (codex-ide--detect-cli))
  codex-ide--cli-available)

;;; Process lifecycle

(defun codex-ide--cleanup-dead-sessions ()
  "Remove entries for dead sessions from the session table."
  (maphash (lambda (root sessions)
             (let ((live-sessions
                    (cl-remove-if-not #'codex-ide--session-live-p sessions)))
               (if live-sessions
                   (puthash root live-sessions codex-ide--sessions)
                 (remhash root codex-ide--sessions))
               (codex-ide--sync-active-session root live-sessions)))
           codex-ide--sessions))

(defun codex-ide--codex-buffer-name-p (name)
  "Return non-nil when NAME has the Codex terminal buffer shape."
  (or (string-match-p "\\`\\*codex\\[[^]\n]+\\]\\*\\(?:<[0-9]+>\\)?\\'"
                      name)
      (string-match-p "\\`\\*codex\\[[^]\n]+\\]<[0-9]+>\\*\\'"
                      name)))

(defun codex-ide--buffer-name-session-id (name)
  "Return the session id parsed from NAME, or nil."
  (cond
   ((string-match "\\`\\*codex\\[[^]\n]+\\]<\\([0-9]+\\)>\\*\\'" name)
    (string-to-number (match-string 1 name)))
   ((string-match "\\`\\*codex\\[[^]\n]+\\]\\*<\\([0-9]+\\)>\\'" name)
    (string-to-number (match-string 1 name)))))

(defun codex-ide--available-session-id-p (root session-id)
  "Return non-nil when SESSION-ID can be used for ROOT."
  (and (integerp session-id)
       (> session-id 0)
       (not (codex-ide--session-by-id root session-id))))

(defun codex-ide--process-command-fragments (process)
  "Return strings that may describe PROCESS's command."
  (let ((command (ignore-errors (process-command process)))
        (recorded (process-get process 'codex-ide--command))
        (buffer (process-buffer process)))
    (append (cl-remove-if-not #'stringp command)
            (cl-remove-if-not #'stringp recorded)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (and (boundp 'vterm-shell)
                     (stringp vterm-shell)
                     (list vterm-shell)))))))

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
  (and (process-live-p process)
       (codex-ide--command-invokes-codex-p
        (codex-ide--process-command-fragments process))))

(defun codex-ide--recoverable-buffer-p (buffer)
  "Return non-nil when BUFFER is a live Codex terminal buffer."
  (and (buffer-live-p buffer)
       (codex-ide--codex-buffer-name-p (buffer-name buffer))
       (and-let* ((process (get-buffer-process buffer)))
         (codex-ide--codex-process-p process))))

(defun codex-ide--recovered-session-id (root buffer)
  "Return the session id to use when recovering BUFFER for ROOT."
  (or (cl-find-if
       (lambda (session-id)
         (codex-ide--available-session-id-p root session-id))
       (list (with-current-buffer buffer codex-ide--session-id)
             (codex-ide--buffer-name-session-id (buffer-name buffer))))
      (codex-ide--next-session-id root)))

(defun codex-ide--active-session-live-p (root)
  "Return non-nil when ROOT has a live active session."
  (and-let* ((session-id (gethash root codex-ide--active-session-ids))
             (session (codex-ide--session-by-id root session-id)))
    (codex-ide--session-live-p session)))

(defun codex-ide--recover-live-session (buffer)
  "Register BUFFER as a live Codex session when needed."
  (or (and-let* ((session (codex-ide--session-by-buffer buffer)))
        (codex-ide--setup-session session))
      (let* ((process (get-buffer-process buffer))
             (root (with-current-buffer buffer
                     (codex-ide--get-working-directory)))
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
  (codex-ide--cleanup-dead-sessions)
  (mapc #'codex-ide--recover-live-session
        (cl-remove-if-not #'codex-ide--recoverable-buffer-p
                          (buffer-list)))
  (codex-ide--cleanup-dead-sessions))

;;; IDE context

(defun codex-ide--record-source-buffer (&optional directory buffer)
  "Record BUFFER as source context for DIRECTORY."
  (codex-ide-context-record-source-buffer
   (or directory (codex-ide--get-working-directory))
   (or buffer (current-buffer))))

(defun codex-ide--maybe-ensure-context-server ()
  "Start the IDE context provider when auto-start is enabled."
  (when codex-ide-context-auto-start
    (codex-ide-context-mode 1)))

;;; Session selection

(defun codex-ide--all-sessions ()
  "Return all live Codex session records."
  (codex-ide--recover-live-sessions)
  (codex-ide--raw-sessions))

(defun codex-ide--session-candidates (&optional directory)
  "Return (BUFFER-NAME . SESSION) pairs for live Codex sessions.
When DIRECTORY is non-nil, return only sessions for that project root."
  (let* ((sessions (if directory
                       (progn
                         (codex-ide--recover-live-sessions)
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
           (choice (completing-read "Codex session: " candidates nil t
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
    (codex-ide--recover-live-sessions)
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
  (codex-ide--display-buffer (plist-get session :buffer)))

(defun codex-ide--display-result-window (buffer result)
  "Return a live display window for BUFFER from display RESULT."
  (cond
   ((and (windowp result) (window-live-p result)) result)
   ((bufferp result) (get-buffer-window result t))
   (t (get-buffer-window buffer t))))

(defun codex-ide--display-window (buffer)
  "Display BUFFER and return its window."
  (codex-ide--display-result-window
   buffer
   (funcall codex-ide-display-buffer-function buffer)))

(defun codex-ide--display-buffer (buffer)
  "Display BUFFER according to Codex window customization.
Returns the displayed window when one is available.  Updates
`codex-ide--last-accessed-buffer'."
  (setq codex-ide--last-accessed-buffer buffer)
  (when-let* ((session (codex-ide--buffer-session buffer)))
    (codex-ide--activate-session session))
  (when-let* ((window (or (get-buffer-window buffer t)
                          (codex-ide--display-window buffer))))
    (select-window window)
    (codex-ide-term--sync-dimensions buffer window)
    window))

(defun codex-ide--hide-window (window)
  "Hide WINDOW without killing its buffer."
  (when (window-live-p window)
    (with-selected-window window
      (bury-buffer))))

(defun codex-ide--hide-displayed-buffer (buffer)
  "Hide every live window showing BUFFER."
  (setq codex-ide--last-accessed-buffer buffer)
  (dolist (window (get-buffer-window-list buffer nil t))
    (codex-ide--hide-window window)))

(defun codex-ide--session-visible-p (session)
  "Return non-nil when SESSION has a visible window."
  (get-buffer-window (plist-get session :buffer) t))

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
  (let ((window (get-buffer-window buffer t)))
    (if window
        (progn
          (codex-ide--hide-displayed-buffer buffer)
          (codex-ide-debug "Codex window hidden"))
      (codex-ide--display-buffer buffer)
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

(defun codex-ide--cleanup-on-exit (&optional directory session-id &rest _ignored)
  "Clean up the Codex session state for DIRECTORY and SESSION-ID.
Reentrancy-guarded: sentinels and `kill-buffer-hook' can both fire."
  (when-let* ((target (codex-ide--cleanup-target directory session-id))
              (directory (car target))
              (session-id (cadr target)))
    (unless codex-ide--cleanup-in-progress
      (let* ((codex-ide--cleanup-in-progress t)
             (session (codex-ide--session-by-id directory session-id))
             (buffer (or (plist-get session :buffer)
                         (get-buffer
                          (codex-ide--get-buffer-name directory session-id)))))
        (codex-ide--remove-session directory session-id)
        (when buffer
          (when (buffer-live-p buffer)
            (let ((kill-buffer-hook nil)
                  (kill-buffer-query-functions nil))
              (kill-buffer buffer))))
        (codex-ide-debug "Cleaned up Codex session %s for %s"
                         session-id
                         (file-name-nondirectory (directory-file-name directory)))))))

(defun codex-ide--cleanup-current-buffer-session ()
  "Clean up the Codex session owned by the current buffer."
  (when (and codex-ide--session-root codex-ide--session-id)
    (codex-ide--cleanup-on-exit
     codex-ide--session-root codex-ide--session-id)))

(defun codex-ide--stale-cleanup-hook-p (function)
  "Return non-nil when FUNCTION is an obsolete cleanup hook."
  (or (eq function 'codex-ide--cleanup-on-exit)
      (string-prefix-p
       "Clean up the Codex session state"
       (or (ignore-errors (documentation function t)) ""))))

(defun codex-ide--remove-stale-cleanup-hooks ()
  "Remove obsolete cleanup hooks from the current session buffer."
  (setq-local kill-buffer-hook
              (cl-remove-if #'codex-ide--stale-cleanup-hook-p
                            kill-buffer-hook)))

(defun codex-ide--make-process-sentinel (directory session-id)
  "Return the process sentinel for DIRECTORY and SESSION-ID."
  (lambda (_proc event)
    (codex-ide-debug "Codex process event: %s" (string-trim event))
    (when (string-match-p
           (rx (or "finished" "exited" "killed" "terminated"))
           event)
      (codex-ide--cleanup-on-exit directory session-id))))

(defun codex-ide--make-env ()
  "Return the list of \"KEY=VALUE\" env vars for a Codex session."
  (list "TERM_PROGRAM=emacs"))

(defun codex-ide--setup-terminal-keybindings ()
  "Install Codex local keybindings in the current terminal buffer."
  (local-set-key (kbd "S-<return>") #'codex-ide-insert-newline)
  (local-set-key (kbd "C-<escape>") #'codex-ide-send-escape))

(defun codex-ide--setup-session (session)
  "Install process and buffer-local state for SESSION."
  (let ((buffer (plist-get session :buffer))
        (process (plist-get session :process))
        (directory (plist-get session :root))
        (session-id (plist-get session :id)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process (codex-ide--make-process-sentinel directory session-id))
    (with-current-buffer buffer
      (setq-local codex-ide--session-root directory)
      (setq-local codex-ide--session-id session-id)
      (codex-ide--remove-stale-cleanup-hooks)
      (add-hook 'kill-buffer-hook
                #'codex-ide--cleanup-current-buffer-session nil t)
      (codex-ide--setup-terminal-keybindings))
    session))

(defun codex-ide--create-session (emacs-session-id &optional resume-last
                                                    codex-session-id)
  "Create a Codex terminal session for the current project.
EMACS-SESSION-ID identifies the live Emacs-managed session.  RESUME-LAST
and CODEX-SESSION-ID are forwarded to `codex-ide--build-command'.
Returns a session record."
  (let* ((working-dir (codex-ide--get-working-directory))
         (buffer-name (codex-ide--get-buffer-name working-dir
                                                  emacs-session-id))
         (codex-ide-config-overrides (codex-ide--session-config-overrides))
         (cmd (codex-ide--build-command resume-last codex-session-id))
         (program (car cmd))
         (args (cdr cmd))
         (env (codex-ide--make-env))
         (default-directory working-dir))
    (codex-ide-debug "Starting Codex: %s %s"
                     program (string-join args " "))
    (codex-ide-debug "Working directory: %s" working-dir)
    (let ((process (codex-ide-term--make-process
                    buffer-name program args env working-dir)))
      (process-put process 'codex-ide--command (cons program args))
      (codex-ide--make-session working-dir emacs-session-id
                                (process-buffer process) process))))

(defun codex-ide--start-session (&optional resume-last codex-session-id
                                           new-session)
  "Start or focus a Codex session for the current project.
If RESUME-LAST is non-nil, resume the most recent session.  When
CODEX-SESSION-ID is given, resume that specific saved session.  If a live
session exists, toggle its window unless NEW-SESSION is non-nil."
  (unless (codex-ide--ensure-cli)
    (user-error "Codex CLI not available.  Install it and ensure it is in PATH"))
  (codex-ide--recover-live-sessions)
  (let* ((working-dir (codex-ide--get-working-directory))
         (existing-session (and (not new-session)
                                (codex-ide--active-session working-dir))))
    (codex-ide--record-source-buffer working-dir)
    (if existing-session
        (codex-ide--toggle-existing-window (plist-get existing-session :buffer))
      (codex-ide--maybe-ensure-context-server)
      (let* ((emacs-session-id (codex-ide--next-session-id working-dir))
             (session (codex-ide--create-session emacs-session-id
                                                  resume-last
                                                  codex-session-id))
             (buffer (plist-get session :buffer))
             (process (plist-get session :process)))
        (unless (and buffer process)
          (error "Failed to create Codex session"))
        (codex-ide--remember-session session)
        (codex-ide--setup-session session)
        (codex-ide--display-buffer buffer)
        (codex-ide-log "Codex started in %s"
                       (file-name-nondirectory
                        (directory-file-name working-dir)))))))

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
  (codex-ide--start-session t))

;;;###autoload
(defun codex-ide-resume ()
  "Resume a Codex session for the current project.
In the MVP this resumes the most recent session via `codex resume --last'.
A session picker is deferred to a later phase."
  (interactive)
  (codex-ide--start-session t))

;;;###autoload
(defun codex-ide-stop ()
  "Stop the Codex session for the current project."
  (interactive)
  (let* ((working-dir (codex-ide--get-working-directory))
         (session (codex-ide--active-session working-dir)))
    (if-let* ((buffer (plist-get session :buffer)))
        (progn
          (kill-buffer buffer)
          (codex-ide-log "Stopping Codex in %s..."
                         (file-name-nondirectory
                          (directory-file-name working-dir))))
      (codex-ide-log "No Codex session is running in this directory"))))

;;;###autoload
(defun codex-ide-toggle ()
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
(defun codex-ide-list-project-sessions ()
  "Switch to a live Codex terminal session for the current project."
  (interactive)
  (let* ((origin (current-buffer))
         (working-dir (codex-ide--get-working-directory))
         (session (codex-ide--read-session working-dir)))
    (codex-ide--record-source-buffer working-dir origin)
    (codex-ide--switch-to-session session)))

;;;###autoload
(defun codex-ide-list-sessions ()
  "Switch to any live Codex terminal session."
  (interactive)
  (let* ((origin (current-buffer))
         (session (codex-ide--read-session))
         (directory (plist-get session :root)))
    (codex-ide--record-source-buffer directory origin)
    (codex-ide--switch-to-session session)))

;;;###autoload
(defun codex-ide-send-prompt (&optional prompt)
  "Send PROMPT to the Codex terminal for the current project.
Interactively, read PROMPT from the minibuffer."
  (interactive)
  (let ((working-dir (codex-ide--get-working-directory))
        (origin (current-buffer)))
    (codex-ide--record-source-buffer working-dir origin)
    (let* ((session (codex-ide--target-session working-dir))
           (buffer (plist-get session :buffer))
           (text (or prompt (read-string "Codex prompt: "))))
      (unless (string-empty-p text)
        (with-current-buffer buffer
          (codex-ide-term--send-string text)
          (sit-for 0.1)
          (codex-ide-term--send-return))
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
(defun codex-ide-insert-newline ()
  "Insert a literal newline into the Codex prompt.
Sends backslash followed by RET, which Codex interprets as a newline."
  (interactive)
  (let* ((session (codex-ide--target-session))
         (buffer (plist-get session :buffer)))
    (with-current-buffer buffer
      (codex-ide-term--send-string "\\")
      (sit-for 0.1)
      (codex-ide-term--send-return))))

;;;###autoload
(defun codex-ide-check-status ()
  "Check whether the Codex CLI is available and report its version."
  (interactive)
  (codex-ide--detect-cli)
  (if codex-ide--cli-available
      (let ((version (with-temp-buffer
                       (call-process codex-ide-cli-path nil t nil "--version")
                       (string-trim (buffer-string)))))
        (codex-ide-log "Codex CLI version: %s" version))
    (codex-ide-log "Codex CLI is not installed.")))

(provide 'codex-ide)

;;; codex-ide.el ends here
