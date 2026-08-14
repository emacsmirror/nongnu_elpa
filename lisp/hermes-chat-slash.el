;;; hermes-chat-slash.el --- Slash commands for Hermes chat  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Assisted-by: Hermes:MoA
;; Keywords: tools, convenience

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

;; Slash commands for `hermes-chat': parsing `/command arg' input, the
;; `commands.catalog' cache and its `completion-at-point' function, the
;; native in-client command table, and gateway dispatch through
;; `slash.exec' with `command.dispatch' fallback.  Part of the one logical
;; chat module (see the require note in `hermes-chat.el'); it preserves
;; the existing `hermes-chat--*' symbols.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-chat-buffer)
(require 'hermes-chat-dashboard)


(defvar hermes-chat--dashboard-active-session-id)
(defvar hermes-chat--dashboard-client)
(defvar hermes-chat--lifecycle-generation)
(defvar hermes-chat--runtime-flags)

(defvar-local hermes-chat--commands-cache nil
  "Cached slash command catalog as an alist of (NAME . DESCRIPTION).")

(defun hermes-chat--subcommand-name (value)
  "Return VALUE as a slash command name without leading slash."
  (and-let* ((name (hermes-chat--scalar-string value)))
    (string-remove-prefix "/" name)))

(defun hermes-chat--format-subcommand-entry (entry)
  "Return readable catalog line for subcommand ENTRY."
  (let* ((command (hermes-chat--subcommand-name (car-safe entry)))
         (items (hermes-chat--listify (cdr-safe entry)))
         (subs (delq nil
                     (mapcar (lambda (item)
                               (when-let* ((sub (hermes-chat--scalar-string item)))
                                 (format "/%s %s" command sub)))
                             items))))
    (and (hermes-transport--non-empty-string command) subs
         (concat "  " (string-join subs ", ")))))

(defun hermes-chat--commands-subcommands-content (result)
  "Return readable subcommand catalog section from RESULT."
  (let* ((sub (hermes-transport--get result 'sub))
         (entries (hermes-chat--maplike-entries sub))
         (lines (delq nil
                      (mapcar #'hermes-chat--format-subcommand-entry entries))))
    (and lines
         (string-join (cons "Subcommands" lines) "\n"))))

(defun hermes-chat--command-name (value)
  "Return VALUE as a bare slash command name, or nil."
  (and-let* ((name (hermes-chat--scalar-string value)))
    (hermes-transport--non-empty-string (string-remove-prefix "/" name))))

(defun hermes-chat--catalog-pairs-candidates (pairs)
  "Return (NAME . DESCRIPTION) cells for catalog PAIRS."
  (delq nil
        (mapcar
         (lambda (pair)
           (when-let* ((name (hermes-chat--command-name
                              (hermes-chat--pair-command pair))))
             (cons name (hermes-chat--scalar-string
                         (hermes-chat--pair-description pair)))))
         (hermes-chat--listify pairs))))

(defun hermes-chat--catalog-candidates (result)
  "Return an alist of (NAME . DESCRIPTION) slash commands from catalog RESULT."
  (let ((candidates
         (append
          (mapcan (lambda (category)
                    (hermes-chat--catalog-pairs-candidates
                     (hermes-transport--get category 'pairs)))
                  (hermes-chat--listify
                   (hermes-transport--get result 'categories)))
          (hermes-chat--catalog-pairs-candidates
           (hermes-transport--get result 'pairs)))))
    (cl-delete-duplicates candidates :key #'car :test #'equal :from-end t)))

(defun hermes-chat--commands-catalog-content (result)
  "Return readable command catalog RESULT content."
  (let ((warning (hermes-chat--result-string result 'warning)))
    (string-join
     (delq nil
           (list (and (hermes-transport--non-empty-string warning)
                      (format "Warning: %s" warning))
                 (hermes-chat--commands-categories-content result)
                 (hermes-chat--commands-subcommands-content result)))
     "\n\n")))

(defun hermes-chat--parse-slash (content)
  "Return (NAME . ARG) when CONTENT is a slash command."
  (when (string-prefix-p "/" content)
    (let* ((rest (substring content 1))
           (space (string-match-p "[ \t\n]" rest)))
      (if space
          (cons (downcase (substring rest 0 space))
                (string-trim-left (substring rest space)))
        (cons (downcase rest) "")))))

(defun hermes-chat--refresh-goal-after-command (name)
  "Refresh goal state when slash command NAME may have changed it."
  (when (string-equal name "goal")
    (hermes-chat--dashboard-refresh-goal)))

(defun hermes-chat--reasoning-flags (flags result)
  "Return FLAGS with effective reasoning from config RESULT."
  (if-let* ((effort (hermes-transport--non-empty-string
                     (hermes-transport--scalar-string
                      (hermes-transport--get result 'value)))))
      (plist-put (copy-sequence flags) :reasoning-effort effort)
    flags))

(defun hermes-chat--command-context (client)
  "Return current command ownership context for CLIENT."
  (list :client client
        :generation hermes-chat--lifecycle-generation
        :session-id hermes-chat--dashboard-active-session-id))

(defun hermes-chat--command-context-current-p (context)
  "Return non-nil when command CONTEXT still owns this chat."
  (hermes-chat--dashboard-context-current-p
   (plist-get context :client)
   (plist-get context :generation)
   (plist-get context :session-id)))

(defun hermes-chat--refresh-reasoning-after-command (name context)
  "Refresh effective reasoning when command NAME in CONTEXT may have changed it."
  (when (and (string-equal name "reasoning")
             (hermes-chat--command-context-current-p context))
    (let ((buffer (current-buffer))
          (client (plist-get context :client))
          (session-id (plist-get context :session-id)))
      (hermes-dashboard-transport-config-get
       client "reasoning"
       :session-id session-id
       :resolve
       (lambda (result)
         (hermes-chat--in-buffer buffer
           (when (hermes-chat--command-context-current-p context)
             (setq hermes-chat--runtime-flags
                   (hermes-chat--reasoning-flags
                    hermes-chat--runtime-flags result))
             (force-mode-line-update))))
       :reject #'ignore))))

(defun hermes-chat--refresh-state-after-command (name context)
  "Refresh structured state that command NAME in CONTEXT may have changed."
  (hermes-chat--refresh-goal-after-command name)
  (hermes-chat--refresh-reasoning-after-command name context))

(defun hermes-chat--dashboard-dispatch-command
    (name arg &optional preserve-content context)
  "Dispatch dashboard command NAME with ARG and render its result.
PRESERVE-CONTENT is restored if session bootstrap fails before dispatch.
CONTEXT, when non-nil, retains ownership from a failed `slash.exec' request."
  (let ((buffer (current-buffer))
        (raw (or preserve-content (hermes-chat--alias-content name arg))))
    (when (or (null context) (hermes-chat--command-context-current-p context))
      (hermes-chat--with-dashboard-session
       raw buffer
       (lambda (live-client)
         (let ((request-context (or context
                                    (hermes-chat--command-context live-client))))
           (when (hermes-chat--command-context-current-p request-context)
             (hermes-dashboard-transport-command-dispatch
              live-client name arg
              :session-id (plist-get request-context :session-id)
              :resolve
              (lambda (result)
                (hermes-chat--in-buffer buffer
                  (when (hermes-chat--command-context-current-p request-context)
                    (hermes-chat--handle-command-result result arg)
                    (hermes-chat--refresh-state-after-command
                     name request-context))))
              :reject
              (lambda (message)
                (hermes-chat--in-buffer buffer
                  (when (hermes-chat--command-context-current-p request-context)
                    (hermes-chat--command-error message))))))))))))

(defun hermes-chat--reasoning-request (arg)
  "Return (VALUE . SCOPE) for reasoning ARG.
SCOPE is \"global\" only when ARG contains `--global'.  `--session' is an
accepted explicit spelling of the default session scope."
  (let* ((tokens (split-string arg "[ \t\n]+" t))
         (scope (and (member "--global" tokens) "global"))
         (value (string-join
                 (cl-remove-if (lambda (token)
                                 (member token '("--global" "--session")))
                               tokens)
                 " ")))
    (and (hermes-transport--non-empty-string value)
         (cons value scope))))

(defun hermes-chat--dashboard-set-reasoning (arg)
  "Set reasoning ARG for its requested scope, then refresh the owned session."
  (let ((buffer (current-buffer))
        (preserve-content (concat "/reasoning " arg))
        (request (hermes-chat--reasoning-request arg)))
    (hermes-chat--with-dashboard-session
     preserve-content buffer
     (lambda (live-client)
       (let ((context (hermes-chat--command-context live-client)))
         (hermes-dashboard-transport-config-set
          live-client "reasoning" (car request)
          :session-id (and (not (cdr request))
                           (plist-get context :session-id))
          :resolve
          (lambda (_result)
            (hermes-chat--in-buffer buffer
              (when (hermes-chat--command-context-current-p context)
                (hermes-chat--refresh-reasoning-after-command
                 "reasoning" context))))
          :reject
          (lambda (message)
            (hermes-chat--in-buffer buffer
              (when (hermes-chat--command-context-current-p context)
                (hermes-chat--command-error message))))))))))

(defun hermes-chat--dashboard-slash-exec (name arg raw)
  "Run RAW slash command for NAME and ARG, using native state paths when available."
  (let ((reasoning-request (and (string-equal name "reasoning")
                                (hermes-chat--reasoning-request arg))))
    (if reasoning-request
        (hermes-chat--dashboard-set-reasoning arg)
      (let ((buffer (current-buffer))
            (preserve-content (concat "/" raw)))
        (hermes-chat--with-dashboard-session
         preserve-content buffer
         (lambda (live-client)
           (let ((context (hermes-chat--command-context live-client)))
             (hermes-dashboard-transport-slash-exec
              live-client raw
              :session-id (plist-get context :session-id)
              :resolve
              (lambda (result)
                (hermes-chat--in-buffer buffer
                  (when (hermes-chat--command-context-current-p context)
                    (hermes-chat--handle-command-result result arg)
                    (hermes-chat--refresh-state-after-command name context))))
              :reject
              (lambda (_message)
                (hermes-chat--in-buffer buffer
                  (when (hermes-chat--command-context-current-p context)
                    (hermes-chat--dashboard-dispatch-command
                     name arg preserve-content context))))))))))))

(defun hermes-chat--fetch-commands-catalog ()
  "Fetch the slash command catalog into the buffer cache, when connected."
  (when (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    (let ((buffer (current-buffer)))
      (hermes-dashboard-transport-commands-catalog
       hermes-chat--dashboard-client
       :resolve (lambda (result)
                  (hermes-chat--in-buffer buffer
                    (setq hermes-chat--commands-cache
                          (hermes-chat--catalog-candidates result))))))))

(defun hermes-chat--command-candidates ()
  "Return cached slash command candidates, fetching the catalog if needed."
  (unless hermes-chat--commands-cache
    (hermes-chat--fetch-commands-catalog))
  hermes-chat--commands-cache)

(defun hermes-chat-refresh-commands ()
  "Refresh the cached slash command catalog from the dashboard."
  (interactive)
  (setq hermes-chat--commands-cache nil)
  (hermes-chat--fetch-commands-catalog))

(defun hermes-chat--slash-completion-bounds ()
  "Return (START . END) of the slash command name at point, or nil.
Only matches while typing the /command word in the writable input tail."
  (let ((input (hermes-chat--input-position)))
    (and input
         (hermes-chat--point-in-input-p)
         (> (point) input)
         (eq (char-after input) ?/)
         (let ((name-start (1+ input)))
           (and (>= (point) name-start)
                (not (string-match-p
                      "[ \t\n]"
                      (buffer-substring-no-properties name-start (point))))
                (cons name-start (point)))))))

(defun hermes-chat--slash-capf ()
  "Completion-at-point for Hermes slash commands in the input tail."
  (when-let* ((bounds (hermes-chat--slash-completion-bounds))
              (candidates (hermes-chat--command-candidates)))
    (list (car bounds) (cdr bounds)
          (mapcar #'car candidates)
          :exclusive 'no
          :annotation-function
          (lambda (cand)
            (when-let* ((desc (cdr (assoc cand candidates))))
              (concat "  " desc))))))

(defun hermes-chat--file-ref-completion-bounds ()
  "Return completion bounds after an @ file-ref prefix in the input tail."
  (when (and (hermes-chat--point-in-input-p)
             (hermes-chat--input-position))
    (let ((end (point))
          (input (hermes-chat--input-position)))
      (save-excursion
        (skip-chars-backward "^ \t\n" input)
        (when (and (< (point) end) (eq (char-after) ?@))
          (cons (1+ (point)) end))))))

(defun hermes-chat--project-file-candidates ()
  "Return project-relative file names for composer @ completion."
  (when-let* ((project (project-current nil))
              (root (project-root project)))
    (mapcar (lambda (file) (file-relative-name file root))
            (project-files project))))

(defun hermes-chat--file-ref-capf ()
  "Completion-at-point for project file references after @ in the input tail."
  (when-let* ((bounds (hermes-chat--file-ref-completion-bounds))
              (candidates (hermes-chat--project-file-candidates)))
    (list (car bounds) (cdr bounds) candidates :exclusive 'no)))

(defun hermes-chat-show-commands ()
  "Fetch and display the dashboard slash command catalog."
  (interactive)
  (let ((buffer (current-buffer))
        (client (hermes-chat--dashboard-control-client)))
    (hermes-dashboard-transport-commands-catalog
     client
     :resolve (lambda (result)
                (hermes-chat--in-buffer buffer
                  (hermes-chat--insert-local-status
                   (hermes-chat--commands-catalog-content result) 'done)))
     :reject (lambda (message)
               (hermes-chat--in-buffer buffer
                 (hermes-chat--command-error message))))))

(defvar hermes-chat--native-slash-commands nil
  "Native in-client slash commands as (NAMES . HANDLER) entries.
NAMES is a list of aliases; HANDLER takes the command's ARG string (empty
when none).  Populated by `hermes-chat', which owns the commands the
handlers call; names absent here fall through to the gateway via
`hermes-chat--dashboard-slash-exec'.")

(defun hermes-chat--native-slash-handler (name)
  "Return the native handler for slash command NAME, or nil when none.
NAME is matched against each alias list in `hermes-chat--native-slash-commands'."
  (and name
       (cdr (cl-find-if (lambda (entry) (member name (car entry)))
                        hermes-chat--native-slash-commands))))

(defun hermes-chat--handle-slash-content (content)
  "Handle slash command CONTENT from the input tail.
Native control commands run in-client through
`hermes-chat--native-slash-commands'; everything else dispatches to the gateway
via `hermes-chat--dashboard-slash-exec'."
  (pcase-let ((`(,name . ,arg) (hermes-chat--parse-slash content)))
    (hermes-chat--delete-input-tail)
    (if-let* ((handler (hermes-chat--native-slash-handler name)))
        (funcall handler (or arg ""))
      (hermes-chat--dashboard-slash-exec name arg (substring content 1)))))
;;; Command results

;; Dispatch/alias/skill/prefill result handling lives with the slash
;; dispatch that produces the results.

(defun hermes-chat--result-type (result)
  "Return command RESULT's lower-case type string."
  (when-let* ((type (hermes-chat--result-string result 'type)))
    (downcase type)))

(defun hermes-chat--result-output (result)
  "Return display output from command RESULT."
  (let ((warning (hermes-transport--non-empty-string
                  (hermes-chat--result-string result 'warning)))
        (body (cl-some
               (lambda (key)
                 (hermes-transport--non-empty-string
                  (hermes-chat--result-string result key)))
               '(output notice message target))))
    (cond
     ((and warning body) (format "warning: %s\n%s" warning body))
     (body)
     (warning (format "warning: %s" warning)))))

(defun hermes-chat--alias-content (target arg)
  "Return slash content for alias TARGET with original ARG."
  (when-let* ((command (hermes-transport--non-empty-string
			(string-trim (or target "")))))
    (string-join
     (delq nil (list (concat "/" (string-remove-prefix "/" command))
                     (hermes-transport--non-empty-string arg)))
     " ")))

(defun hermes-chat--handle-alias-result (target arg)
  "Follow command-dispatch alias TARGET with original ARG."
  (if-let* ((content (hermes-chat--alias-content target arg))
            (parsed (hermes-chat--parse-slash content)))
      (pcase-let ((`(,name . ,next-arg) parsed))
        (hermes-chat--dashboard-slash-exec name next-arg (substring content 1)))
    (user-error "Command alias target missing")))

(defun hermes-chat--handle-send-result (message &optional notice)
  "Handle command-dispatch MESSAGE with optional NOTICE."
  (when (hermes-transport--non-empty-string notice)
    (hermes-chat--insert-local-status notice 'done))
  (cond
   ((not (hermes-transport--non-empty-string message))
    (user-error "Command returned no message to send"))
   ((hermes-chat--active-turn-p)
    (hermes-chat--queue-content message))
   (t
    (hermes-chat--dashboard-queue-or-submit message (current-buffer)))))

(defun hermes-chat--handle-skill-result (message name)
  "Send skill MESSAGE to the agent, echoing a compact loading line for NAME.
The dispatch returns the full skill payload (the agent needs it); the
transcript shows only \"loading skill: NAME\", not the whole skill."
  (unless (hermes-transport--non-empty-string message)
    (user-error "Skill returned no content to load"))
  (let ((display (format "⚡ loading skill: %s"
                         (or (hermes-transport--non-empty-string name) "skill"))))
    (hermes-chat--dashboard-queue-or-submit message (current-buffer) display)))

(defun hermes-chat--prefill-input (message)
  "Replace the input tail with MESSAGE."
  (hermes-chat--delete-input-tail)
  (insert (or message "")))

(defun hermes-chat--handle-prefill-result (message notice)
  "Handle command-dispatch prefill MESSAGE with optional NOTICE."
  (when (hermes-transport--non-empty-string notice)
    (hermes-chat--insert-local-status notice 'done))
  (hermes-chat--prefill-input message))

(defun hermes-chat--handle-command-result (result &optional arg)
  "Render or act on a dashboard command RESULT using optional ARG."
  (pcase (hermes-chat--result-type result)
    ("alias"
     (hermes-chat--handle-alias-result
      (hermes-chat--result-string result 'target) arg))
    ("send"
     (hermes-chat--handle-send-result
      (hermes-chat--result-string result 'message)
      (hermes-chat--result-string result 'notice)))
    ("skill"
     (hermes-chat--handle-skill-result
      (hermes-chat--result-string result 'message)
      (hermes-chat--result-string result 'name)))
    ("prefill"
     (hermes-chat--handle-prefill-result
      (hermes-chat--result-string result 'message)
      (hermes-chat--result-string result 'notice)))
    (_
     (when-let* ((output (hermes-chat--result-output result)))
       (hermes-chat--insert-local-status output 'done)))))

(defun hermes-chat--format-command-pair (pair)
  "Return a readable catalog line for PAIR."
  (let ((name (hermes-chat--scalar-string (hermes-chat--pair-command pair)))
        (desc (hermes-chat--scalar-string (hermes-chat--pair-description pair))))
    (string-join (delq nil (list name desc)) " — ")))

(defun hermes-chat--format-command-category (category)
  "Return readable command catalog text for CATEGORY."
  (let* ((name (or (hermes-chat--result-string category 'name) "Commands"))
         (pairs (hermes-chat--listify (hermes-transport--get category 'pairs)))
         (lines (mapcar #'hermes-chat--format-command-pair pairs)))
    (string-join (cons name (mapcar (lambda (line) (concat "  " line)) lines))
                 "\n")))

(defun hermes-chat--commands-categories-content (result)
  "Return readable command categories from catalog RESULT."
  (let ((categories (hermes-chat--listify
                     (hermes-transport--get result 'categories))))
    (if categories
        (string-join (mapcar #'hermes-chat--format-command-category categories)
                     "\n\n")
      (hermes-chat--format-command-category
       `((name . "Commands") (pairs . ,(hermes-transport--get result 'pairs)))))))


(provide 'hermes-chat-slash)
;;; hermes-chat-slash.el ends here
