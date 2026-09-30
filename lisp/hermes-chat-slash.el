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
(require 'hermes-chat-models)


(defvar hermes-chat--dashboard-active-session-id)
(defvar hermes-chat--dashboard-client)
(defvar hermes-chat--dashboard-create-reasoning-effort)
(defvar hermes-chat--lifecycle-generation)
(defvar hermes-chat--runtime-flags)

(defconst hermes-chat--reasoning-efforts
  '("none" "minimal" "low" "medium" "high" "xhigh" "max" "ultra")
  "Reasoning effort values accepted by the Hermes dashboard.")

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

(defun hermes-chat--command-pair-description (pair)
  "Return PAIR description, accounting for native command behavior."
  (if (equal (hermes-chat--command-name (hermes-chat--pair-command pair)) "stop")
      "Confirm stopping all processes in the connected Hermes instance (all chats)"
    (hermes-chat--scalar-string (hermes-chat--pair-description pair))))

(defun hermes-chat--catalog-pairs-candidates (pairs)
  "Return (NAME . DESCRIPTION) cells for catalog PAIRS."
  (delq nil
        (mapcar
         (lambda (pair)
           (when-let* ((name (hermes-chat--command-name
                              (hermes-chat--pair-command pair))))
             (cons name (hermes-chat--command-pair-description pair))))
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

(defvar-local hermes-chat--command-owner nil
  "Identity-bearing plist owning the session during a slash command RPC.
Its :input-edited slot records composer changes since command admission.")

(defun hermes-chat--command-owner-current-p (owner)
  "Return non-nil when OWNER owns the current command operation."
  (and owner (eq owner hermes-chat--command-owner)))

(defun hermes-chat--capture-command-terminal-owner ()
  "Return plain terminal authority for the current command operation."
  (list :owner hermes-chat--command-owner))

(defun hermes-chat--take-command-terminal-owner (snapshot)
  "Clear the exact command owner in SNAPSHOT and return no effects."
  (let ((owner (plist-get snapshot :owner)))
    (when (hermes-chat--command-owner-current-p owner)
      (hermes-chat--command-stop owner)))
  nil)

(defun hermes-chat--command-input-changing (beg end)
  "Record a composer edit between BEG and END for the current command."
  (when-let* ((owner hermes-chat--command-owner)
              (input (hermes-chat--input-position)))
    (when (or (>= beg input) (> end input))
      (setf (plist-get owner :input-edited) t))))

(defun hermes-chat--command-start ()
  "Acquire and return exclusive ownership for a command operation."
  (when hermes-chat--command-owner
    (user-error "A session command is already in progress"))
  (add-hook 'before-change-functions #'hermes-chat--command-input-changing nil t)
  (setq hermes-chat--command-owner
        (list :input-edited (save-restriction
                             (widen)
                             (not (string-empty-p (hermes-chat-input-string)))))))

(defun hermes-chat--command-stop (&optional owner)
  "Release command ownership when optional OWNER remains current."
  (when (or (null owner) (hermes-chat--command-owner-current-p owner))
    (remove-hook 'before-change-functions #'hermes-chat--command-input-changing t)
    (setq hermes-chat--command-owner nil)))

(defun hermes-chat--command-submit-inhibit-reason ()
  "Return the command submission guard while its RPC is in flight."
  (and hermes-chat--command-owner "A session command is in progress"))

(defun hermes-chat--command-run-owned (content action)
  "Run ACTION with a live client and exclusive owner for CONTENT."
  (let ((buffer (current-buffer))
        (owner (hermes-chat--command-start)))
    (condition-case err
        (hermes-chat--with-dashboard-session
         content buffer
         (lambda (client)
           (condition-case callback-error
               (funcall action client owner)
             (error
              (hermes-chat--command-stop owner)
              (signal (car callback-error) (cdr callback-error)))))
         (lambda (message)
           (hermes-chat--command-stop owner)
           (hermes-chat--dashboard-bootstrap-error message content)))
      (error
       (hermes-chat--command-stop owner)
       (signal (car err) (cdr err))))))

(defun hermes-chat--command-finish (context action)
  "Release current command CONTEXT, then run its synchronous ACTION."
  (let ((owner (plist-get context :owner))
        (current (hermes-chat--command-context-current-p context)))
    (when (hermes-chat--command-owner-current-p owner)
      (hermes-chat--command-stop owner)
      (when current
        (funcall action)))))

(defun hermes-chat--command-context (client &optional owner)
  "Return current command ownership context for CLIENT and optional OWNER."
  (list :client client
        :generation hermes-chat--lifecycle-generation
        :session-id hermes-chat--dashboard-active-session-id
        :owner owner))

(defun hermes-chat--command-context-current-p (context)
  "Return non-nil when command CONTEXT still owns this chat."
  (let ((owner (plist-get context :owner)))
    (and (hermes-chat--dashboard-context-current-p
          (plist-get context :client)
          (plist-get context :generation)
          (plist-get context :session-id))
         (or (null owner)
             (hermes-chat--command-owner-current-p owner)))))

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

(defun hermes-chat--command-result (context name arg result)
  "Handle RESULT for NAME and ARG under command CONTEXT."
  (hermes-chat--command-finish
   context
   (lambda ()
     (hermes-chat--handle-command-result
      result arg (let ((owner (plist-get context :owner)))
                   ;; Pre-reload commands have no composer edit evidence.
                   (or (not (listp owner)) (plist-get owner :input-edited))))
     (hermes-chat--refresh-state-after-command
      name (plist-put (copy-sequence context) :owner nil)))))

(defun hermes-chat--command-rejection (context message)
  "Handle MESSAGE as the terminal rejection for command CONTEXT."
  (hermes-chat--command-finish
   context (lambda () (hermes-chat--command-error message))))

(defun hermes-chat--dashboard-dispatch-command-request
    (client name arg context buffer)
  "Dispatch NAME with ARG through CLIENT under CONTEXT for BUFFER."
  (hermes-dashboard-transport-command-dispatch
   client name arg
   :session-id (plist-get context :session-id)
   :resolve (lambda (result)
              (hermes-chat--in-buffer buffer
                (hermes-chat--command-result context name arg result)))
   :reject (lambda (message)
             (hermes-chat--in-buffer buffer
               (hermes-chat--command-rejection context message)))))

(defun hermes-chat--dashboard-dispatch-command
    (name arg &optional preserve-content context)
  "Dispatch dashboard command NAME with ARG and render its result.
PRESERVE-CONTENT is restored if session bootstrap fails before dispatch.
CONTEXT, when non-nil, retains ownership from a failed `slash.exec' request."
  (let ((buffer (current-buffer))
        (raw (or preserve-content (hermes-chat--alias-content name arg))))
    (if context
        (when (hermes-chat--command-context-current-p context)
          (condition-case err
              (hermes-chat--dashboard-dispatch-command-request
               (plist-get context :client) name arg context buffer)
            (error
             (hermes-chat--command-stop (plist-get context :owner))
             (signal (car err) (cdr err)))))
      (hermes-chat--command-run-owned
       raw (lambda (client owner)
             (hermes-chat--dashboard-dispatch-command-request
              client name arg (hermes-chat--command-context client owner)
              buffer))))))

(defun hermes-chat--reasoning-report (result)
  "Return the effort and display reported by session config RESULT.
Do not infer defaults or full/clamped display from fields this RPC omits."
  (let ((effort (hermes-transport--non-empty-string
                 (hermes-chat--result-string result 'value)))
        (display (hermes-transport--non-empty-string
                  (hermes-chat--result-string result 'display))))
    (format "Reasoning effort:  %s\nReasoning display: %s"
            (if (equal effort "none") "none (disabled)" (or effort "unknown"))
            (pcase display
              ("show" "on")
              ("hide" "off")
              (_ (or display "unknown"))))))

(defun hermes-chat--dashboard-query-reasoning ()
  "Report reasoning from the owning session, not the isolated slash worker."
  (let ((buffer (current-buffer)))
    (hermes-chat--command-run-owned
     "/reasoning"
     (lambda (client owner)
       (let ((context (hermes-chat--command-context client owner)))
         (hermes-dashboard-transport-config-get
          client "reasoning" :session-id (plist-get context :session-id)
          :resolve
          (lambda (result)
            (hermes-chat--in-buffer buffer
              (hermes-chat--command-finish
               context
               (lambda ()
                 (setq hermes-chat--runtime-flags
                       (hermes-chat--reasoning-flags hermes-chat--runtime-flags result))
                 (hermes-chat--handle-command-result
                  `((output . ,(hermes-chat--reasoning-report result))))
                 (force-mode-line-update)))))
          :reject
          (lambda (message)
            (hermes-chat--in-buffer buffer
              (hermes-chat--command-rejection context message)))))))))

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
    (hermes-chat--command-run-owned
     preserve-content
     (lambda (client owner)
       (let ((context (hermes-chat--command-context client owner)))
         (hermes-dashboard-transport-config-set
          client "reasoning" (car request)
          :session-id (and (not (cdr request))
                           (plist-get context :session-id))
          :resolve
          (lambda (_result)
            (hermes-chat--in-buffer buffer
              (hermes-chat--command-finish
               context
               (lambda ()
                 (hermes-chat--refresh-reasoning-after-command
                  "reasoning" (plist-put (copy-sequence context) :owner nil))))))
          :reject
          (lambda (message)
            (hermes-chat--in-buffer buffer
              (hermes-chat--command-rejection context message)))))))))

(defun hermes-chat--reasoning-setting-value (&optional label)
  "Return the current chat's reasoning value, optionally prefixed with LABEL."
  (let ((pending (and (hermes-chat--pending-setting-p)
                      hermes-chat--dashboard-create-reasoning-effort)))
    (hermes-chat--setting-value
     (or pending (plist-get hermes-chat--runtime-flags :reasoning-effort))
     pending label)))

(defun hermes-chat--read-reasoning-effort ()
  "Read reasoning with a known default, refusing a changed prompt owner."
  (when (hermes-chat--active-turn-p)
    (user-error "Interrupt the active turn before changing reasoning"))
  (hermes-chat--require-setting-session)
  (let* ((buffer (current-buffer))
         (client hermes-chat--dashboard-client)
         (generation hermes-chat--lifecycle-generation)
         (transport hermes-chat--transport-generation)
         (session hermes-chat--dashboard-active-session-id)
         (default (or (and (hermes-chat--pending-setting-p)
                           hermes-chat--dashboard-create-reasoning-effort)
                      (plist-get hermes-chat--runtime-flags :reasoning-effort)))
         (effort (completing-read
                  (format "Reasoning effort (current: %s): "
                          (hermes-chat--reasoning-setting-value))
                  hermes-chat--reasoning-efforts nil t nil nil
                  (and (member default hermes-chat--reasoning-efforts) default))))
    (unless (and (eq buffer (current-buffer))
                 (hermes-chat--dashboard-context-current-p client generation)
                 (equal session hermes-chat--dashboard-active-session-id)
                 (= transport hermes-chat--transport-generation))
      (user-error "Hermes reasoning prompt is no longer current"))
    effort))

(defun hermes-chat-set-reasoning (&optional effort)
  "Set reasoning EFFORT for this session or its first turn.
In a fresh chat, store the choice locally and apply it after `session.create'
but before the first prompt.  A live chat uses the owned session command path.
Detached sessions must reconnect or resume first, except for an owned
failed-create retry."
  (interactive (list (hermes-chat--read-reasoning-effort)) hermes-chat-mode)
  (unless (member effort hermes-chat--reasoning-efforts)
    (user-error "Unsupported reasoning effort: %s" effort))
  (when (hermes-chat--active-turn-p)
    (user-error "Interrupt the active turn before changing reasoning"))
  (hermes-chat--require-setting-session)
  (if (hermes-chat--dashboard-session-attached-p)
      (hermes-chat--dashboard-set-reasoning effort)
    (setq hermes-chat--dashboard-create-reasoning-effort effort
          hermes-chat--runtime-flags
          (plist-put hermes-chat--runtime-flags :reasoning-effort effort))
    (force-mode-line-update)
    (hermes-chat--insert-local-status
     (format "Reasoning set to %s (applies to next session)" effort) 'ready)))

(defun hermes-chat--slash-model-name (arg)
  "Return the model token from `/model' ARG for status display."
  (or (car (split-string arg "[ \t\n]+" t)) arg))

(defun hermes-chat--slash-model-request (context arg confirmed buffer)
  "Set model ARG under CONTEXT for BUFFER with optional CONFIRMED consent."
  (hermes-dashboard-transport-config-set
   (plist-get context :client) "model" arg
   :session-id (plist-get context :session-id)
   :confirm-expensive-model confirmed
   :resolve
   (lambda (result)
     (hermes-chat--in-buffer buffer
       (hermes-chat--slash-model-result context arg result confirmed)))
   :reject
   (lambda (message)
     (hermes-chat--in-buffer buffer
       (hermes-chat--command-rejection context message)))))

(defun hermes-chat--slash-model-confirm (context arg result)
  "Prompt to confirm model ARG under its original CONTEXT using RESULT."
  (let ((owner (plist-get context :owner))
        (buffer (current-buffer))
        prompt-returned accepted)
    (unwind-protect
        (progn
          (setq accepted
                (yes-or-no-p
                 (or (hermes-transport--scalar-string
                      (hermes-transport--get result 'confirm_message))
                     "Confirm switching to this model? "))
                prompt-returned t)
          (cond
           ((not (hermes-chat--command-context-current-p context))
            (hermes-chat--command-stop owner))
           (accepted
            (condition-case err
                (hermes-chat--slash-model-request context arg t buffer)
              (error
               (hermes-chat--command-stop owner)
               (signal (car err) (cdr err)))))
           (t
            (hermes-chat--command-finish
             context
             (lambda ()
               (hermes-chat--insert-local-status
                "Model switch cancelled" 'ready))))))
      (unless prompt-returned
        (hermes-chat--command-stop owner)))))

(defun hermes-chat--slash-model-result (context arg result confirmed)
  "Handle model ARG RESULT under CONTEXT after optional CONFIRMED consent."
  (if (hermes-transport--get result 'confirm_required)
      (if confirmed
          (hermes-chat--command-finish
           context
           (lambda ()
             (hermes-chat--command-error
              "Model switch still requires confirmation")))
        (if (hermes-chat--command-context-current-p context)
            (hermes-chat--slash-model-confirm context arg result)
          (hermes-chat--command-stop (plist-get context :owner))))
    (hermes-chat--command-finish
     context
     (lambda ()
       (hermes-chat--insert-local-status
        (format "Model set to %s" (hermes-chat--slash-model-name arg))
        'ready)))))

(defun hermes-chat--dashboard-set-model (arg &optional confirmed)
  "Set model ARG on the owned dashboard session.
CONFIRMED acknowledges a prior expensive-model warning."
  (let ((buffer (current-buffer))
        (content (concat "/model " arg)))
    (hermes-chat--command-run-owned
     content
     (lambda (client owner)
       (hermes-chat--slash-model-request
        (hermes-chat--command-context client owner)
        arg confirmed buffer)))))

(defun hermes-chat--dashboard-slash-exec (name arg raw)
  "Run RAW slash command for NAME and ARG, using native state paths when available."
  (let ((reasoning-request (and (string-equal name "reasoning")
                                (hermes-chat--reasoning-request arg))))
    (cond
     (reasoning-request (hermes-chat--dashboard-set-reasoning arg))
     ((and (string-equal name "reasoning") (string-empty-p (string-trim arg)))
      (hermes-chat--dashboard-query-reasoning))
     (t
      (let ((buffer (current-buffer))
            (preserve-content (concat "/" raw)))
        (hermes-chat--command-run-owned
         preserve-content
         (lambda (client owner)
           (let ((context (hermes-chat--command-context client owner)))
             (hermes-dashboard-transport-slash-exec
              client raw
              :session-id (plist-get context :session-id)
              :resolve
              (lambda (result)
                (hermes-chat--in-buffer buffer
                  (hermes-chat--command-result context name arg result)))
              :reject
              (lambda (_message)
                (hermes-chat--in-buffer buffer
                  (if (hermes-chat--command-context-current-p context)
                      (hermes-chat--dashboard-dispatch-command
                       name arg preserve-content context)
                    (hermes-chat--command-stop owner)))))))))))))

(defun hermes-chat--fetch-commands-catalog ()
  "Fetch the slash command catalog into the buffer cache, when connected."
  (when (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    (let ((buffer (current-buffer))
          (lifetime hermes-chat--lifecycle-generation))
      (hermes-dashboard-transport-commands-catalog
       hermes-chat--dashboard-client
       :resolve (lambda (result)
                  (hermes-chat--in-lifetime buffer lifetime
                    (setq hermes-chat--commands-cache
                          (hermes-chat--catalog-candidates result))))))))

(defun hermes-chat--command-candidates ()
  "Return cached slash command candidates, fetching the catalog if needed."
  (unless hermes-chat--commands-cache
    (hermes-chat--fetch-commands-catalog))
  hermes-chat--commands-cache)

(defun hermes-chat-refresh-commands ()
  "Refresh the cached slash command catalog from the dashboard."
  (interactive nil hermes-chat-mode)
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

(defvar-local hermes-chat--path-completion nil
  "Exact pending or accepted backend path completion snapshot.")

(defun hermes-chat--file-ref-completion-bounds ()
  "Return bounds of an @ path query at point in the composer.
Accept explicit file/folder prefixes, including a quoted path with spaces."
  (when (and (hermes-chat--point-in-input-p) (hermes-chat--input-position))
    (let ((end (point)))
      (save-excursion
        (when (search-backward "@" (hermes-chat--input-position) t)
          (let ((start (point))
                (word (buffer-substring-no-properties (point) end)))
            (when (and (or (= start (hermes-chat--input-position))
                           (memq (char-before) '(?\s ?\t ?\n)))
                       (not (string-match-p "[\n\r]" word))
                       (or (not (string-match-p "[ \t]" word))
                           (string-match-p "\\`@\\(?:file\\|folder\\):[`\"']" word)))
              (cons start end))))))))

(defun hermes-chat--path-query (word)
  "Remove the optional reference quoting from completion WORD."
  (if (string-match "\\`\\(@\\(?:file\\|folder\\):\\)\\([`\"']\\)\\(.*\\)\\'" word)
      (let ((prefix (match-string 1 word)) (quote (match-string 2 word))
            (path (match-string 3 word)))
        (concat prefix (if (string-suffix-p quote path) (substring path 0 -1) path)))
    word))

(defun hermes-chat--path-reference (item)
  "Return a representable file/folder reference for backend ITEM, or nil."
  (when-let* ((text (hermes-transport--get item 'text))
              ((stringp text))
              ((string-match "\\`@\\(file\\|folder\\):\\(.+\\)\\'" text)))
    (let ((kind (match-string 1 text)) (path (match-string 2 text)))
      (unless (string-match-p "[\n\r\0]" path)
        (when-let* ((quote (seq-find (lambda (q) (not (string-search q path)))
                                    '("`" "\"" "'"))))
          (concat "@" kind ":" quote path quote))))))

(defun hermes-chat--path-completion-context-p (snapshot)
  "Return non-nil when SNAPSHOT still owns the backend context."
  (and (eq snapshot hermes-chat--path-completion)
       (hermes-chat--dashboard-context-current-p
        (plist-get snapshot :client) (plist-get snapshot :lifetime)
        (plist-get snapshot :session))
       (= (plist-get snapshot :connection)
          (hermes-dashboard-transport-client-generation
           (plist-get snapshot :client)))
       (equal (plist-get snapshot :cwd) (hermes-chat--current-working-directory))))

(defun hermes-chat--path-completion-current-p (snapshot)
  "Return non-nil when SNAPSHOT still owns the composer and backend."
  (and (hermes-chat--path-completion-context-p snapshot)
       (= (plist-get snapshot :tick) (buffer-chars-modified-tick))
       (= (plist-get snapshot :point) (point))))

(defun hermes-chat--path-completion-replace (replace beg end text)
  "Run native REPLACE of BEG through END with TEXT under its CAPF owner.
Native completion has no hook around its common-prefix edit.  Only this
CAPF's exact dynamic owner may advance the edit snapshot; ordinary edits
and other completion tables must not renew it."
  (let ((snapshot (plist-get completion-extra-properties :hermes-path-owner)))
    (if (null snapshot)
        (funcall replace beg end text)
      (unless (hermes-chat--path-completion-current-p snapshot)
        (user-error "Path completion expired; invoke completion again"))
      (let ((expected (concat
                       (buffer-substring-no-properties (hermes-chat--input-position) beg)
                       text (buffer-substring-no-properties end (point-max)))))
        (prog1 (funcall replace beg end text)
          (when (and (hermes-chat--path-completion-context-p snapshot)
                     (equal expected (hermes-chat-input-string)))
            (setf (plist-get snapshot :tick) (buffer-chars-modified-tick)
                  (plist-get snapshot :point) (point))))))))

(defun hermes-chat--request-path-completion (bounds)
  "Request backend candidates for BOUNDS without editing the composer."
  (let* ((buffer (current-buffer))
         (client hermes-chat--dashboard-client)
         (snapshot (list :client client :lifetime hermes-chat--lifecycle-generation
                         :session hermes-chat--dashboard-active-session-id
                         :connection (hermes-dashboard-transport-client-generation client)
                         :cwd (hermes-chat--current-working-directory)
                         :tick (buffer-chars-modified-tick) :point (point)
                         :items nil)))
    (setq hermes-chat--path-completion snapshot)
    (condition-case nil
        (hermes-dashboard-transport-complete-path
         client (hermes-chat--path-query
                 (buffer-substring-no-properties (car bounds) (cdr bounds)))
         :session-id (plist-get snapshot :session) :cwd (plist-get snapshot :cwd)
         :resolve (lambda (result)
                    (hermes-chat--in-buffer buffer
                      (when (hermes-chat--path-completion-current-p snapshot)
                        (setf (plist-get snapshot :items)
                              (delq nil (mapcar #'hermes-chat--path-reference
                                               (hermes-chat--listify
                                                (hermes-transport--get result 'items))))))))
         :reject #'ignore)
      (error nil))
    snapshot))

(defun hermes-chat--guard-path-choice (choice _buffer _base &rest _ignored)
  "Refuse stale native completion CHOICE before it can replace input."
  (when (and hermes-chat--path-completion
             (string-match-p "\\`@\\(?:file\\|folder\\):" choice))
    (unless (and (hermes-chat--path-completion-current-p hermes-chat--path-completion)
                 (member choice (plist-get hermes-chat--path-completion :items)))
      (user-error "Path completion expired; invoke completion again"))))

(defun hermes-chat--file-ref-capf ()
  "Complete @ paths using the attached backend session, never local files.
The first invocation starts a read; invoke completion again when it finishes.
Unavailable or stale reads offer no candidates and never edit the input."
  (when-let* ((bounds (hermes-chat--file-ref-completion-bounds))
              (client hermes-chat--dashboard-client)
              ((hermes-dashboard-transport-client-p client))
              (hermes-chat--dashboard-active-session-id))
    (let* ((buffer (current-buffer))
           (query (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (snapshot (if (and hermes-chat--path-completion
                              (hermes-chat--path-completion-current-p
                               hermes-chat--path-completion))
                         hermes-chat--path-completion
                       (hermes-chat--request-path-completion bounds))))
      (when (plist-get snapshot :items)
        (advice-add 'completion--replace :around #'hermes-chat--path-completion-replace)
        (add-hook 'choose-completion-string-functions
                  #'hermes-chat--guard-path-choice nil t)
        (list (car bounds) (cdr bounds)
              (lambda (string predicate action)
                (when (and (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (hermes-chat--path-completion-current-p snapshot)))
                  (complete-with-action action (plist-get snapshot :items)
                                        (if (equal string query) "" string)
                                        predicate)))
              :hermes-path-owner snapshot :exclusive 'no)))))

(defun hermes-chat-show-commands ()
  "Fetch and display the dashboard slash command catalog."
  (interactive nil hermes-chat-mode)
  (let ((buffer (current-buffer))
        (lifetime hermes-chat--lifecycle-generation)
        (client (hermes-chat--dashboard-control-client)))
    (hermes-dashboard-transport-commands-catalog
     client
     :resolve (lambda (result)
                (hermes-chat--in-lifetime buffer lifetime
                  (hermes-chat--insert-local-status
                   (hermes-chat--commands-catalog-content result) 'done)))
     :reject (lambda (message)
               (hermes-chat--in-lifetime buffer lifetime
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
  (hermes-chat--ensure-submit-allowed)
  (pcase-let ((`(,name . ,arg) (hermes-chat--parse-slash content)))
    (unless (member name '("branch" "btw")) (hermes-chat--delete-input-tail))
    (if-let* ((handler (hermes-chat--native-slash-handler name)))
        (funcall handler (or arg ""))
      (hermes-chat--dashboard-slash-exec name arg (substring content 1)))))

(defconst hermes-chat--compress-entry-id "status:compressing"
  "Session-scoped EWOC id for a live `session.compress' progress line.")

(defun hermes-chat--compress-summary (result)
  "Return display text for a `session.compress' RESULT."
  (let* ((summary (hermes-transport--get result 'summary))
         (headline (hermes-chat--result-string summary 'headline))
         (token-line (hermes-chat--result-string summary 'token_line))
         (note (hermes-chat--result-string summary 'note)))
    (or (hermes-transport--non-empty-string
         (string-join (delq nil (list headline token-line note)) "\n"))
        "Compressed.")))

(defun hermes-chat--compress-show (text status)
  "Show TEXT as the session-scoped compression line with STATUS."
  (if (and hermes-chat--nodes
           (gethash hermes-chat--compress-entry-id hermes-chat--nodes))
      (hermes-chat--update-entry
       hermes-chat--compress-entry-id
       (lambda (entry)
         (hermes-chat--entry-with entry :content text :status status)))
    (hermes-chat--insert-entry
     (hermes-chat--make-entry
      'status text status hermes-chat--compress-entry-id)
     (hermes-chat--pending-assistant-node))))

(defun hermes-chat--dashboard-compress (name arg)
  "Compress the live session as slash NAME, optionally focused on ARG."
  (let* ((focus (hermes-transport--non-empty-string arg))
         (raw (string-join
               (delq nil (list (concat "/" (or name "compact")) focus))
               " ")))
    (hermes-chat--command-run-owned
     raw
     (lambda (client owner)
       (let ((buffer (current-buffer))
             (context (hermes-chat--command-context client owner)))
         (hermes-chat--compress-show "Compressing…" 'running)
         (hermes-dashboard-transport-session-compress
          client
          :session-id (plist-get context :session-id)
          :focus-topic focus
          :resolve
          (lambda (result)
            (hermes-chat--in-buffer buffer
              (hermes-chat--command-finish
               context
               (lambda ()
                 (hermes-chat--compress-show
                  (hermes-chat--compress-summary result) 'done)))))
          :reject
          (lambda (message)
            (hermes-chat--in-buffer buffer
              (hermes-chat--command-finish
               context
               (lambda ()
                 (hermes-chat--compress-show message 'error)))))))))))

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
  (if (hermes-chat--point-in-input-p)
      (hermes-chat--replace-input-tail (or message ""))
    (save-excursion
      (hermes-chat--replace-input-tail (or message "")))))

(defun hermes-chat--handle-prefill-result (message notice &optional input-edited)
  "Handle prefill MESSAGE and NOTICE without replacing edited input.
When INPUT-EDITED is non-nil, retain MESSAGE for explicit manual recovery."
  (when (hermes-transport--non-empty-string notice)
    (hermes-chat--insert-local-status notice 'done))
  (save-restriction
    (widen)
    (if (or input-edited (not (string-empty-p (hermes-chat-input-string))))
        (let ((buffer (generate-new-buffer "*Hermes prefill*")))
          (with-current-buffer buffer
            (insert (or message ""))
            (goto-char (point-min)))
          (hermes-chat--insert-local-status
           (format "Composer preserved; prefill in %s for manual copying"
                   (buffer-name buffer)) 'done)
          (display-buffer buffer))
      (hermes-chat--prefill-input message))))

(defun hermes-chat--handle-command-result (result &optional arg input-edited)
  "Render dashboard command RESULT using ARG and INPUT-EDITED prefill guard."
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
      (hermes-chat--result-string result 'notice) input-edited))
    (_
     (when-let* ((output (hermes-chat--result-output result)))
       (hermes-chat--insert-local-status output 'done)))))

(defun hermes-chat--format-command-pair (pair)
  "Return a readable catalog line for PAIR."
  (let ((name (hermes-chat--scalar-string (hermes-chat--pair-command pair)))
        (desc (hermes-chat--command-pair-description pair)))
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

(hermes-chat-register-submit-inhibit-function
 #'hermes-chat--command-submit-inhibit-reason)
(hermes-chat-register-cleanup-function #'hermes-chat--command-stop)

(provide 'hermes-chat-slash)
;;; hermes-chat-slash.el ends here
