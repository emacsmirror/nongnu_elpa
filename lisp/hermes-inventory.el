;;; hermes-inventory.el --- Inventory browsers for Hermes  -*- lexical-binding: t; -*-

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

;; `tabulated-list' browsers over dashboard inventory methods and safe
;; dashboard actions.  Toolsets and skills can be enabled or disabled from the
;; list; memory status is shown in a separate buffer that displays only provider
;; names and built-in store sizes, never memory contents.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'url-util)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-promise)
(require 'hermes-browser)

(defun hermes-inventory--bool-cell (value &optional unknown)
  "Return an on/off display cell for VALUE.
When VALUE is nil and UNKNOWN is non-nil, return `?' instead
of the string \"off\"."
  (hermes-browser--status-cell
   (cond
    ((eq value t) "on")
    ((and (null value) unknown) "?")
    (t "off"))
   'hermes-browser-enabled))

(defun hermes-inventory--json-bool (value)
  "Return VALUE encoded for `json-serialize' as a JSON boolean."
  (if value t :false))

(defun hermes-inventory--toolset-rows (result)
  "Return inventory rows for a `tools.list' or toolset list RESULT."
  (mapcar (lambda (toolset)
            (let ((name (hermes-transport--display-field toolset 'name))
                  (tool-count (or (hermes-transport--get toolset 'tool_count)
                                  (length (or (hermes-transport--get toolset 'tools)
                                              '())))))
              (list name
                    (vector (hermes-browser--face-cell
                             name 'hermes-browser-name)
                            (hermes-inventory--bool-cell
                             (hermes-transport--get toolset 'enabled))
                            (hermes-browser--face-cell
                             tool-count 'hermes-browser-tool-count)
                            (hermes-browser--face-cell
                             (hermes-transport--display-field
                              toolset 'description)
                             'hermes-browser-description)))))
          (hermes-transport--get result 'toolsets)))

(defun hermes-inventory--skill-object-p (entry)
  "Return non-nil if ENTRY is a dashboard skill object."
  (and (consp entry)
       (or (hermes-transport--get entry 'name)
           (hermes-transport--get entry "name"))))

(defun hermes-inventory--skill-object-row (skill)
  "Return a tabulated-list row for dashboard SKILL metadata."
  (let ((name (hermes-transport--display-field skill 'name)))
    (list name
          (vector (hermes-browser--face-cell
                   (hermes-transport--display-field skill 'category)
                   'hermes-browser-category)
                  (hermes-browser--face-cell name 'hermes-browser-name)
                  (hermes-inventory--bool-cell
                   (hermes-transport--get skill 'enabled))
                  (hermes-browser--face-cell
                   (hermes-transport--display-field skill 'description)
                   'hermes-browser-description)))))

(defun hermes-inventory--skill-group-rows (skills)
  "Return rows for legacy SKILLS grouped by category."
  (mapcan (lambda (entry)
            (let ((category (format "%s" (car entry))))
              (mapcar (lambda (name)
                        (let ((name (or (hermes-transport--scalar-string name)
                                        "")))
                          (list name
                                (vector (hermes-browser--face-cell
                                         category 'hermes-browser-category)
                                        (hermes-browser--face-cell
                                         name 'hermes-browser-name)
                                        (hermes-inventory--bool-cell nil t)
                                        (hermes-browser--face-cell
                                         "" 'hermes-browser-description)))))
                      (cdr entry))))
          skills))

(defun hermes-inventory--skill-rows (result)
  "Return inventory rows for a skill list RESULT.
RESULT may come from dashboard REST `/api/skills' or legacy `skills.manage'."
  (let ((skills (hermes-transport--get result 'skills)))
    (if (and skills (hermes-inventory--skill-object-p (car skills)))
        (mapcar #'hermes-inventory--skill-object-row skills)
      (hermes-inventory--skill-group-rows skills))))

(defun hermes-inventory--skills-result (payload)
  "Return a `skills' result object for REST PAYLOAD.
The dashboard REST endpoint currently returns a raw list; accept an object with
a `skills' field too so older/newer dashboard shapes render the same way."
  (if (hermes-transport--get payload 'skills)
      payload
    `((skills . ,payload))))

(defun hermes-inventory--agent-rows (result)
  "Return inventory rows for an `agents.list' RESULT."
  (mapcar (lambda (process)
            (let ((id (hermes-transport--display-field process 'session_id)))
              (list id
                    (vector (hermes-browser--face-cell
                             id 'hermes-browser-identifier)
                            (hermes-browser--status-cell
                             (hermes-transport--display-field process 'status)
                             'hermes-browser-status)
                            (hermes-browser--face-cell
                             (or (hermes-transport--get process 'uptime) 0)
                             'hermes-browser-uptime)
                            (hermes-browser--face-cell
                             (hermes-transport--display-field process 'command)
                             'hermes-browser-command)))))
          (hermes-transport--get result 'processes)))

(defun hermes-inventory--plugin-rows (result)
  "Return inventory rows for a `plugins.list' RESULT."
  (mapcar (lambda (plugin)
            (list (hermes-transport--display-field plugin 'name)
                  (vector (hermes-browser--face-cell
                           (hermes-transport--display-field plugin 'name)
                           'hermes-browser-name)
                          (hermes-browser--face-cell
                           (hermes-transport--display-field plugin 'version)
                           'hermes-browser-version)
                          (hermes-inventory--bool-cell
                           (hermes-transport--get plugin 'enabled)))))
          (hermes-transport--get result 'plugins)))

(defconst hermes-inventory--specs
  `(("Toolsets" "tools.list" nil
     [("Toolset" 24 t) ("On" 4 t) ("Tools" 6 t) ("Description" 50 nil)]
     ,#'hermes-inventory--toolset-rows toolsets)
    ("Skills" "skills.manage" ((action . "list"))
     [("Category" 20 t) ("Skill" 32 t) ("On" 4 t) ("Description" 50 nil)]
     ,#'hermes-inventory--skill-rows skills)
    ("Agents" "agents.list" nil
     [("Session" 18 t) ("Status" 10 t) ("Uptime" 8 t) ("Command" 50 nil)]
     ,#'hermes-inventory--agent-rows agents)
    ("Plugins" "plugins.list" nil
     [("Plugin" 30 t) ("Version" 12 t) ("On" 4 t)]
     ,#'hermes-inventory--plugin-rows plugins))
  "Inventory categories as (LABEL METHOD PARAMS FORMAT ROW-FN KIND).")

(defvar-local hermes-inventory--spec nil
  "The inventory spec backing the current buffer, for refresh and actions.")

(defun hermes-inventory--spec-method (spec)
  "Return SPEC's dashboard JSON-RPC method."
  (nth 1 spec))

(defun hermes-inventory--spec-params (spec)
  "Return SPEC's dashboard JSON-RPC params."
  (nth 2 spec))

(defun hermes-inventory--spec-format (spec)
  "Return SPEC's `tabulated-list-format' vector."
  (nth 3 spec))

(defun hermes-inventory--spec-rows (spec)
  "Return SPEC's pure rows function."
  (nth 4 spec))

(defun hermes-inventory--spec-kind (spec)
  "Return SPEC's inventory kind."
  (nth 5 spec))

(defun hermes-inventory--revert (&rest _)
  "Re-fetch the inventory shown in the current buffer."
  (when hermes-inventory--spec
    (let ((target (current-buffer))
          (generation (hermes-browser--next-request-generation)))
      (hermes-inventory--fetch hermes-inventory--spec nil target generation))))

(defvar-keymap hermes-inventory-mode-map
  :doc "Keymap for `hermes-inventory-mode'."
  :parent tabulated-list-mode-map
  "e" #'hermes-inventory-enable
  "d" #'hermes-inventory-disable
  "t" #'hermes-inventory-toggle
  "R" #'hermes-inventory-reload-skills)

(define-derived-mode hermes-inventory-mode tabulated-list-mode "Hermes Inventory"
  "Major mode for Hermes inventory listings.
\<hermes-inventory-mode-map>
Toolsets and skills support `\[hermes-inventory-enable]' and
`\[hermes-inventory-disable]'.  Skill reload is available with
`\[hermes-inventory-reload-skills]'."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-inventory--revert))

(defun hermes-inventory--render (spec rows &optional buffer)
  "Display ROWS for inventory SPEC in BUFFER when given."
  (with-current-buffer (or buffer
                           (get-buffer-create (format "*Hermes %s*" (car spec))))
    (unless (derived-mode-p 'hermes-inventory-mode)
      (hermes-inventory-mode))
    (setq hermes-inventory--spec spec)
    (setq tabulated-list-format (hermes-inventory--spec-format spec))
    (tabulated-list-init-header)
    (setq tabulated-list-entries rows)
    (tabulated-list-print t)))

(defun hermes-inventory--render-result (spec result &optional buffer)
  "Render inventory SPEC from dashboard RESULT in BUFFER when given."
  (hermes-inventory--render spec (funcall (hermes-inventory--spec-rows spec)
                                          result)
                            buffer))

(defun hermes-inventory--skills-promise (client spec)
  "Return a promise of the skill inventory for CLIENT.
Prefer dashboard REST, falling back to SPEC's read-only JSON-RPC method when
REST is unavailable."
  (hermes--promise-catch
   (hermes--promise-map
    (hermes-dashboard-transport-api-request-async
     "GET" "/api/skills" :client client)
    #'hermes-inventory--skills-result)
   (lambda (reason)
     (message "Hermes: skill status unavailable over REST (%s); using read-only list"
              reason)
     (hermes-dashboard-transport-call client (hermes-inventory--spec-method spec) (hermes-inventory--spec-params spec)))))

(defun hermes-inventory--fetch (spec &optional display target generation)
  "Fetch and render the inventory described by SPEC asynchronously.
DISPLAY pops the buffer when non-nil; revert refreshes in place without it.
TARGET and GENERATION identify an existing buffer-owned refresh.
Reuses a live chat connection when one exists; otherwise connects a transient
client for the listing."
  (let ((target (or target
                    (and display
                         (get-buffer-create (format "*Hermes %s*" (car spec))))
                    (current-buffer))))
    (with-current-buffer target
      (unless (derived-mode-p 'hermes-inventory-mode)
        (hermes-inventory-mode)))
    (let ((generation (or generation
                          (with-current-buffer target
                            (hermes-browser--next-request-generation)))))
      (hermes-browser--run-on-client
       (lambda (client)
         (if (eq (hermes-inventory--spec-kind spec) 'skills)
             (hermes-inventory--skills-promise client spec)
           (hermes-dashboard-transport-call
            client (hermes-inventory--spec-method spec)
            (hermes-inventory--spec-params spec))))
       (lambda (result)
         (when (hermes-browser--request-current-mode-p
                target generation 'hermes-inventory-mode)
           (hermes-inventory--render-result spec result target)
           (when display (pop-to-buffer target))))))))

(defun hermes-inventory--refresh-origin (buffer)
  "Start a fresh read of live inventory BUFFER."
  (when (hermes-browser--buffer-mode-p buffer 'hermes-inventory-mode)
    (with-current-buffer buffer
      (hermes-inventory--revert))))

(defun hermes-inventory--row-name ()
  "Return the current inventory row name, or signal `user-error'."
  (or (tabulated-list-get-id)
      (user-error "No Hermes inventory row on this line")))

(defun hermes-inventory--row-enabled-p ()
  "Return whether the current row appears enabled, or nil when unknown/off."
  (let* ((entry (tabulated-list-get-entry))
         (kind (and hermes-inventory--spec
                    (hermes-inventory--spec-kind hermes-inventory--spec)))
         (index (pcase kind
                  ('toolsets 1)
                  ('skills 2)
                  (_ nil))))
    (and entry index (equal (aref entry index) "on"))))

(defun hermes-inventory--action-kind ()
  "Return the current inventory kind for actions, or signal `user-error'."
  (or (and hermes-inventory--spec
           (hermes-inventory--spec-kind hermes-inventory--spec))
      (user-error "This buffer is not backed by a Hermes inventory action")))

(defun hermes-inventory--toolset-done-message (name enabled result)
  "Return completion message for toolset NAME set to ENABLED with RESULT."
  (let ((reset (hermes-transport--get result 'reset)))
    (concat (format "%s toolset %s" (if enabled "Enabled" "Disabled") name)
            (if reset
                "; current dashboard session was reset"
              "; new sessions use this setting after reset/restart"))))

(defun hermes-inventory--set-toolset-enabled (name enabled)
  "Set toolset NAME to ENABLED through dashboard RPC.
Toolset changes are global configuration: they are not scoped to a single
chat session, so no `:session-id' is sent.  New sessions pick up the toggle
after a reset/restart."
  (let ((origin (current-buffer)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-dashboard-transport-call-fn
        #'hermes-dashboard-transport-tools-configure
        client (list name) (if enabled "enable" "disable")))
     (lambda (result)
       (message "Hermes: %s"
                (hermes-inventory--toolset-done-message name enabled result))
       (hermes-inventory--refresh-origin origin)))))

(defun hermes-inventory--set-skill-enabled (name enabled)
  "Set skill NAME to ENABLED through the dashboard REST API."
  (let ((origin (current-buffer)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-dashboard-transport-api-request-async
        "PUT" "/api/skills/toggle"
        :body `((name . ,name)
                (enabled . ,(hermes-inventory--json-bool enabled)))
        :client client))
     (lambda (_result)
       (message "Hermes: %s skill %s; new sessions use this setting, or press R to reload skills"
                (if enabled "enabled" "disabled") name)
       (hermes-inventory--refresh-origin origin)))))

(defun hermes-inventory--set-enabled (enabled)
  "Set the toolset or skill at point to ENABLED."
  (let ((name (hermes-inventory--row-name)))
    (pcase (hermes-inventory--action-kind)
      ('toolsets (hermes-inventory--set-toolset-enabled name enabled))
      ('skills (hermes-inventory--set-skill-enabled name enabled))
      (_ (user-error "Enable/disable is available only for toolsets and skills")))))

(defun hermes-inventory-enable ()
  "Enable the toolset or skill at point."
  (interactive)
  (hermes-inventory--set-enabled t))

(defun hermes-inventory-disable ()
  "Disable the toolset or skill at point."
  (interactive)
  (hermes-inventory--set-enabled nil))

(defun hermes-inventory-toggle ()
  "Toggle the toolset or skill at point."
  (interactive)
  (hermes-inventory--set-enabled (not (hermes-inventory--row-enabled-p))))

(defun hermes-inventory-reload-skills ()
  "Reload dashboard skills, reporting added/removed skills when supported."
  (interactive)
  (let ((origin (current-buffer)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-dashboard-transport-call-fn
        #'hermes-dashboard-transport-skills-reload client))
     (lambda (result)
       (message "Hermes: %s"
                (or (hermes-transport--scalar-string
                     (hermes-transport--get result 'output))
                    "skills reloaded"))
       (when (and (hermes-browser--buffer-mode-p
                   origin 'hermes-inventory-mode)
                  (with-current-buffer origin
                    (and hermes-inventory--spec
                         (eq (hermes-inventory--spec-kind
                              hermes-inventory--spec)
                             'skills))))
         (hermes-inventory--refresh-origin origin))))))

;;; Memory status

(defvar-local hermes-memory--status nil
  "Latest memory status owned by this buffer.")

(defvar-local hermes-memory--operation nil
  "Identity token for the current memory configuration operation.")

(defconst hermes-memory--no-fields :hermes-memory-no-fields
  "Result marker for a provider with no configurable fields.")

(defconst hermes-memory--unchanged :hermes-memory-unchanged
  "Result marker for a deliberately unchanged provider field.")

(defun hermes-inventory--safe-memory-name (value)
  "Return a safe provider name for VALUE, redacting secret-shaped strings."
  (let ((text (or (hermes-transport--scalar-string value) "")))
    (cond
     ((string-empty-p text) "built-in")
     ((or (string-match-p "token\\|secret\\|password\\|api[_-]?key"
                          (downcase text))
          (string-match-p "[A-Za-z0-9_-]\\{48,\\}" text))
      "<redacted>")
     (t text))))

(defun hermes-inventory--memory-size (status key)
  "Return built-in memory STATUS size for KEY as a number."
  (let ((value (hermes-transport--get
                (hermes-transport--get status 'builtin_files) key)))
    (if (numberp value) value 0)))

(defun hermes-inventory--memory-status-text (status)
  "Return redacted display text for memory STATUS.
The text intentionally omits memory contents, provider lists, paths, and
unknown backend fields so secrets cannot leak through this buffer."
  (string-join
   (list "Hermes Memory"
         ""
         (format "Active provider: %s"
                 (hermes-inventory--safe-memory-name
                  (hermes-transport--get status 'active)))
         ""
         "Built-in store sizes:"
         (format "  MEMORY.md: %d bytes"
                 (hermes-inventory--memory-size status 'memory))
         (format "  USER.md: %d bytes"
                 (hermes-inventory--memory-size status 'user))
         ""
         "Keys: p provider, c configure, g refresh, D reset built-in memory.")
   "\n"))

(defun hermes-inventory--render-memory-status (status target &optional display)
  "Render memory STATUS in the memory buffer.
TARGET is the existing memory buffer.  DISPLAY pops it when non-nil."
  (with-current-buffer target
    (unless (derived-mode-p 'hermes-memory-status-mode)
      (hermes-memory-status-mode))
    (setq hermes-memory--status status)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (hermes-inventory--memory-status-text status))
      (goto-char (point-min)))
    (when display (pop-to-buffer (current-buffer)))))

(defvar-keymap hermes-memory-status-mode-map
  :doc "Keymap for `hermes-memory-status-mode'."
  :parent special-mode-map
  "p" #'hermes-memory-select-provider
  "c" #'hermes-memory-configure-provider
  "g" #'hermes-memory-status
  "D" #'hermes-memory-reset)

(define-derived-mode hermes-memory-status-mode special-mode "Hermes Memory"
  "Major mode for redacted Hermes memory provider status."
  :interactive nil)

(defun hermes-memory--provider-name-p (name)
  "Return non-nil when NAME is safe for a memory provider route."
  (and (stringp name)
       (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9_-]\\{0,63\\}\\'" name)
       (equal name (hermes-inventory--safe-memory-name name))))

(defun hermes-memory--provider-names ()
  "Return safe provider names from the current status, including built-in."
  (delete-dups
   (cons "built-in"
         (delq nil
               (mapcar
                (lambda (provider)
                  (let ((name (hermes-transport--scalar-string
                               (hermes-transport--get provider 'name))))
                    (and (hermes-memory--provider-name-p name) name)))
                (hermes-transport--get hermes-memory--status 'providers))))))

(defun hermes-memory--read-provider (action)
  "Read a provider from current status for ACTION."
  (completing-read (format "Memory provider to %s: " action)
                   (hermes-memory--provider-names) nil t))

(defun hermes-memory--require-provider (provider)
  "Return validated PROVIDER from current status, or signal `user-error'."
  (unless (member provider (hermes-memory--provider-names))
    (user-error "Unknown memory provider: %s" provider))
  provider)

(defun hermes-memory--require-idle ()
  "Signal `user-error' while a memory configuration operation is active."
  (when hermes-memory--operation
    (user-error "A memory update is still in progress")))

(defun hermes-memory--operation-current-p (buffer generation)
  "Return non-nil when BUFFER still owns memory operation GENERATION."
  (hermes-browser--request-current-mode-p
   buffer generation 'hermes-memory-status-mode))

(defun hermes-memory--clear-operation (buffer token)
  "Clear BUFFER's memory operation when it still belongs to TOKEN."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq hermes-memory--operation token)
        (setq hermes-memory--operation nil)))))

(defun hermes-memory--run-owned
    (buffer generation token make-promise on-success &optional secrets)
  "Run MAKE-PROMISE while BUFFER owns GENERATION and TOKEN.
MAKE-PROMISE receives a dashboard client.  ON-SUCCESS and errors apply only
while the originating memory buffer remains current.  SECRETS are redacted."
  (condition-case err
      (hermes-browser--with-client
       (lambda (client done)
         (let ((finish
                (lambda ()
                  (unwind-protect (funcall done)
                    (hermes-memory--clear-operation buffer token)))))
           (hermes--promise-catch
            (hermes--promise-then
             (condition-case request-error
                 (hermes--promise-finally
                  (funcall make-promise client) finish)
               ((error quit)
                (funcall finish)
                (hermes--promise-rejected
                 (error-message-string request-error))))
             (lambda (result)
               (when (hermes-memory--operation-current-p buffer generation)
                 (funcall on-success result))))
            (lambda (reason)
              (when (hermes-memory--operation-current-p buffer generation)
                (message "Hermes: %s"
                         (hermes-dashboard-transport--redact-secret
                          reason secrets))))))))
    ((error quit)
     (hermes-memory--clear-operation buffer token)
     (message "Hermes: %s"
              (hermes-dashboard-transport--redact-secret
               (error-message-string err) secrets)))))

(defun hermes-memory--api (client method path &optional body secrets)
  "Send memory METHOD PATH through CLIENT with optional BODY and SECRETS."
  (hermes-dashboard-transport-api-request-async
   method (concat "/api/memory" path)
   :body body :secrets secrets :client client))

(defun hermes-memory--provider-path (provider)
  "Return PROVIDER's configuration path."
  (format "/providers/%s/config" (url-hexify-string provider)))

(defun hermes-memory--status-promise (client)
  "Return a memory status promise through CLIENT."
  (hermes-memory--api client "GET" ""))

(defun hermes-memory--comparable-value (value)
  "Return VALUE's stable string form for schema dependency comparison."
  (cond ((eq value t) "true")
        ((eq value :false) "false")
        ((null value) "")
        (t (format "%s" value))))

(defun hermes-memory--field-visible-p (field fields)
  "Return non-nil when FIELD's `when' dependencies match FIELDS."
  (let ((when (hermes-transport--get field 'when)))
    (or (null when)
        (seq-every-p
         (lambda (condition)
           (let* ((key (format "%s" (car condition)))
                  (dependency
                   (seq-find
                    (lambda (candidate)
                      (equal key
                             (hermes-transport--scalar-string
                              (hermes-transport--get candidate 'key))))
                    fields)))
             (equal (hermes-memory--comparable-value
                     (hermes-transport--get dependency 'value))
                    (hermes-memory--comparable-value (cdr condition)))))
         when))))

(defun hermes-memory--visible-fields (schema)
  "Return currently visible configurable fields from SCHEMA."
  (let ((fields (hermes-transport--get schema 'fields)))
    (seq-filter (lambda (field)
                  (and (hermes-transport--non-empty-string
                        (hermes-transport--scalar-string
                         (hermes-transport--get field 'key)))
                       (hermes-memory--field-visible-p field fields)))
                fields)))

(defun hermes-memory--read-field (fields)
  "Read and return one field from FIELDS."
  (let* ((choices
          (mapcar
           (lambda (field)
             (format "%s (%s)"
                     (or (hermes-transport--non-empty-string
                          (hermes-transport--scalar-string
                           (hermes-transport--get field 'label)))
                         (hermes-transport--get field 'key))
                     (hermes-transport--get field 'key)))
           fields))
         (choice (completing-read "Memory field: " choices nil t))
         (index (seq-position choices choice #'equal)))
    (nth index fields)))

(defun hermes-memory--field-options (field)
  "Return (DISPLAY . VALUE) choices from select FIELD."
  (mapcar
   (lambda (option)
     (let* ((value (or (hermes-transport--scalar-string
                        (hermes-transport--get option 'value))
                       (hermes-transport--scalar-string option)
                       ""))
            (label (or (hermes-transport--non-empty-string
                        (hermes-transport--scalar-string
                         (hermes-transport--get option 'label)))
                       value)))
       (cons (if (equal label value) value (format "%s (%s)" label value))
             value)))
   (hermes-transport--get field 'options)))

(defun hermes-memory--read-field-value (field)
  "Read FIELD's value using its backend-declared kind."
  (let* ((kind (hermes-transport--scalar-string
                (hermes-transport--get field 'kind)))
         (key (hermes-transport--get field 'key))
         (label (or (hermes-transport--non-empty-string
                     (hermes-transport--scalar-string
                      (hermes-transport--get field 'label)))
                    key))
         (current (hermes-transport--get field 'value)))
    (pcase kind
      ("secret"
       (let ((value (read-passwd (format "%s: " label))))
         (if (string-empty-p value) hermes-memory--unchanged value)))
      ("select"
       (let* ((options (hermes-memory--field-options field))
              (initial (rassoc (hermes-memory--comparable-value current)
                               options))
              (choice (completing-read
                       (format "%s: " label) (mapcar #'car options) nil t nil nil
                       (car initial))))
         (cdr (assoc choice options))))
      ("boolean"
       (if (y-or-n-p (format "Enable %s? " label)) t :false))
      (_
       (read-string (format "%s: " label)
                    (hermes-memory--comparable-value current))))))

(defun hermes-memory--configuration-promise
    (client buffer generation provider redactions)
  "Return PROVIDER configuration flow promise through CLIENT.
BUFFER and GENERATION own prompts and writes.  REDACTIONS is updated when a
secret is read."
  (hermes--promise-then
   (hermes-memory--api client "GET" (hermes-memory--provider-path provider))
   (lambda (schema)
     (when (hermes-memory--operation-current-p buffer generation)
       (let ((fields (hermes-memory--visible-fields schema)))
         (if (null fields)
             hermes-memory--no-fields
           (let* ((field (hermes-memory--read-field fields))
                  (key (hermes-transport--scalar-string
                        (hermes-transport--get field 'key)))
                  (secret-p (equal (hermes-transport--get field 'kind) "secret"))
                  (value (hermes-memory--read-field-value field)))
             (if (eq value hermes-memory--unchanged)
                 hermes-memory--unchanged
               (when secret-p (setcar redactions value))
               (when (hermes-memory--operation-current-p buffer generation)
                 (hermes--promise-then
                  (hermes-memory--api
                   client "PUT" (hermes-memory--provider-path provider)
                   `((values . ((,key . ,value))))
                   (and secret-p (list value)))
                  (lambda (_result)
                    (when (hermes-memory--operation-current-p buffer generation)
                      (hermes-memory--status-promise client)))))))))))))

;;;###autoload
(defun hermes-memory-select-provider (&optional provider)
  "Select PROVIDER from the current memory status and refresh it."
  (interactive)
  (hermes-memory--require-idle)
  (setq provider (hermes-memory--require-provider
                  (or provider (hermes-memory--read-provider "activate"))))
  (let ((buffer (current-buffer))
        (generation (hermes-browser--next-request-generation))
        (token (list 'memory-provider-selection)))
    (setq hermes-memory--operation token)
    (hermes-memory--run-owned
     buffer generation token
     (lambda (client)
       (hermes--promise-then
        (hermes-memory--api client "PUT" "/provider"
                            `((provider . ,provider)))
        (lambda (_result)
          (when (hermes-memory--operation-current-p buffer generation)
            (hermes-memory--status-promise client)))))
     (lambda (status)
       (hermes-inventory--render-memory-status status buffer)
       (message "Hermes: active memory provider is %s" provider)))))

;;;###autoload
(defun hermes-memory-configure-provider (&optional provider)
  "Configure one schema field for PROVIDER and refresh memory status."
  (interactive)
  (hermes-memory--require-idle)
  (setq provider (hermes-memory--require-provider
                  (or provider (hermes-memory--read-provider "configure"))))
  (let ((buffer (current-buffer))
        (generation (hermes-browser--next-request-generation))
        (token (list 'memory-provider-configuration))
        (redactions (list nil)))
    (setq hermes-memory--operation token)
    (hermes-memory--run-owned
     buffer generation token
     (lambda (client)
       (hermes-memory--configuration-promise
        client buffer generation provider redactions))
     (lambda (result)
       (pcase result
         (:hermes-memory-no-fields
          (message "Hermes: %s exposes no configurable fields" provider))
         (:hermes-memory-unchanged
          (message "Hermes: %s configuration unchanged" provider))
         (_
          (hermes-inventory--render-memory-status result buffer)
          (message "Hermes: configured memory provider %s" provider))))
     redactions)))

;;;###autoload
(defun hermes-memory-status ()
  "Show Hermes memory provider and built-in store sizes.
The buffer never displays memory contents or secret material."
  (interactive)
  (let* ((display (not (derived-mode-p 'hermes-memory-status-mode)))
         (target (if display
                     (get-buffer-create "*Hermes Memory*")
                   (current-buffer)))
         (generation
          (with-current-buffer target
            (unless (derived-mode-p 'hermes-memory-status-mode)
              (hermes-memory-status-mode))
            (hermes-browser--next-request-generation))))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-dashboard-transport-api-request-async
        "GET" "/api/memory" :client client))
     (lambda (status)
       (when (hermes-browser--request-current-mode-p
              target generation 'hermes-memory-status-mode)
         (hermes-inventory--render-memory-status status target display))))))

;;;###autoload
(defun hermes-memory-reset (target)
  "Reset built-in Hermes memory TARGET after confirmation.
TARGET is one of all, memory, or user.  External providers are not reset."
  (interactive
   (list (completing-read "Reset built-in memory store: "
                          '("all" "memory" "user") nil t nil nil "all")))
  (unless (member target '("all" "memory" "user"))
    (user-error "Memory reset target must be all, memory, or user"))
  (let ((origin (current-buffer)))
    (when (yes-or-no-p
           (format "Erase built-in Hermes %s memory?  This deletes only MEMORY.md/USER.md data.  Continue?"
                   target))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes-dashboard-transport-api-request-async
          "POST" "/api/memory/reset"
          :body `((target . ,target))
          :client client))
       (lambda (result)
         (message "Hermes: reset %s memory (%s)"
                  target
                  (string-join
                   (or (hermes-transport--get result 'deleted) '())
                   ", "))
         (when (hermes-browser--buffer-mode-p
                origin 'hermes-memory-status-mode)
           (with-current-buffer origin
             (hermes-memory-status))))))))

;;;###autoload
(defun hermes-list-inventory ()
  "Browse Hermes inventory: toolsets, skills, agents, plugins, or memory."
  (interactive)
  (let* ((labels (append (mapcar #'car hermes-inventory--specs) '("Memory")))
         (choice (completing-read "Hermes inventory: " labels nil t)))
    (if (equal choice "Memory")
        (hermes-memory-status)
      (hermes-inventory--fetch (assoc choice hermes-inventory--specs) t))))

(provide 'hermes-inventory)
;;; hermes-inventory.el ends here
