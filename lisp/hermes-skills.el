;;; hermes-skills.el --- Owned skill content and passive Hub -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Installed SKILL.md text is inert, explicitly editable data.  Saves replace
;; the whole document (last writer wins); only an owned GET confirms a save.
;; Hub previews and scans are separate from installed content and installation.

;;; Code:

(require 'hermes-browser)
(require 'hermes-dashboard-api)
(require 'hermes-promise)
(require 'keymap-popup)
(require 'text-mode)

(defvar-local hermes-skills--profile nil "Exact profile, nil for launch scope.")
(defvar-local hermes-skills--name nil "Exact installed skill name.")
(defvar-local hermes-skills--candidate nil "Hub identity (SOURCE IDENTIFIER).")
(defvar-local hermes-skills--original nil "Last confirmed installed content.")
(defvar-local hermes-skills--path nil "Backend path label, never a local file.")
(defvar-local hermes-skills--busy nil "Current save token, or nil.")
(defvar-local hermes-skills--parent nil "Exact originating view buffer.")
(defvar-local hermes-skills--sources nil "Configured Hub source metadata.")
(defvar-local hermes-skills--query nil "Last explicit Hub search, nil for official.")
(defvar-local hermes-skills--origin-current-p nil "Retained Hub selection guard.")

(defun hermes-skills--require-owner ()
  "Reject an unowned or retired skill view."
  (unless (hermes-buffer--owned-p)
    (user-error "Skill view retired; reopen it")))

(defun hermes-skills--guard ()
  "Capture this skill view's claim, endpoint and domain ownership."
  (hermes-skills--require-owner)
  (when (and hermes-skills--origin-current-p
             (not (funcall hermes-skills--origin-current-p)))
    (user-error "Hub selection changed; reopen the preview"))
  (let ((view (hermes-browser--mutation-context))
        (origin hermes-skills--origin-current-p)
        (domain (hermes-browser--owned-predicate
                 '(hermes-skills--profile hermes-skills--name
                   hermes-skills--candidate))))
    (lambda () (and (funcall view) (funcall domain)
                    (or (not origin) (funcall origin))))))

(defun hermes-skills--api (client active profile method path &optional query body timeout)
  "Request METHOD PATH on CLIENT under ACTIVE for PROFILE, QUERY and BODY.
Optional TIMEOUT is a request-local HTTP budget retained across authentication."
  (hermes-dashboard-transport-api-request-async
   method path :client client :current-p active :body body :timeout timeout
   :query (append query (when profile `((profile . ,profile))))))

(defun hermes-skills--run-owned (request active success failure)
  "Run REQUEST under ACTIVE, delivering RESULT and exact operation to SUCCESS.
FAILURE retains the shared browser settlement behavior."
  (let (operation)
    (hermes-browser--run-owned
     (lambda (client guard)
       (setq operation guard)
       (funcall request client guard))
     active (lambda (result) (funcall success result operation)) failure)))

(defun hermes-skills--publish (text active)
  "Replace this view with TEXT only while the exact operation ACTIVE owns it.
Notify native change hooks around a callback-free edit.  Return non-nil only
if publication remains current.  Roll back errors and quits for the same
owner, never a successor established by a hook."
  (when (funcall active)
    (let* ((buffer (current-buffer))
           (tick (buffer-chars-modified-tick))
           (old (save-restriction (widen) (buffer-string)))
           (point (point)) (undo buffer-undo-list)
           (modified (buffer-modified-p)) (readonly buffer-read-only)
           (retired (make-symbol "retired-publication")))
      (condition-case err
          (catch retired
            (save-restriction
              (widen)
              (let ((inhibit-read-only t))
                (combine-change-calls (point-min) (point-max)
                  (unless (and (eq buffer (current-buffer)) (funcall active)
                               (= tick (buffer-chars-modified-tick)))
                    (throw retired nil))
                  (let ((inhibit-modification-hooks t) (inhibit-quit t))
                    (erase-buffer)
                    (insert text)
                    (setq tick (buffer-chars-modified-tick)))))
              (when (and (funcall active) (= tick (buffer-chars-modified-tick)))
                (goto-char (min point (point-max)))
                t)))
        ((error quit)
         (when (funcall active)
           (with-current-buffer buffer
             (save-restriction
               (widen)
               (let ((inhibit-read-only t) (inhibit-modification-hooks t)
                     (inhibit-quit t))
                 (erase-buffer) (insert old)
                 (setq buffer-undo-list undo buffer-read-only readonly)
                 (set-buffer-modified-p modified)
                 (goto-char point)))))
         (signal (car err) (cdr err)))))))

(defun hermes-skills--failure (reason)
  "Expose request failure REASON without replacing content or replaying writes."
  (setq hermes-browser--status
        (propertize "Failed/uncertain; draft retained; explicit retry only"
                    'face 'error
                    'help-echo (hermes-dashboard-transport--redact-secret reason)))
  (force-mode-line-update))

(defun hermes-skills--new-buffer (name mode instance profile parent)
  "Create NAME in MODE owned by INSTANCE, PROFILE and PARENT."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (funcall mode)
      (hermes-buffer--claim mode)
      (hermes-browser--own-instance (hermes-browser--copy-identity instance))
      (setq hermes-skills--profile (and profile (copy-sequence profile))
            hermes-skills--parent parent)
      (setq-local header-line-format '(:eval (hermes-skills--header))))
    buffer))

(defun hermes-skills--header ()
  "Return the owning skill view's compact provenance header."
  (list " " (propertize (hermes-instance-name hermes-instance) 'face 'shadow)
        " | Profile: " (propertize (or hermes-skills--profile "backend launch")
                                   'face 'font-lock-constant-face)
        " | " (propertize (or hermes-skills--name
                              (cadr hermes-skills--candidate) "Skill Hub")
                          'face 'hermes-browser-name)
        (when hermes-skills--path
          (propertize " | Backend path (label)" 'face 'shadow
                      'help-echo hermes-skills--path))))

(defun hermes-skills-back ()
  "Return to this skill view's retained parent when still live."
  (interactive)
  (hermes-skills--require-owner)
  (if (buffer-live-p hermes-skills--parent)
      (pop-to-buffer hermes-skills--parent)
    (quit-window)))

(defvar-keymap hermes-skill-content-mode-map
  :doc "Keymap for inert installed skill content."
  :parent text-mode-map
  "C-c C-e" #'hermes-skill-content-edit
  "C-c C-c" #'hermes-skill-content-save
  "C-c C-k" #'hermes-skill-content-discard
  "C-c C-g" #'hermes-skill-content-refresh
  "C-c C-b" #'hermes-skills-back)

(keymap-popup-annotate hermes-skill-content-mode-map
  :popup-key "C-c C-h" :exit-key "C-g" :description "Installed skill (full replacement)"
  :group "Content"
  hermes-skill-content-edit "Edit"
  hermes-skill-content-save "Save (last writer wins)"
  hermes-skill-content-discard "Discard local edits"
  :group "View"
  hermes-skill-content-refresh "Read again"
  hermes-skills-back "Back")

(define-derived-mode hermes-skill-content-mode text-mode "Hermes Skill"
  "View inert SKILL.md text; explicitly Edit, Save or Discard.
No file locals or Markdown code are executed.  Saves replace the entire
backend document and can overwrite concurrent edits.  No live-session
activation is promised.  Use `\\[hermes-skill-content-mode-map-popup]' for help."
  :interactive nil
  (setq buffer-read-only t)
  (setq-local buffer-auto-save-file-name nil)
  (hermes-browser--setup-status))

(defun hermes-skill-content--accept (result active)
  "Accept exact installed content RESULT only under operation ACTIVE."
  (let ((content (hermes-transport--get result 'content)))
    (unless (and (equal hermes-skills--name (hermes-transport--get result 'name))
                 (stringp content))
      (error "Missing or mismatched skill content"))
    (when (hermes-skills--publish content active)
      (setq hermes-skills--original content
            hermes-skills--path (hermes-transport--get result 'path)
            buffer-read-only t
            hermes-browser--status "Read-only; C-c C-e edit; C-c C-h help")
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

(defun hermes-skill-content-refresh ()
  "Read this exact installed skill without discarding a local draft."
  (interactive nil hermes-skill-content-mode)
  (hermes-skills--require-owner)
  (when (or hermes-skills--busy (buffer-modified-p))
    (user-error "Keep or explicitly discard the draft before reading again"))
  (let ((active (hermes-skills--guard))
        (name (copy-sequence hermes-skills--name))
        (profile hermes-skills--profile)
        (tick (buffer-chars-modified-tick)))
    (setq hermes-browser--status "Loading")
    (hermes-skills--run-owned
     (lambda (client guard)
       (hermes-skills--api client guard profile "GET" "/api/skills/content"
                           `((name . ,name))))
     active
     (lambda (result operation)
       (if (= tick (buffer-chars-modified-tick))
           (hermes-skill-content--accept result operation)
         (setq hermes-browser--status "Read superseded by local edits; draft retained")))
     #'hermes-skills--failure)))

;;;###autoload
(defun hermes-skill-content (name &optional profile)
  "Open installed skill NAME in PROFILE as inert native text.
Nil PROFILE means the owning backend's launch profile, not local files."
  (interactive (list (read-string "Installed skill name: ")))
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "An exact installed skill name is required"))
  (let ((buffer (hermes-skills--new-buffer
                 (format "*Hermes Skill %s*" name) #'hermes-skill-content-mode
                 (hermes-instance-resolve) profile (current-buffer))))
    (with-current-buffer buffer (setq hermes-skills--name (copy-sequence name)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer (hermes-skill-content-refresh))
    buffer))

(defun hermes-skill-content-edit ()
  "Make the confirmed installed content editable without executing it."
  (interactive nil hermes-skill-content-mode)
  (hermes-skills--require-owner)
  (unless (and (stringp hermes-skills--original) (not hermes-skills--busy))
    (user-error "Wait for an authoritative content read"))
  (setq buffer-read-only nil
        hermes-browser--status "Editing; Save replaces all content (last writer wins)")
  (message "Save can overwrite concurrent edits; no conflict detection"))

(defun hermes-skill-content-discard ()
  "Discard local edits back to the last confirmed content after consent."
  (interactive nil hermes-skill-content-mode)
  (hermes-skills--require-owner)
  (when hermes-skills--busy (user-error "Save pending; retain the draft"))
  (unless (stringp hermes-skills--original) (user-error "No confirmed content"))
  (let ((buffer (current-buffer))
        (active (hermes-skills--guard))
        (tick (buffer-chars-modified-tick)))
    (when (and (yes-or-no-p "Discard local edits to last confirmed content? ")
               (funcall active))
      (with-current-buffer buffer
        (unless (= tick (buffer-chars-modified-tick))
          (user-error "Draft changed during confirmation"))
        (hermes-skill-content--accept
         `((name . ,hermes-skills--name) (content . ,hermes-skills--original)
           (path . ,hermes-skills--path)) active)))))

(defun hermes-skill-content--save-request (client active name profile content)
  "Save CONTENT for NAME and PROFILE on CLIENT under ACTIVE, then read back."
  (hermes--promise-then
   (hermes-skills--api
    client active profile "PUT" "/api/skills/content" nil
    (append `((name . ,name) (content . ,content))
            (when profile `((profile . ,profile)))))
   (lambda (_receipt)
     (when (funcall active)
       (hermes-skills--api client active profile "GET" "/api/skills/content"
                           `((name . ,name)))))))

(defun hermes-skill-content-save ()
  "Explicitly replace this exact installed skill, preserving uncertain drafts."
  (interactive nil hermes-skill-content-mode)
  (hermes-skills--require-owner)
  (when hermes-skills--busy (user-error "Save already pending"))
  (unless (and (stringp hermes-skills--original) (not buffer-read-only))
    (user-error "Read and Edit content before saving"))
  (let ((buffer (current-buffer))
        (active (hermes-skills--guard))
        (tick (buffer-chars-modified-tick))
        (name (copy-sequence hermes-skills--name))
        (profile hermes-skills--profile)
        (content (save-restriction
                   (widen) (buffer-substring-no-properties (point-min) (point-max)))))
    (when (and (yes-or-no-p "Concurrent edits may be overwritten.  Replace entire SKILL.md? ")
               (funcall active))
      (with-current-buffer buffer
        (unless (= tick (buffer-chars-modified-tick))
          (user-error "Draft changed during confirmation; save again explicitly"))
        (hermes-skill-content--save-owned active name profile content tick)))))

(defun hermes-skill-content--save-owned (active name profile content tick)
  "Save NAME, PROFILE and CONTENT under ACTIVE, retaining draft TICK."
  (let ((buffer (current-buffer)) (token (list 'skill-save)))
    (setq hermes-skills--busy token hermes-browser--status "Saving")
    (hermes-browser--run-owned
     (lambda (client guard)
       (hermes-skill-content--save-request client guard name profile content))
     active
     (lambda (result)
       (if (and (equal name (hermes-transport--get result 'name))
                (equal content (hermes-transport--get result 'content))
                (= tick (buffer-chars-modified-tick)))
           (progn (setq hermes-skills--original content)
                  (set-buffer-modified-p nil)
                  (setq hermes-browser--status "Saved and read back; no live activation claim"))
         (setq hermes-browser--status "Readback differs or draft changed; draft retained")))
     #'hermes-skills--failure
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (eq token hermes-skills--busy)
             (setq hermes-skills--busy nil))))))))

;;; Passive Hub

(defvar-keymap hermes-skills-hub-mode-map
  :doc "Keymap for source-qualified Skill Hub browsing."
  :parent tabulated-list-mode-map
  "n" #'next-line "p" #'previous-line
  "f" #'hermes-skills-hub-preview "RET" #'hermes-skills-hub-preview
  "b" #'hermes-skills-back "g" #'hermes-skills-hub-refresh
  "s" #'hermes-skills-hub-search "o" #'hermes-skills-hub-official
  "S" #'hermes-skills-hub-sources "P" #'hermes-skills-hub-profile)

(keymap-popup-annotate hermes-skills-hub-mode-map
  :popup-key "?" :exit-key "C-g" :description "Skill Hub (no installation)"
  :group "Browse"
  hermes-skills-hub-preview "Preview"
  hermes-skills-hub-search "Search (network)"
  hermes-skills-hub-official "Official"
  :group "Scope"
  hermes-skills-hub-sources "Configured sources"
  hermes-skills-hub-profile "Profile"
  :group "View"
  hermes-skills-hub-refresh "Refresh"
  hermes-skills-back "Back"
  quit-window "Quit")

(define-derived-mode hermes-skills-hub-mode tabulated-list-mode "Hermes Skill Hub"
  "Browse source-qualified candidates without installation or automatic scan."
  :interactive nil
  (setq tabulated-list-format
        [("Name" 26 t) ("Source" 18 t) ("Trust" 12 t) ("Repository" 32 t)])
  (tabulated-list-init-header)
  (hermes-browser--setup-status))

(defvar-keymap hermes-skill-preview-mode-map
  :doc "Keymap for inert Hub preview and explicit optional scan."
  :parent special-mode-map
  "n" #'next-line "p" #'previous-line
  "f" #'forward-page "b" #'hermes-skills-back
  "g" #'hermes-skills-hub-preview-refresh
  "S" #'hermes-skills-hub-scan)

(keymap-popup-annotate hermes-skill-preview-mode-map
  :popup-key "?" :exit-key "C-g" :description "Passive skill preview"
  :group "Inspect"
  hermes-skills-hub-scan "Scan (explicit network/quarantine)"
  :group "View"
  hermes-skills-hub-preview-refresh "Read preview again"
  hermes-skills-back "Back"
  quit-window "Quit")

(define-derived-mode hermes-skill-preview-mode special-mode "Hermes Skill Preview"
  "Display inert remote instructions or source metadata; never execute them."
  :interactive nil
  (hermes-browser--setup-status))

(defun hermes-skills-hub--rows (rows)
  "Return source-qualified native list entries for ROWS."
  (mapcar
   (lambda (row)
     (let ((source (hermes-transport--get row 'source))
           (identifier (hermes-transport--get row 'identifier)))
       (unless (and (stringp source) (not (string-empty-p source))
                    (stringp identifier) (not (string-empty-p identifier)))
         (error "Hub row lacks a source-qualified identity"))
       (list (list source identifier)
             (vector (hermes-browser--face-cell
                      (hermes-transport--get row 'name) 'hermes-browser-name)
                     source
                     (hermes-browser--face-cell
                      (hermes-transport--get row 'trust_level) 'font-lock-type-face)
                     (or (hermes-transport--get row 'repo) "")))))
   rows))

(defun hermes-skills-hub--catalog (client active profile query)
  "Read configured sources and catalog through CLIENT under ACTIVE.
Retain PROFILE and QUERY across both requests.  Search allows 35 seconds:
the released backend fanout uses 30, leaving 5 for HTTP response overhead."
  (hermes--promise-then
   (hermes-skills--api client active profile "GET" "/api/skills/hub/sources")
   (lambda (sources)
     (when (funcall active)
       (hermes--promise-map
        (hermes-skills--api client active profile "GET"
                            (if query "/api/skills/hub/search" "/api/skills/hub/official")
                            (when query `((q . ,query) (source . "all") (limit . 50)))
                            nil (when query 35))
        (lambda (result) (list sources result)))))))

(defun hermes-skills-hub--table-text (rows)
  "Render ROWS off-buffer using this view's native table configuration."
  (let ((format tabulated-list-format) (padding tabulated-list-padding)
        (sort-key tabulated-list-sort-key) (printer tabulated-list-printer))
    (with-temp-buffer
      (setq tabulated-list-format format tabulated-list-padding padding
            tabulated-list-sort-key sort-key tabulated-list-printer printer
            tabulated-list-entries rows)
      (tabulated-list-print t)
      (buffer-string))))

(defun hermes-skills-hub-refresh ()
  "Read configured sources and the current bounded Hub catalog."
  (interactive nil hermes-skills-hub-mode)
  (let ((active (hermes-skills--guard))
        (profile hermes-skills--profile)
        (query hermes-skills--query))
    (when (hermes-skills--publish "" active)
      (set-buffer-modified-p nil)
      (setq tabulated-list-entries nil hermes-skills--sources nil
            hermes-browser--status "Loading")
      (hermes-skills--run-owned
       (lambda (client guard) (hermes-skills-hub--catalog client guard profile query))
       active
       (lambda (pair operation)
         (let* ((result (cadr pair))
                (timeouts (hermes-transport--get result 'timed_out))
                (rows (hermes-skills-hub--rows
                       (hermes-transport--get result (if query 'results 'skills)))))
           (when (hermes-skills--publish (hermes-skills-hub--table-text rows) operation)
             (setq hermes-skills--sources (car pair) tabulated-list-entries rows)
             (set-buffer-modified-p nil)
             (goto-char (point-min))
             (setq hermes-browser--status
                   (cond (timeouts (propertize
                                    (format "Partial results; timed out: %s" (string-join timeouts ", "))
                                    'face 'warning))
                         (query "Bounded search (up to 50); not exhaustive")
                         (rows "Official catalog; f preview; S sources")
                         (t "No candidates; s search or S inspect sources"))))))
       (lambda (reason)
         (hermes-browser--read-error reason)
         (setq hermes-browser--status
               (propertize "Hub unavailable; g retry, s search, P profile (requires REST support)"
                           'face 'error)))))))

;;;###autoload
(defun hermes-skills-hub (&optional profile)
  "Browse the passive Skill Hub for PROFILE on the owning backend."
  (interactive)
  (let ((buffer (hermes-skills--new-buffer
                 "*Hermes Skill Hub*" #'hermes-skills-hub-mode
                 (hermes-instance-resolve) profile (current-buffer))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer (hermes-skills-hub-refresh))
    buffer))

(defun hermes-skills-hub-search ()
  "Explicitly search configured Hub sources over the network."
  (interactive nil hermes-skills-hub-mode)
  (let* ((buffer (current-buffer)) (active (hermes-skills--guard))
         (query (read-string "Search configured skill sources (network): ")))
    (when (funcall active)
      (with-current-buffer buffer
        (setq hermes-skills--query query)
        (hermes-skills-hub-refresh)))))

(defun hermes-skills-hub-official ()
  "Return to the backend's official optional catalog."
  (interactive nil hermes-skills-hub-mode)
  (hermes-skills--require-owner)
  (setq hermes-skills--query nil)
  (hermes-skills-hub-refresh))

(defun hermes-skills-hub-profile ()
  "Choose an exact backend profile and replace the current Hub catalog."
  (interactive nil hermes-skills-hub-mode)
  (let ((active (hermes-skills--guard)) (buffer (current-buffer)))
    (hermes-browser--run-owned
     (lambda (client guard)
       (hermes-skills--api client guard nil "GET" "/api/profiles"))
     active
     (lambda (result)
       (let* ((names (mapcar (lambda (row) (hermes-transport--get row 'name))
                             (hermes-transport--get result 'profiles)))
              (name (and names (completing-read "Hub profile: " names nil t))))
         (when (and (funcall active) (member name names))
           (with-current-buffer buffer
             (setq hermes-skills--profile name)
             (hermes-skills-hub-refresh)))))
     #'hermes-browser--read-error)))

(defun hermes-skills--show-text (text active)
  "Display inert TEXT in this view under exact operation ACTIVE."
  (when (hermes-skills--publish text active)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (setq hermes-browser--status "Read-only; b back; ? help")))

(defun hermes-skills--source-flag (source key)
  "Return yes, no or unknown for the backend SOURCE flag KEY."
  (if (seq-some (lambda (candidate)
                  (not (eq (hermes-transport--member-value source candidate)
                           hermes-transport--missing)))
                (hermes-transport--key-candidates key))
      (if (hermes-transport--get source key) "yes" "no")
    "unknown"))

(defun hermes-skills-hub-sources ()
  "Show configured source provenance and availability without probing files."
  (interactive nil hermes-skills-hub-mode)
  (hermes-skills--require-owner)
  (unless hermes-skills--sources (user-error "No source snapshot; refresh first"))
  (let ((sources (hermes-transport--get hermes-skills--sources 'sources))
        (buffer (hermes-skills--new-buffer
                 "*Hermes Skill Sources*" #'hermes-skill-preview-mode
                 hermes-instance hermes-skills--profile (current-buffer))))
    (with-current-buffer buffer
      (hermes-skills--show-text
       (concat "Configured sources (backend-reported)\n\n"
               (if sources
                   (mapconcat
                    (lambda (source)
                      (format "%s [%s]\n  available: %s; searchable: %s; rate limited: %s\n"
                              (hermes-transport--get source 'label)
                              (hermes-transport--get source 'id)
                              (hermes-skills--source-flag source 'available)
                              (hermes-skills--source-flag source 'searchable)
                              (hermes-skills--source-flag source 'rate_limited)))
                    sources "\n")
                 "No configured sources reported.\n"))
       (hermes-skills--guard)))
    (pop-to-buffer buffer)))

(defun hermes-skills-hub--preview-text (result)
  "Format inert candidate RESULT and label the unreviewed file manifest."
  (format "%s\nSource: %s\nIdentifier: %s\nTrust (backend-reported): %s\nRepository: %s\n\nSupporting file names only; contents NOT reviewed:\n%s\n\nSKILL.md (inert text; not scanned):\n\n%s"
          (hermes-transport--get result 'name) (hermes-transport--get result 'source)
          (hermes-transport--get result 'identifier)
          (hermes-transport--get result 'trust_level) (hermes-transport--get result 'repo)
          (mapconcat (lambda (file) (concat "  " file))
                     (hermes-transport--get result 'files) "\n")
          (or (hermes-transport--get result 'skill_md) "")))

(defun hermes-skills-hub-preview ()
  "Preview the exact candidate at point without scanning or installing it."
  (interactive nil hermes-skills-hub-mode)
  (hermes-skills--require-owner)
  (let* ((candidate (hermes-browser--copy-identity (tabulated-list-get-id)))
         (parent (current-buffer))
         (parent-view (hermes-skills--guard))
         (parent-guard
          (lambda ()
            (and (funcall parent-view)
                 (with-current-buffer parent
                   (equal candidate (tabulated-list-get-id))))))
         (profile hermes-skills--profile))
    (unless candidate (user-error "No candidate on this line"))
    (let ((buffer (hermes-skills--new-buffer
                   "*Hermes Skill Preview*" #'hermes-skill-preview-mode
                   hermes-instance profile parent)))
      (with-current-buffer buffer
        (setq hermes-skills--candidate candidate
              hermes-skills--origin-current-p parent-guard))
      (pop-to-buffer buffer)
      (with-current-buffer buffer
        (hermes-skills-hub-preview-refresh))
      buffer)))

(defun hermes-skills-hub-preview-refresh ()
  "Read this exact candidate again without scanning it."
  (interactive nil hermes-skill-preview-mode)
  (hermes-skills--require-owner)
  (unless hermes-skills--candidate (user-error "This view has no candidate"))
  (let ((active (hermes-skills--guard))
        (profile hermes-skills--profile)
        (candidate (hermes-browser--copy-identity hermes-skills--candidate)))
    (setq hermes-browser--status "Loading")
    (hermes-skills--run-owned
     (lambda (client guard)
       (hermes-skills--api client guard profile "GET" "/api/skills/hub/preview"
                           `((identifier . ,(cadr candidate)))))
     active
     (lambda (result operation)
       (unless (equal candidate (list (hermes-transport--get result 'source)
                                     (hermes-transport--get result 'identifier)))
         (error "Preview identity does not match the selected source"))
       (hermes-skills--show-text (hermes-skills-hub--preview-text result) operation))
     #'hermes-browser--read-error)))

(defun hermes-skills-hub--scan-text (result)
  "Format RESULT as a policy assessment, not a security guarantee."
  (format "\n\nExplicit scan (not a security guarantee)\nPolicy: %s\nReason: %s\nVerdict: %s\n%s\nFindings: %S\nAdvisory tier1: %s\n"
          (hermes-transport--get result 'policy) (hermes-transport--get result 'policy_reason)
          (hermes-transport--get result 'verdict) (hermes-transport--get result 'summary)
          (hermes-transport--get result 'findings)
          (if-let* ((tier1 (hermes-transport--get result 'tier1)))
              (format "%S" tier1)
            "Absent/unavailable/failed; no advisory assurance")))

(defun hermes-skills-hub-scan ()
  "Explicitly request a quarantined backend scan after informed consent."
  (interactive nil hermes-skill-preview-mode)
  (hermes-skills--require-owner)
  (unless hermes-skills--candidate (user-error "This view has no candidate"))
  (let ((buffer (current-buffer)) (active (hermes-skills--guard))
        (profile hermes-skills--profile) (candidate hermes-skills--candidate))
    (when (and (yes-or-no-p
                "Scan fetches/quarantines files on backend and may run an external scanner.  Continue? ")
               (funcall active))
      (with-current-buffer buffer
        (setq hermes-browser--status "Loading")
        (hermes-skills--run-owned
         (lambda (client guard)
           (hermes-skills--api client guard profile "GET" "/api/skills/hub/scan"
                               `((identifier . ,(cadr candidate)))))
         active
         (lambda (result operation)
           (unless (equal (cadr candidate) (hermes-transport--get result 'identifier))
             (error "Scan identifier mismatch"))
           (when (hermes-skills--publish
                  (concat (save-restriction (widen) (buffer-string))
                          (hermes-skills-hub--scan-text result)) operation)
             (set-buffer-modified-p nil)
             (setq hermes-browser--status "Scan reported; not an installation or safety guarantee")))
         #'hermes-browser--read-error)))))

(provide 'hermes-skills)
;;; hermes-skills.el ends here
