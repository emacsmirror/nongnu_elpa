;;; hermes-endpoints.el --- Named custom endpoints -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Backend-owned named endpoint configuration.  Drafting, probing, saving and
;; activation are separate actions.  Secrets remain in memory until submitted;
;; only an explicit Clear sends an empty key.  No mutation is retried.

;;; Code:

(require 'hermes-browser)
(require 'hermes-onboarding)
(require 'url-parse)
(require 'url-util)

(defconst hermes-endpoints--path "/api/providers/custom-endpoints")
(defconst hermes-endpoints--python-space
  "[\t-\r\u001c-\u0020\u0085\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000]"
  "Whitespace stripped by the released backend's Python str.strip.
Emacs `string-trim' and its space character class differ from Python.")
(defvar-local hermes-endpoints--profile nil "Profile owning this view.")
(defvar-local hermes-endpoints--snapshot nil "Last authoritative endpoint list.")
(defvar-local hermes-endpoints--draft nil "In-memory draft; never printed wholesale.")
(defvar-local hermes-endpoints--probe nil "Last probe observations for this draft.")

(defun hermes-endpoints--guard ()
  "Capture the owned view, profile, draft and backend before input."
  (unless (or (hermes-buffer--owned-p 'hermes-endpoint-edit-mode)
              (hermes-buffer--owned-p 'hermes-endpoint-list-mode))
    (user-error "Reopen the endpoint view"))
  (when hermes-browser--owned-cleanup
    (user-error "Operation pending; wait or refresh to reconcile"))
  (let ((context (hermes-browser--mutation-context))
        (owner (hermes-browser--owned-predicate
                '(hermes-endpoints--profile hermes-endpoints--draft))))
    (lambda () (and (funcall context) (funcall owner)))))

(defun hermes-endpoints--request (client guard profile method suffix &optional body)
  "Request METHOD at SUFFIX with BODY on CLIENT under GUARD for PROFILE."
  (hermes-dashboard-transport-api-request-async
   method (concat hermes-endpoints--path suffix) :client client
   :current-p guard :query (hermes-onboarding--profile-query profile)
   :body body :secrets (list (alist-get 'api_key body))))

(defun hermes-endpoints--failure (_reason)
  "Report failure without echoing potentially secret-bearing REASON."
  (setq hermes-browser--status "Failed/uncertain; g reconcile; no automatic retry")
  (force-mode-line-update)
  (message "Hermes endpoint request failed; reconcile before retrying"))

(defun hermes-endpoints--rows (result)
  "Return secret-free tabulated rows from RESULT."
  (mapcar
   (lambda (row)
     (list (hermes-transport--get row 'id)
           (vector
            (hermes-browser--face-cell (hermes-transport--get row 'name)
                                      'hermes-browser-name)
            (if (eq t (hermes-transport--get row 'is_current)) "Active" "")
            (hermes-transport--display-field row 'model)
            (hermes-transport--display-field row 'base_url))))
   (hermes-transport--get result 'endpoints)))

(defun hermes-endpoints--accept-list (result)
  "Publish endpoint RESULT and its active model configuration."
  (hermes-endpoint-list--render result)
  (setq hermes-endpoints--snapshot (hermes-endpoints--redact result))
  (let ((current (hermes-transport--get result 'current)))
    (setq hermes-browser--status
          (format "Active provider: %s; model: %s; URL: %s"
                  (hermes-transport--display-field current 'provider)
                  (hermes-transport--display-field current 'model)
                  (hermes-transport--display-field current 'base_url)))))

(defun hermes-endpoints-refresh ()
  "Reconcile this endpoint view without discarding its draft."
  (interactive nil hermes-endpoint-list-mode hermes-endpoint-edit-mode)
  (hermes-browser--retire-owned)
  (let ((guard (hermes-endpoints--guard))
        (profile hermes-endpoints--profile))
    (setq hermes-browser--status "Loading")
    (hermes-browser--run-owned
     (lambda (client active)
       (hermes-endpoints--request client active profile "GET" ""))
     guard
     (lambda (result)
       (if (derived-mode-p 'hermes-endpoint-list-mode)
           (hermes-endpoints--accept-list result)
         (setq hermes-endpoints--snapshot (hermes-endpoints--redact result)
               hermes-browser--status "Reconciled; draft retained")
         (hermes-endpoints--render-draft)))
     #'hermes-endpoints--failure)))

(defun hermes-endpoints--row ()
  "Return the selected saved endpoint, refusing synthetic direct config rows."
  (let ((row (seq-find
              (lambda (row) (equal (tabulated-list-get-id)
                                  (hermes-transport--get row 'id)))
              (hermes-transport--get hermes-endpoints--snapshot 'endpoints))))
    (unless row (user-error "No saved endpoint selected"))
    (unless (equal (hermes-transport--get row 'source) "providers")
      (user-error "This legacy/direct endpoint requires backend migration first"))
    row))

(defun hermes-endpoints--draft-from-row (row)
  "Build an editable draft from ROW, omitting credentials and API mode.
Omitting API mode preserves hand-written transport settings.  The backend
merges existing model metadata; do not reconstruct that map from list labels."
  `((id . ,(or (hermes-transport--get row 'id) ""))
    (name . ,(or (hermes-transport--get row 'name) ""))
    (base_url . ,(or (hermes-transport--get row 'base_url) ""))
    (model . ,(or (hermes-transport--get row 'model) ""))
    (discover_models . ,(if (or (null row)
                               (eq t (hermes-transport--get row 'discover_models)))
                           t :false))))

(defun hermes-endpoints--open-draft (row)
  "Open a retained draft for ROW, or a new endpoint when nil."
  (let ((instance hermes-instance)
        (profile hermes-endpoints--profile)
        (snapshot hermes-endpoints--snapshot)
        (buffer (hermes-buffer--get
                 (generate-new-buffer-name "*Hermes Endpoint Draft*")
                 #'hermes-endpoint-edit-mode)))
    (with-current-buffer buffer
      (hermes-browser--own-instance instance)
      (setq hermes-endpoints--profile profile
            hermes-endpoints--snapshot snapshot
            hermes-endpoints--draft (hermes-endpoints--draft-from-row row))
      (hermes-endpoints--render-draft))
    (pop-to-buffer buffer)))

(defun hermes-endpoints-new ()
  "Open a new unsaved named endpoint draft."
  (interactive nil hermes-endpoint-list-mode)
  (hermes-endpoints--guard)
  (hermes-endpoints--open-draft nil))

(defun hermes-endpoints-edit ()
  "Edit the selected endpoint without saving or testing it."
  (interactive nil hermes-endpoint-list-mode)
  (hermes-endpoints--guard)
  (hermes-endpoints--open-draft (hermes-endpoints--row)))

(defun hermes-endpoints--safe (value)
  "Return display VALUE redacted against the in-memory draft credential."
  (hermes-dashboard-transport--redact-secret
   (format "%s" (or value "")) (list (alist-get 'api_key hermes-endpoints--draft))))

(defun hermes-endpoints--redact (value)
  "Copy VALUE, removing the entered credential before retaining readback."
  (cond ((stringp value) (hermes-endpoints--safe value))
        ((consp value) (cons (hermes-endpoints--redact (car value))
                            (hermes-endpoints--redact (cdr value))))
        ((vectorp value) (apply #'vector (mapcar #'hermes-endpoints--redact value)))
        (t value)))

(defun hermes-endpoints--render-draft ()
  "Render non-secret fields, probe observations and saved readback."
  (let ((inhibit-read-only t)
        (draft hermes-endpoints--draft)
        (probe hermes-endpoints--probe))
    (erase-buffer)
    (insert (propertize "Named endpoint draft\n" 'face 'bold))
    (dolist (field '(id name base_url model discover_models api_mode context_length))
      (insert (format "%s: " field)
              (propertize (hermes-endpoints--safe
                           (if (assq field draft) (alist-get field draft) "Preserve"))
                          'face 'font-lock-constant-face) "\n"))
    (insert "API key: " (if (assq 'api_key draft)
                             (if (equal "" (alist-get 'api_key draft)) "Clear" "Replace")
                           "Preserve (not loaded)") "\n\n")
    (when probe
      (insert (format "Probe ok: %s; reachable: %s; transport checked: %s\n"
                      (eq t (hermes-transport--get probe 'ok))
                      (eq t (hermes-transport--get probe 'reachable))
                      (hermes-endpoints--safe (hermes-transport--get probe 'transport_checked))))
      (insert "Resolved URL (not adopted): "
              (hermes-endpoints--safe (hermes-transport--get probe 'resolved_base_url))
              "\nModels: " (hermes-endpoints--safe (hermes-transport--get probe 'models))
              "\nBackend observation: "
              (hermes-endpoints--safe (hermes-transport--get probe 'message))
              "\nObservations do not prove inference/authentication: 401/429 and\n"
              "inconclusive transport timeouts can still yield ok.\n\n"))
    (when hermes-endpoints--snapshot
      (insert "Saved backend readback (draft above is separate):\n")
      (dolist (row (hermes-transport--get hermes-endpoints--snapshot 'endpoints))
        (insert (format "%s | %s | %s | %s\n"
                        (hermes-endpoints--safe (hermes-transport--get row 'id))
                        (hermes-endpoints--safe (hermes-transport--get row 'model))
                        (hermes-endpoints--safe (hermes-transport--get row 'base_url))
                        (if (eq t (hermes-transport--get row 'is_current)) "Active" ""))))
      (let ((current (hermes-transport--get hermes-endpoints--snapshot 'current)))
        (insert "Active model config: "
                (hermes-endpoints--safe (hermes-transport--get current 'provider)) " / "
                (hermes-endpoints--safe (hermes-transport--get current 'model)) " / "
                (hermes-endpoints--safe (hermes-transport--get current 'base_url)) "\n")))
    (insert "\nSave is last-write-wins upsert; it can recreate a concurrently deleted\n"
            "endpoint. Updating an active provider can affect future runtime use.\n"
            "No atomic edit/existence guarantee. Test neither saves nor activates.\n")
    (goto-char (point-min))))

(defun hermes-endpoints-field ()
  "Edit one non-secret draft field, retaining all other settings."
  (interactive nil hermes-endpoint-edit-mode)
  (let* ((guard (hermes-endpoints--guard))
         (buffer (current-buffer))
         (field (intern (completing-read
                         "Field: " '("name" "base_url" "model" "discover_models"
                                     "api_mode" "context_length") nil t))))
    (when (funcall guard)
      (let ((value
             (pcase field
               ('discover_models (if (y-or-n-p "Discover models? ") t :false))
               ('api_mode (completing-read "API mode (empty = auto): "
                                          '("" "chat_completions" "codex_responses"
                                            "anthropic_messages") nil t))
               ('context_length (read-number "Context length (positive): "))
               (_ (read-string (format "%s: " field)
                               (with-current-buffer buffer
                                 (alist-get field hermes-endpoints--draft)))))))
        (when (funcall guard)
          (when (and (eq field 'context_length) (not (and (integerp value) (> value 0))))
            (user-error "Context length must be a positive integer; omission preserves it"))
          (with-current-buffer buffer
            (setf (alist-get field hermes-endpoints--draft) value)
            (setq hermes-endpoints--probe nil)
            (hermes-endpoints--render-draft)))))))

(defun hermes-endpoints--empty-stored-key-p (key)
  "Return non-nil when the released backend would store KEY as empty.
Hermes Agent 0.21.4 strips Python boundary whitespace, then removes CR/LF
and all non-ASCII characters in `save_env_value'.  Quoting the remaining
value for dotenv does not remove characters.  Test for a surviving ASCII
character after the boundary strip; never rewrite an admitted KEY."
  (let ((space (concat hermes-endpoints--python-space "+")))
    (not (string-match-p "[\u0000-\u0009\u000b\u000c\u000e-\u007f]"
                         (string-trim key space space)))))

(defun hermes-endpoints-key ()
  "Choose Preserve, Replace or explicit Clear for the draft API key.
Refuse persistence-empty replacements; retain other keys verbatim."
  (interactive nil hermes-endpoint-edit-mode)
  (let* ((guard (hermes-endpoints--guard))
         (buffer (current-buffer))
         (choice (completing-read "API key action: " '("Preserve" "Replace" "Clear") nil t)))
    (when (funcall guard)
      (let ((key (pcase choice
                   ("Replace" (read-passwd "New endpoint API key: "))
                   ("Clear" "") (_ nil))))
        (when (funcall guard)
          (when (and (equal choice "Replace")
                     (hermes-endpoints--empty-stored-key-p key))
            (user-error "Replacement would become empty in backend storage; use explicit Clear"))
          (with-current-buffer buffer
            (setq hermes-endpoints--draft
                  (assq-delete-all 'api_key hermes-endpoints--draft)
                  hermes-endpoints--probe nil)
            (when key (push (cons 'api_key key) hermes-endpoints--draft))
            (hermes-endpoints--render-draft)))))))

(defun hermes-endpoints--body ()
  "Return a validated wire draft with explicit nonactivation."
  (let ((key (alist-get 'api_key hermes-endpoints--draft)))
    (when (and key (not (string-empty-p key))
               (hermes-endpoints--empty-stored-key-p key))
      (user-error "Replacement would become empty in backend storage; choose a key policy again")))
  (dolist (field '(name base_url model))
    (unless (hermes-transport--non-blank-string (alist-get field hermes-endpoints--draft))
      (user-error "Complete name, base URL and model first")))
  (let* ((url (url-generic-parse-url (alist-get 'base_url hermes-endpoints--draft)))
         (type (url-type url)))
    (unless (and (member type '("http" "https")) (url-host url)
                 (not (url-user url)) (not (url-password url))
                 (not (url-target url))
                 (not (string-match-p "?" (url-filename url))))
      (user-error "Use an HTTP(S) base URL without credentials, query or fragment")))
  (cons '(make_default . :false) (copy-tree hermes-endpoints--draft)))

(defun hermes-endpoints--probe-body (body)
  "Return BODY with the saved effective API mode for a preservation draft.
Save omits an unedited mode to retain hand-written settings, but Test must
probe that mode rather than silently choosing the backend's auto default."
  (let* ((row (seq-find
               (lambda (row) (equal (alist-get 'id body)
                                   (hermes-transport--get row 'id)))
               (hermes-transport--get hermes-endpoints--snapshot 'endpoints)))
         (mode (hermes-transport--get row 'api_mode)))
    (if (and row (not (assq 'api_mode body)) (stringp mode))
        (cons (cons 'api_mode mode) body)
      body)))

(defun hermes-endpoints-test ()
  "Probe this draft after explicit consent; never save or activate it."
  (interactive nil hermes-endpoint-edit-mode)
  (let* ((guard (hermes-endpoints--guard))
         (buffer (current-buffer))
         (profile hermes-endpoints--profile)
         (body (hermes-endpoints--probe-body (hermes-endpoints--body))))
    (when (and (yes-or-no-p
                (format (concat "Test %s (API mode %s) via backend %s? Sends entered credentials; "
                                "stored keys are NOT loaded. Probes alternate /v1 paths "
                                "and may incur paid inference (Chat/Messages 1 token; "
                                "Responses 16 max output tokens). No save/activation. Proceed? ")
                        (alist-get 'base_url body)
                        (or (hermes-transport--non-blank-string (alist-get 'api_mode body)) "auto")
                        (hermes-instance-name hermes-instance)))
               (funcall guard))
      (with-current-buffer buffer
        (setq hermes-browser--status "Loading")
        (hermes-browser--run-owned
         (lambda (client active)
           (hermes-endpoints--request client active profile "POST" "/validate" body))
         guard
         (lambda (result)
           (setq hermes-endpoints--probe (hermes-endpoints--redact result)
                 hermes-browser--status "Probe observations")
           (hermes-endpoints--render-draft))
         #'hermes-endpoints--failure)))))

(defun hermes-endpoints-adopt-probe ()
  "Explicitly adopt the probe's resolved URL and advertised model metadata."
  (interactive nil hermes-endpoint-edit-mode)
  (let ((guard (hermes-endpoints--guard))
        (buffer (current-buffer))
        (probe hermes-endpoints--probe))
    (unless (hermes-transport--get probe 'resolved_base_url)
      (user-error "No resolved probe URL to adopt"))
    (when (and (yes-or-no-p
                (format "Adopt resolved URL %s and advertised model metadata into draft? "
                        (hermes-endpoints--safe (hermes-transport--get probe 'resolved_base_url))))
               (funcall guard))
      (with-current-buffer buffer
        (setf (alist-get 'base_url hermes-endpoints--draft)
              (hermes-transport--get probe 'resolved_base_url)
              (alist-get 'models hermes-endpoints--draft)
              (vconcat (hermes-transport--get probe 'models))
              (alist-get 'model_details hermes-endpoints--draft)
              (vconcat (hermes-transport--get probe 'model_details)))
        (setq hermes-endpoints--probe nil)
        (hermes-endpoints--render-draft)))))

(defun hermes-endpoints--mutate (guard method suffix body)
  "Send METHOD/SUFFIX/BODY under GUARD, then read the same profile back."
  (let ((profile hermes-endpoints--profile)
        saved-id)
    (setq hermes-browser--status "Saving")
    (hermes-browser--run-owned
     (lambda (client active)
       (hermes--promise-then
        (hermes-endpoints--request client active profile method suffix body)
        (lambda (receipt)
          (setq saved-id (hermes-transport--get receipt 'id))
          (if (funcall active)
              (hermes-endpoints--request client active profile "GET" "")
            (hermes--promise-rejected "Retired endpoint readback")))))
     guard
     (lambda (result)
       (if (derived-mode-p 'hermes-endpoint-list-mode)
           (hermes-endpoints--accept-list result)
         (setq hermes-endpoints--snapshot (hermes-endpoints--redact result)
               hermes-endpoints--draft (assq-delete-all 'api_key hermes-endpoints--draft)
               hermes-browser--status "Saved; inspect normalized backend readback")
         (when (hermes-transport--non-blank-string saved-id)
           (setf (alist-get 'id hermes-endpoints--draft) saved-id))
         (hermes-endpoints--render-draft))
       (hermes-dashboard-transport-invalidate-model-options))
     #'hermes-endpoints--failure)))

(defun hermes-endpoints-save ()
  "Save this draft with make_default false and inspect authoritative readback."
  (interactive nil hermes-endpoint-edit-mode)
  (let* ((guard (hermes-endpoints--guard))
         (buffer (current-buffer))
         (body (hermes-endpoints--body)))
    (when (and (yes-or-no-p
                (format (concat "Save %s on %s / %s? Last-write-wins upsert may recreate "
                                "a deleted endpoint. An active provider edit can affect future "
                                "runtime use; this does NOT activate. Proceed? ")
                        (alist-get 'name body) (hermes-instance-name hermes-instance)
                        (or hermes-endpoints--profile "launch profile")))
               (funcall guard))
      (with-current-buffer buffer (hermes-endpoints--mutate guard "POST" "" body)))))

(defun hermes-endpoints--actionable-id-p (id)
  "Return non-nil when released Activate/Delete can retain literal ID.
Do not use the new-endpoint slug grammar for existing stored keys.  Hermes
Agent 0.21.4 activation replaces ASCII spaces and treats a leading custom:
as a namespace; deletion's comparison does not undo those transformations.
Boundary whitespace aliases another key through Python str.strip.  Slash
and dot segments cannot be addressed by the HTTP route.  Other punctuation,
case and Unicode remain literal request data, not client-normalized IDs."
  (let ((case-fold-search t)
        (space hermes-endpoints--python-space))
    (and (stringp id) (not (string-empty-p id))
         (not (member id '("." "..")))
         (not (string-match-p "[ /]\\|\\`custom:" id))
         (not (string-match-p (concat "\\`" space "\\|" space "\\'") id)))))

(defun hermes-endpoints--saved-action (delete)
  "Confirm activation or, when DELETE is non-nil, removal of the saved row."
  (let* ((guard (hermes-endpoints--guard))
         (buffer (current-buffer))
         (row (hermes-endpoints--row))
         (id (copy-sequence (hermes-transport--get row 'id)))
         (name (hermes-transport--get row 'name)))
    (unless (hermes-endpoints--actionable-id-p id)
      (user-error "Stored ID has a released backend identity/addressing hazard; no action sent"))
    (when (and (yes-or-no-p
                (format "%s %s [%s] on %s / %s: %s Proceed? "
                        (if delete "Delete" "Activate") name id
                        (hermes-instance-name hermes-instance)
                        (or hermes-endpoints--profile "launch profile")
                        (if delete
                            "If active, detach provider/URL/key from main model; remove its managed key."
                          "Replace the main model provider, URL and credentials.")))
               (funcall guard))
      (with-current-buffer buffer
        (hermes-endpoints--mutate
         guard (if delete "DELETE" "POST")
         (concat "/" (url-hexify-string id) (unless delete "/activate")) nil)))))

(defun hermes-endpoints-activate ()
  "Confirm activation of the saved endpoint at point."
  (interactive nil hermes-endpoint-list-mode)
  (hermes-endpoints--saved-action nil))

(defun hermes-endpoints-delete ()
  "Confirm deletion and possible active-provider detachment at point."
  (interactive nil hermes-endpoint-list-mode)
  (hermes-endpoints--saved-action t))

(hermes-define-list-browser endpoint-list
  :title "Hermes Endpoints" :buffer "*Hermes Endpoints*"
  :command hermes-endpoints--list
  :command-modes (hermes-endpoint-list-mode)
  :on-mode (lambda ()
             (setq-local hermes-browser--snapshot-variables
                         '(hermes-endpoints--snapshot hermes-endpoints--profile)))
  :columns [("Name" 24 t) ("State" 8 t) ("Model" 28 t) ("Base URL" 40 t)]
  :refresh #'hermes-endpoints-refresh
  :rows #'hermes-endpoints--rows
  :help (:group "Draft" hermes-endpoints-new "New" hermes-endpoints-edit "Edit"
         :group "Saved" hermes-endpoints-activate "Activate" hermes-endpoints-delete "Delete")
  :keys ("c" #'hermes-endpoints-new "RET" #'hermes-endpoints-edit
         "e" #'hermes-endpoints-edit "a" #'hermes-endpoints-activate
         "d" #'hermes-endpoints-delete))

(defvar-keymap hermes-endpoint-edit-mode-map
  :parent special-mode-map
  "e" #'hermes-endpoints-field "k" #'hermes-endpoints-key
  "t" #'hermes-endpoints-test "s" #'hermes-endpoints-save
  "u" #'hermes-endpoints-adopt-probe "g" #'hermes-endpoints-refresh)

(keymap-popup-annotate hermes-endpoint-edit-mode-map
  :popup-key "?" :exit-key "C-g" :description "Endpoint draft"
  :group "Draft" hermes-endpoints-field "Edit field" hermes-endpoints-key "Key policy"
  :group "Probe" hermes-endpoints-test "Test (may charge)"
  hermes-endpoints-adopt-probe "Adopt observations"
  :group "Persist" hermes-endpoints-save "Save (not activate)"
  hermes-endpoints-refresh "Reconcile" quit-window "Quit view")

(define-derived-mode hermes-endpoint-edit-mode special-mode "Endpoint Draft"
  "Edit a retained named endpoint draft through field commands."
  :interactive nil
  (hermes-browser--setup-status))

;;;###autoload
(defun hermes-endpoints ()
  "Browse named custom endpoints on this backend and profile.
Test can transmit credentials and incur inference charges; it requires
separate consent.  Saving does not activate a new provider."
  (interactive)
  (let ((instance (hermes-instance-resolve))
        (profile (if (derived-mode-p 'hermes-endpoint-list-mode 'hermes-endpoint-edit-mode)
                     hermes-endpoints--profile
                   (hermes-onboarding--current-profile)))
        (buffer (hermes-buffer--get "*Hermes Endpoints*" #'hermes-endpoint-list-mode)))
    (with-current-buffer buffer
      (hermes-browser--own-instance instance)
      (unless (equal profile hermes-endpoints--profile)
        (setq hermes-endpoints--snapshot nil tabulated-list-entries nil)
        (tabulated-list-print t))
      (setq hermes-endpoints--profile profile))
    (pop-to-buffer buffer)
    (with-current-buffer buffer (hermes-endpoints-refresh))))

(provide 'hermes-endpoints)
;;; hermes-endpoints.el ends here
