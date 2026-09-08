;;; hermes-system.el --- Gateway status and logs for Hermes  -*- lexical-binding: t; -*-

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

;; Native read-only views over dashboard status and log REST endpoints.

;;; Code:

(require 'pp)
(require 'subr-x)
(require 'keymap-popup)
(require 'hermes-dashboard-transport)
(require 'hermes-browser)

(defvar-local hermes-system--heading nil
  "Heading rendered by the current system buffer.")

(defvar-local hermes-system--path nil
  "REST path fetched by the current system buffer.")

(defvar-local hermes-system--query nil
  "REST query fetched by the current system buffer.")

(defun hermes-system--api (client path &optional query)
  "Return dashboard GET PATH promise through CLIENT with QUERY."
  (hermes-dashboard-transport-api-request-async
   "GET" path :query query :client client))

(defun hermes-system--redact-text (text)
  "Return management TEXT with credential-shaped values redacted."
  (let ((case-fold-search t)
        (safe (hermes-dashboard-transport--redact-secret text)))
    (setq safe
          (replace-regexp-in-string
           "\\(bearer[ \t]+\\)[^[:space:],;}]+" "\\1<redacted>" safe t nil))
    (replace-regexp-in-string
     "\\(\\(?:api[-_ ]?key\\|token\\|secret\\|password\\)[\"']?[ \t]*[:=][ \t]*[\"']?\\)[^\"'[:space:],;}]+"
     "\\1<redacted>" safe t nil)))

(defun hermes-system--result-text (result)
  "Return display text for system RESULT."
  (if-let* ((reason (hermes-transport--get result 'error)))
      (concat (propertize "Error: " 'face 'error) (format "%s" reason))
    (if-let* ((lines (hermes-transport--get result 'lines)))
        (string-join lines "\n")
      (pp-to-string result))))

(defun hermes-system--render (buffer result &optional secrets)
  "Render RESULT in live system BUFFER, redacting captured SECRETS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'hermes-system-mode)
        (let ((inhibit-read-only t)
              (line (line-number-at-pos))
              (column (current-column))
              (windows
               (mapcar (lambda (window)
                         (save-excursion
                           (goto-char (window-point window))
                           (list window
                                 (line-number-at-pos (window-start window))
                                 (save-excursion
                                   (goto-char (window-start window))
                                   (current-column))
                                 (line-number-at-pos) (current-column))))
                       (get-buffer-window-list buffer nil t))))
          (erase-buffer)
          (insert (propertize hermes-system--heading 'face 'bold) "\n\n")
          (insert (hermes-system--redact-text
                   (hermes-dashboard-transport--redact-secret
                    (if (and (equal hermes-system--path "/api/logs")
                             (not (hermes-transport--get result 'error))
                             (not (hermes-transport--get result 'lines)))
                        "No log lines"
                      (hermes-system--result-text result))
                    secrets)))
          (pcase-dolist (`(,window ,start ,start-column ,window-line ,window-column)
                        windows)
            (goto-char (point-min))
            (forward-line (1- start))
            (move-to-column start-column)
            (set-window-start window (point) t)
            (goto-char (point-min))
            (forward-line (1- window-line))
            (move-to-column window-column)
            (set-window-point window (point)))
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column column))))))

(defvar-local hermes-system--auto-refresh nil
  "Non-nil means refresh the visible log tail after each five-second pause.")

(defvar-local hermes-system--timer nil
  "One-shot refresh timer owned by this buffer.")

(defun hermes-system--cancel-timer ()
  "Cancel this buffer's pending refresh timer."
  (when hermes-system--timer
    (cancel-timer hermes-system--timer)
    (setq hermes-system--timer nil)))

(defun hermes-system--stop ()
  "Quietly retire this system view's requests and polling."
  (hermes-browser--next-request-generation)
  (hermes-system--cancel-timer)
  (setq hermes-system--auto-refresh nil))

(defun hermes-system--visibility-change (&optional _window)
  "Stop refreshing when this buffer is no longer displayed."
  (unless (get-buffer-window (current-buffer) t)
    (hermes-system--stop)))

(defun hermes-system--client-scope (client)
  "Return CLIENT's endpoint and connection lifetime, without credentials.
The log endpoint is scoped to the server process's profile, not a chat's
profile selection; the API has no profile parameter."
  (when (hermes-dashboard-transport-client-p client)
    (list (hermes-dashboard-transport-client-generation client)
          (hermes-dashboard-transport--api-client-base-url client))))

(defun hermes-system--schedule (buffer current-p)
  "Schedule BUFFER's next poll if CURRENT-P still grants ownership."
  (when (and (funcall current-p) hermes-system--auto-refresh)
    (let ((generation hermes-browser--request-generation))
      (setq hermes-system--timer
            (run-at-time
             5 nil
             (lambda ()
               (when (hermes-browser--request-current-mode-p
                      buffer generation 'hermes-system-mode)
                 (with-current-buffer buffer
                   (if (and (funcall current-p)
                            (get-buffer-window buffer t))
                       (hermes-system--fetch buffer)
                     (hermes-system--stop))))))))))

(defun hermes-system--fetch (buffer)
  "Fetch and render the REST view owned by BUFFER."
  (with-current-buffer buffer
    (hermes-system--cancel-timer)
    (let* ((generation (hermes-browser--next-request-generation))
           (instance hermes-instance)
           (instance-value (copy-tree hermes-instance))
           (path hermes-system--path)
           (query (copy-tree hermes-system--query))
           (visible (get-buffer-window buffer t))
           client scope secrets completed connection-current
           (current-p
            (lambda ()
              (and (hermes-browser--request-current-mode-p
                    buffer generation 'hermes-system-mode)
                   (eq instance (buffer-local-value 'hermes-instance buffer))
                   (equal instance-value instance)
                   (if completed connection-current
                     (equal scope (hermes-system--client-scope client)))
                   (equal (cdr scope)
                          (cdr (hermes-system--client-scope client)))
                   (or (not visible) (get-buffer-window buffer t)))))
           (settle
            (lambda (result)
              (when (hermes-browser--request-current-mode-p
                     buffer generation 'hermes-system-mode)
                (with-current-buffer buffer
                  (if (funcall current-p)
                      (progn
                        (hermes-system--render buffer result secrets)
                        (hermes-system--schedule buffer current-p))
                    (hermes-system--stop)))))))
      (hermes-browser--run-on-client
       (lambda (owner)
         (setq client owner
               scope (copy-tree (hermes-system--client-scope owner))
               secrets (when (hermes-dashboard-transport-client-p owner)
                         (copy-tree
                          (cons (hermes-dashboard-transport-client-token owner)
                                (cons (hermes-dashboard-transport-client-auth-token owner)
                                      (hermes-dashboard-transport-client-secrets owner))))))
         (hermes--promise-finally
          (condition-case err
              (hermes-system--api owner path query)
            ((error quit)
             (hermes--promise-rejected (error-message-string err))))
          (lambda ()
            ;; Validate the response before the browser releases its client.
            ;; Completed HTTP requests and future polls no longer own that
            ;; connection, but still belong to the exact instance and endpoint.
            (setq connection-current
                  (equal scope (hermes-system--client-scope client))
                  completed t))))
       settle
       (lambda (reason) (funcall settle (list :error reason)))))))

(defun hermes-system--open (buffer-name heading path &optional query)
  "Open BUFFER-NAME for HEADING fetched from PATH with QUERY."
  (let ((instance (hermes-instance-resolve))
        (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (hermes-system-mode)
      (hermes-browser--own-instance instance)
      (setq-local header-line-format '(:eval (hermes-system--header-line)))
      (setq hermes-system--heading heading
            hermes-system--path path
            hermes-system--query query))
    (pop-to-buffer buffer)
    (hermes-system--fetch buffer)))

(defun hermes-system-status ()
  "Show gateway status in a native buffer."
  (interactive)
  (hermes-system--open "*Hermes Status*" "Hermes Gateway Status"
                       "/api/status"))

(defun hermes-system--bounded-log-lines (lines)
  "Return requested log LINES clamped to the backend's 1..500 tail range."
  (min 500 (max 1 (if lines (prefix-numeric-value lines) 100))))

(defun hermes-system-logs (&optional lines)
  "Show the server profile's agent log tail, limited to LINES.
Use \\<hermes-system-mode-map>\\[hermes-system-mode-map-popup] for filters
and auto-refresh controls.  With no prefix, request 100 lines."
  (interactive "P")
  (hermes-system--open
   "*Hermes Logs*" "Hermes Logs" "/api/logs"
   `((file . "agent") (lines . ,(hermes-system--bounded-log-lines lines)))))

(defun hermes-system--header-line ()
  "Return faced instance and log state for the current system view."
  (concat
   (hermes-browser--instance-header-line)
   (when (equal hermes-system--path "/api/logs")
     (concat
      " "
      (mapconcat
       (lambda (field)
         (concat (propertize (car field) 'face 'shadow)
                 (propertize (cdr field) 'face 'font-lock-type-face)))
       `(("Source " . ,(or (alist-get 'file hermes-system--query) "agent"))
         ("Min " . ,(or (alist-get 'level hermes-system--query) "ALL"))
         ("Component " . ,(or (alist-get 'component hermes-system--query) "all"))
         ("" . ,(format "%s lines" (alist-get 'lines hermes-system--query))))
       " · ")
      (propertize " · Auto " 'face 'shadow)
      (propertize (if hermes-system--auto-refresh "5s" "off")
                  'face (if hermes-system--auto-refresh 'success 'shadow))
      " · " (propertize "? Help" 'face 'help-key-binding)))))

(defun hermes-system--filter-description (label key)
  "Return LABEL with this buffer's bounded log query value for KEY."
  (let ((value (if (not (equal hermes-system--path "/api/logs")) "not applicable"
                 (format "%s" (or (alist-get key hermes-system--query)
                                  (alist-get key '((file . "agent") (level . "ALL")
                                                   (component . "all") (lines . 100))))))))
    (concat label ": "
            (propertize (truncate-string-to-width value 24 nil nil t)
                        'face 'keymap-popup-value 'help-echo value))))

(defun hermes-system--read-filter (key prompt &optional choices)
  "Read query KEY using PROMPT and CHOICES without outliving the owner."
  (unless (and (derived-mode-p 'hermes-system-mode)
               (equal hermes-system--path "/api/logs"))
    (user-error "Not a Hermes log buffer"))
  (let* ((buffer (current-buffer))
         (generation hermes-browser--request-generation)
         (instance hermes-instance)
         (query (copy-tree hermes-system--query))
         (prompt (concat (hermes-system--filter-description prompt key) "; new value: "))
         (value (if choices (completing-read prompt choices nil t)
                  (read-number prompt (or (alist-get key query) 100)))))
    (unless (and (hermes-browser--request-current-mode-p buffer generation 'hermes-system-mode)
                 (eq (current-buffer) buffer)
                 (equal instance hermes-instance)
                 (equal query hermes-system--query)
                 (equal hermes-system--path "/api/logs"))
      (user-error "Log view changed while choosing"))
    value))

(defun hermes-system--log-option (key value &optional choices)
  "Set log query KEY to VALUE and refresh, validating against CHOICES."
  (unless (and (derived-mode-p 'hermes-system-mode)
               (equal hermes-system--path "/api/logs"))
    (user-error "Not a Hermes log buffer"))
  (when (and choices (not (member value choices)))
    (user-error "Invalid log %s: %s" key value))
  (setq hermes-system--query (copy-tree hermes-system--query))
  (setf (alist-get key hermes-system--query) value)
  (force-mode-line-update)
  (hermes-system--fetch (current-buffer)))

(defun hermes-system-log-source (source)
  "Show log SOURCE: agent, errors, or gateway."
  (interactive (list (hermes-system--read-filter 'file "Log source"
                                     '("agent" "errors" "gateway"))))
  (hermes-system--log-option 'file source '("agent" "errors" "gateway")))

(defun hermes-system-log-level (level)
  "Show log lines at minimum LEVEL, or use ALL for no level filter."
  (interactive (list (hermes-system--read-filter 'level "Minimum log level"
                                     '("ALL" "DEBUG" "INFO" "WARNING" "ERROR"))))
  (hermes-system--log-option 'level level '("ALL" "DEBUG" "INFO" "WARNING" "ERROR")))

(defun hermes-system-log-component (component)
  "Show log lines for COMPONENT, or use all for no component filter."
  (interactive (list (hermes-system--read-filter 'component "Log component"
                                     '("all" "gateway" "agent" "tools" "cli" "cron"))))
  (hermes-system--log-option 'component component
                           '("all" "gateway" "agent" "tools" "cli" "cron")))

(defun hermes-system-log-lines (lines)
  "Set the requested log tail to LINES, clamped to 1..500."
  (interactive (list (hermes-system--read-filter 'lines "Log tail lines")))
  (unless (integerp lines) (user-error "Log lines must be an integer"))
  (hermes-system--log-option 'lines (hermes-system--bounded-log-lines lines)))

(defun hermes-system-log-auto-refresh ()
  "Toggle log polling, waiting five seconds after each completed request.
Polling starts disabled and stops when the view is hidden or replaced.
Requests never accumulate automatically while a previous poll is pending."
  (interactive)
  (unless (equal hermes-system--path "/api/logs")
    (user-error "Not a Hermes log buffer"))
  (if hermes-system--auto-refresh
      (hermes-system--stop)
    (setq hermes-system--auto-refresh t)
    (hermes-system--fetch (current-buffer)))
  (force-mode-line-update))

(defun hermes-system--revert (&rest _)
  "Refresh the current Hermes system buffer."
  (hermes-system--fetch (current-buffer)))

(defvar hermes-system-mode-map)

(keymap-popup-define hermes-system-mode-map
  "Keymap for Hermes status and log buffers."
  :parent special-mode-map
  :popup-key "?"
  :exit-key "C-g"
  :description (lambda () (or hermes-system--heading "Hermes System"))
  :group ("Filter" :inapt-if (lambda () (not (equal hermes-system--path "/api/logs"))))
  "s" ((lambda () (hermes-system--filter-description "Source" 'file))
       hermes-system-log-source :stay-open t)
  "l" ((lambda () (hermes-system--filter-description "Min level" 'level))
       hermes-system-log-level :stay-open t)
  "c" ((lambda () (hermes-system--filter-description "Component" 'component))
       hermes-system-log-component :stay-open t)
  "n" ((lambda () (hermes-system--filter-description "Tail lines" 'lines))
       hermes-system-log-lines :stay-open t)
  :group "View"
  ;; Native switches only assign a variable; polling also owns request cleanup.
  "a" ((lambda () (format "Auto-refresh: %s" (if hermes-system--auto-refresh "5s" "off")))
       hermes-system-log-auto-refresh :stay-open t
       :inapt-if (lambda () (not (equal hermes-system--path "/api/logs"))))
  "g" ("Refresh" revert-buffer :stay-open t)
  "q" ("Quit view" quit-window)
  "?" ("Help" hermes-system-mode-map-popup))

(define-derived-mode hermes-system-mode special-mode "Hermes System"
  "Major mode for Hermes gateway status and server-profile logs.
\\{hermes-system-mode-map}"
  :interactive nil
  (setq-local revert-buffer-function #'hermes-system--revert)
  (add-hook 'kill-buffer-hook #'hermes-system--stop nil t)
  (add-hook 'change-major-mode-hook #'hermes-system--stop nil t)
  (add-hook 'window-buffer-change-functions
            #'hermes-system--visibility-change nil t))

(provide 'hermes-system)
;;; hermes-system.el ends here
