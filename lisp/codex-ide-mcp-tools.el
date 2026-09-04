;;; codex-ide-mcp-tools.el --- MCP harness tools for Codex  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Public harness helpers and the narrow MCP registry that exposes them.

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'project)
(require 'subr-x)
(require 'xref)
(require 'codex-ide-mcp-core)
(require 'codex-ide-mcp-treesit)

;;; Harness state

(defconst codex-ide-harness-event-limit 500
  "Maximum number of harness events retained in memory.")

(defconst codex-ide-harness-job-limit 32
  "Maximum number of harness jobs retained in memory.")

(defconst codex-ide-harness-job-output-limit (* 256 1024)
  "Maximum characters retained per harness job output string.")

(defconst codex-ide-harness-event-field-limit (* 32 1024)
  "Maximum characters retained for a single harness event string field.")

(defvar codex-ide-harness--event-cursor 0
  "Monotonic cursor for harness events.")

(defvar codex-ide-harness--events nil
  "Newest-first list of recent harness events.")

(defvar codex-ide-harness--jobs (make-hash-table :test 'equal)
  "Hash table of async harness jobs keyed by string id.")

(defvar codex-ide-harness--next-job-id 0
  "Counter used to allocate harness job ids.")

;;; Shared data helpers

(defun codex-ide-harness--non-empty-string-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value)
       (not (string-empty-p (string-trim value)))))

(defun codex-ide-harness--time-string ()
  "Return the current time as an ISO-like string."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun codex-ide-harness--truncate-string (value &optional limit)
  "Return VALUE truncated to LIMIT characters with an honest marker."
  (let ((cap (or limit codex-ide-harness-event-field-limit)))
    (if (and (stringp value)
             (integerp cap)
             (> cap 0)
             (> (length value) cap))
        (concat (substring value 0 cap)
                (format "\n...[truncated %d chars]" (- (length value) cap)))
      value)))

(defun codex-ide-harness--bound-event-data (data)
  "Return DATA after truncating oversized string fields for retention."
  (cond
   ((stringp data)
    (codex-ide-harness--truncate-string data))
   ((vectorp data)
    (vconcat (mapcar #'codex-ide-harness--bound-event-data data)))
   ((listp data)
    (if (and data (not (consp (car data))))
        (mapcar #'codex-ide-harness--bound-event-data data)
      (mapcar (lambda (cell)
                (if (consp cell)
                    (cons (car cell)
                          (codex-ide-harness--bound-event-data (cdr cell)))
                  cell))
              data)))
   (t data)))

(defun codex-ide-harness--record-event (type data)
  "Record a harness event of TYPE with DATA."
  (setq codex-ide-harness--event-cursor
        (1+ codex-ide-harness--event-cursor))
  (let ((event (list (cons "cursor" codex-ide-harness--event-cursor)
                     (cons "time" (codex-ide-harness--time-string))
                     (cons "type" type)
                     (cons "data" (codex-ide-harness--bound-event-data data)))))
    (push event codex-ide-harness--events)
    (when (> (length codex-ide-harness--events)
             codex-ide-harness-event-limit)
      (setcdr (nthcdr (1- codex-ide-harness-event-limit)
                      codex-ide-harness--events)
              nil))
    event))

(defun codex-ide-harness-reset ()
  "Cancel live jobs and clear retained harness jobs/events."
  (maphash
   (lambda (_id job)
     (when-let* ((process (plist-get job :process)))
       (when (process-live-p process)
         (ignore-errors (delete-process process)))))
   codex-ide-harness--jobs)
  (clrhash codex-ide-harness--jobs)
  (setq codex-ide-harness--events nil
        codex-ide-harness--event-cursor 0)
  nil)

(defun codex-ide-harness--events-since (since limit)
  "Return recent events after SINCE, bounded by LIMIT."
  (let* ((cursor (if (integerp since) since 0))
         ;; `cl-remove-if-not' can return the input list itself, so a
         ;; destructive reverse here would corrupt the global event log.
         (events (cl-remove-if-not
                  (lambda (event)
                    (> (cdr (assoc "cursor" event)) cursor))
                  codex-ide-harness--events))
         (ordered (reverse events))
         (bounded (if (> (length ordered) limit)
                      (last ordered limit)
                    ordered)))
    (list (cons "cursor" codex-ide-harness--event-cursor)
          (cons "events" (vconcat bounded)))))

(defun codex-ide-harness--buffer-named (name)
  "Return live buffer named NAME, or signal `user-error'."
  (let ((buffer (and (codex-ide-harness--non-empty-string-p name)
                     (get-buffer name))))
    (unless (buffer-live-p buffer)
      (user-error "No live buffer named %s" name))
    buffer))

(defun codex-ide-harness--buffer-for-args (args &optional live-only)
  "Return the buffer selected by ARGS.
When LIVE-ONLY is non-nil, require an explicit buffer or open file path."
  (let ((buffer (codex-ide-mcp--object-get args "buffer"))
        (path (codex-ide-mcp--object-get args "path")))
    (cond
     ((codex-ide-harness--non-empty-string-p buffer)
      (codex-ide-harness--buffer-named buffer))
     ((codex-ide-harness--non-empty-string-p path)
      (if live-only
          (codex-ide-mcp--buffer-for-path path)
        (find-file-noselect (expand-file-name path))))
     (live-only (user-error "Edit requires an explicit buffer or path"))
     (t (current-buffer)))))

(defun codex-ide-harness--directory-for-args (args buffer)
  "Return the effective directory for ARGS and BUFFER."
  (let ((directory (codex-ide-mcp--object-get args "directory")))
    (cond
     ((codex-ide-harness--non-empty-string-p directory)
      (let ((expanded (file-name-as-directory
                       (expand-file-name directory))))
        (unless (file-directory-p expanded)
          (user-error "Directory does not exist: %s" expanded))
        expanded))
     ((buffer-live-p buffer)
      (with-current-buffer buffer default-directory))
     (t default-directory))))

(defun codex-ide-harness--context (args &optional live-only)
  "Return `(BUFFER DIRECTORY)' selected by ARGS.
When LIVE-ONLY is non-nil, path lookup requires an existing live buffer."
  (let* ((buffer (codex-ide-harness--buffer-for-args args live-only))
         (directory (codex-ide-harness--directory-for-args args buffer)))
    (list buffer directory)))

(defun codex-ide-harness--buffer-summary (&optional buffer)
  "Return JSON-ready metadata for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((file (buffer-file-name))
          (root (codex-ide-mcp--buffer-project-root)))
      (delq nil
            (list (cons "buffer" (buffer-name))
                  (when file (cons "path" (expand-file-name file)))
                  (cons "directory" default-directory)
                  (when root (cons "projectRoot" root))
                  (cons "majorMode" (symbol-name major-mode))
                  (cons "modified" (codex-ide-mcp--json-false
                                    (buffer-modified-p)))
                  (cons "readOnly" (codex-ide-mcp--json-false
                                    buffer-read-only))
                  (cons "point" (codex-ide-mcp--line-column)))))))

(defun codex-ide-harness--region-summary ()
  "Return JSON-ready metadata for the active region."
  (let* ((active (use-region-p))
         (beg (if active (region-beginning) (point)))
         (end (if active (region-end) (point)))
         (text (if active (buffer-substring-no-properties beg end) ""))
         (truncated (> (length text) codex-ide-mcp-selection-content-limit))
         (content (if truncated
                      (substring text 0 codex-ide-mcp-selection-content-limit)
                    text)))
    (list (cons "active" (codex-ide-mcp--json-false active))
          (cons "range" (codex-ide-mcp--range beg end))
          (cons "text" content)
          (cons "truncated" (codex-ide-mcp--json-false truncated)))))

(defun codex-ide-harness--project-summary (&optional buffer)
  "Return project metadata for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((root (codex-ide-mcp--buffer-project-root))
           (file-count
            (if root
                (condition-case nil
                    (if-let* ((project (project-current nil root)))
                        (length (project-files project))
                      0)
                  (error 0))
              0)))
      (list (cons "root" root)
            (cons "fileCount" file-count)))))

(defun codex-ide-harness--window-summary (window)
  "Return JSON-ready metadata for WINDOW."
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (window-point window))
        (delq nil
              (list (cons "buffer" (buffer-name buffer))
                    (when buffer-file-name
                      (cons "path" (expand-file-name buffer-file-name)))
                    (cons "selected" (codex-ide-mcp--json-false
                                      (eq window (selected-window))))
                    (cons "point" (codex-ide-mcp--line-column))
                    (cons "start" (codex-ide-mcp--line-column
                                   (window-start window)))))))))

(defun codex-ide-harness--window-summaries ()
  "Return JSON-ready metadata for visible windows."
  (vconcat (mapcar #'codex-ide-harness--window-summary
                   (window-list nil 'no-minibuf))))

(defun codex-ide-harness--message-tail (limit)
  "Return up to LIMIT recent lines from `*Messages*'."
  (if-let* ((buffer (get-buffer "*Messages*")))
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-max))
            (forward-line (- limit))
            (vconcat (split-string
                      (buffer-substring-no-properties (point) (point-max))
                      "\n" t)))))
    []))

(defun codex-ide-harness--modified-buffers ()
  "Return summaries for modified live buffers."
  (vconcat
   (delq nil
         (mapcar
          (lambda (buffer)
            (when (and (buffer-live-p buffer)
                       (buffer-modified-p buffer))
              (codex-ide-harness--buffer-summary buffer)))
          (buffer-list)))))

;;; Diagnostics, xref, and imenu helpers

(defun codex-ide-mcp--xref-location-line (location)
  "Return one-based line number for xref LOCATION, or nil."
  (condition-case nil
      (or (xref-location-line location)
          (and-let* ((marker (xref-location-marker location)))
            (and (markerp marker)
                 (marker-buffer marker)
                 (with-current-buffer (marker-buffer marker)
                   (line-number-at-pos marker)))))
    (error nil)))

(defun codex-ide-mcp--xref-item->entry (item)
  "Return a JSON-ready entry for xref ITEM, or nil."
  (condition-case nil
      (let ((location (xref-item-location item)))
        (list (cons "file" (or (condition-case nil
                                   (xref-location-group location)
                                 (error nil))
                               ""))
              (cons "line" (or (codex-ide-mcp--xref-location-line
                                 location)
                                0))
              (cons "summary" (or (xref-item-summary item) ""))))
    (error nil)))

(defun codex-ide-mcp--xref-items->entries (items)
  "Return JSON-ready entries for xref ITEMS."
  (delq nil (mapcar #'codex-ide-mcp--xref-item->entry items)))

(defun codex-ide-mcp--imenu-position-line (position)
  "Return one-based line number for imenu POSITION, or nil."
  (cond
   ((markerp position)
    (when (marker-buffer position)
      (with-current-buffer (marker-buffer position)
        (line-number-at-pos position))))
   ((integerp position)
    (line-number-at-pos position))
   (t nil)))

(defun codex-ide-mcp--imenu-flatten (index &optional category)
  "Return flat JSON-ready imenu entries from INDEX.
CATEGORY names the parent group when recursing into sublists."
  (delq nil
        (mapcan
         (lambda (item)
           (let ((name (car item))
                 (value (cdr item)))
             (cond
              ((not (stringp name)) nil)
              ((string-prefix-p "*" name) nil)
              ((imenu--subalist-p item)
               (codex-ide-mcp--imenu-flatten value name))
              ((or (markerp value) (integerp value))
               (and-let* ((line (codex-ide-mcp--imenu-position-line
                                  value)))
                 (list (list (cons "name" name)
                             (cons "category" (or category ""))
                             (cons "line" line)))))
              (t nil))))
         (or index nil))))

(defun codex-ide-mcp--flymake-diagnostics ()
  "Return available Flymake diagnostics for the current buffer."
  (when (and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-diagnostics)
             (fboundp 'flymake-diagnostic-beg)
             (fboundp 'flymake-diagnostic-end)
             (fboundp 'flymake-diagnostic-type)
             (fboundp 'flymake-diagnostic-text))
    (mapcar
     (lambda (diag)
       (let ((beg (funcall (symbol-function 'flymake-diagnostic-beg) diag))
             (end (funcall (symbol-function 'flymake-diagnostic-end) diag))
             (type (funcall (symbol-function 'flymake-diagnostic-type) diag))
             (text (funcall (symbol-function 'flymake-diagnostic-text) diag)))
         (list (cons "source" "flymake")
               (cons "type" (format "%s" type))
               (cons "message" text)
               (cons "range" (codex-ide-mcp--range beg end)))))
     (funcall (symbol-function 'flymake-diagnostics)
              (point-min) (point-max)))))

(defun codex-ide-mcp--flycheck-diagnostics ()
  "Return available Flycheck diagnostics for the current buffer."
  (when (and (boundp 'flycheck-current-errors)
             (fboundp 'flycheck-error-line)
             (fboundp 'flycheck-error-column)
             (fboundp 'flycheck-error-level)
             (fboundp 'flycheck-error-message)
             (fboundp 'flycheck-error-filename))
    (let ((file (buffer-file-name)))
      (cl-loop for err in (symbol-value 'flycheck-current-errors)
               for filename = (funcall (symbol-function
                                        'flycheck-error-filename)
                                       err)
               when (or (not file)
                        (not filename)
                        (equal (file-truename file)
                               (file-truename filename)))
               collect
               (let* ((line (or (funcall (symbol-function
                                          'flycheck-error-line)
                                         err)
                                1))
                      (column (or (funcall (symbol-function
                                            'flycheck-error-column)
                                           err)
                                  0))
                      (pos (save-excursion
                             (goto-char (point-min))
                             (forward-line (max 0 (1- line)))
                             (move-to-column column)
                             (point))))
                 (list (cons "source" "flycheck")
                       (cons "type" (format "%s" (funcall
                                                   (symbol-function
                                                    'flycheck-error-level)
                                                   err)))
                       (cons "message" (funcall (symbol-function
                                                  'flycheck-error-message)
                                                 err))
                       (cons "range" (codex-ide-mcp--range pos pos))))))))

(defun codex-ide-harness-diagnostics (&optional args)
  "Return already-known diagnostics for ARGS or the current buffer."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (buffer (if (codex-ide-harness--non-empty-string-p path)
                     (codex-ide-mcp--buffer-for-path path)
                   (current-buffer))))
    (with-current-buffer buffer
      (vconcat (append (codex-ide-mcp--flymake-diagnostics)
                       (codex-ide-mcp--flycheck-diagnostics))))))

(defun codex-ide-harness-xref (args)
  "Return xref data described by ARGS.
The `action' field is `references' or `apropos'."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (action (or (codex-ide-mcp--object-get args "action")
                     "references"))
         (identifier (codex-ide-mcp--object-get args "identifier"))
         (pattern (codex-ide-mcp--object-get args "pattern"))
         (buffer (codex-ide-mcp--buffer-for-path path)))
    (with-current-buffer buffer
      (if-let* ((backend (xref-find-backend)))
          (pcase action
            ("references"
             (unless (codex-ide-harness--non-empty-string-p identifier)
               (user-error "Xref references requires identifier"))
             (vconcat
              (codex-ide-mcp--xref-items->entries
               (xref-backend-references backend identifier))))
            ("apropos"
             (unless (codex-ide-harness--non-empty-string-p pattern)
               (user-error "Xref apropos requires pattern"))
             (vconcat
              (codex-ide-mcp--xref-items->entries
               (xref-backend-apropos backend pattern))))
            (_ (user-error "Unknown xref action: %s" action)))
        (user-error "No xref backend available for %s" path)))))

(defun codex-ide-harness-imenu (args)
  "Return imenu symbols for the open buffer described by ARGS."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (buffer (codex-ide-mcp--buffer-for-path path)))
    (with-current-buffer buffer
      (vconcat (codex-ide-mcp--imenu-flatten
                (imenu--make-index-alist t))))))

(defun codex-ide-harness-tree-sitter (&optional args)
  "Return tree-sitter data for ARGS or the current buffer."
  (codex-ide-mcp--tree-sitter-info args))

;;; Execution

(defun codex-ide-harness--read-forms (code)
  "Read every Emacs Lisp form from CODE."
  (with-temp-buffer
    (insert code)
    (goto-char (point-min))
    (let (forms done)
      (while (not done)
        (condition-case nil
            (push (read (current-buffer)) forms)
          (end-of-file (setq done t))))
      (nreverse forms))))

(defun codex-ide-harness--eval-forms (forms)
  "Evaluate FORMS and return the final value."
  (let (value)
    (dolist (form forms)
      (setq value (eval form t)))
    value))

(defun codex-ide-harness--backtrace-string ()
  "Return a best-effort backtrace string."
  (with-output-to-string
    (backtrace)))

(defun codex-ide-harness--error-summary (err)
  "Return JSON-ready data for error ERR."
  (list (cons "type" (symbol-name (car err)))
        (cons "message" (error-message-string err))
        (cons "data" (prin1-to-string err t))
        (cons "backtrace" (codex-ide-harness--backtrace-string))))

(defun codex-ide-harness--selected-buffer-summary ()
  "Return metadata for the selected window buffer."
  (if-let* ((window (selected-window)))
      (codex-ide-harness--buffer-summary (window-buffer window))
    (codex-ide-harness--buffer-summary (current-buffer))))

(defun codex-ide-harness--execute-captured (forms output-buffer)
  "Evaluate FORMS with output captured in OUTPUT-BUFFER."
  (let ((original-message (symbol-function 'message))
        messages value error-data)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest message-args)
                 (when format-string
                   (push (apply #'format-message
                                format-string message-args)
                         messages))
                 (apply original-message format-string message-args))))
      (let ((standard-output output-buffer))
        (condition-case err
            (setq value (codex-ide-harness--eval-forms forms))
          (error
           (setq error-data (codex-ide-harness--error-summary err))))))
    (list :value value :error error-data :messages (nreverse messages))))

(defun codex-ide-harness--execute-result (capture output-buffer)
  "Return JSON-ready execution result from CAPTURE and OUTPUT-BUFFER."
  (let ((error-data (plist-get capture :error)))
    (delq nil
          (list (cons "ok" (codex-ide-mcp--json-false
                            (not error-data)))
                (cons "value" (and (not error-data)
                                   (prin1-to-string
                                    (plist-get capture :value) t)))
                (cons "output" (with-current-buffer output-buffer
                                 (buffer-string)))
                (cons "messages" (vconcat (plist-get capture :messages)))
                (cons "currentBuffer" (codex-ide-harness--buffer-summary))
                (cons "selectedBuffer"
                      (codex-ide-harness--selected-buffer-summary))
                (cons "point" (codex-ide-mcp--line-column))
                (cons "modifiedBuffers"
                      (codex-ide-harness--modified-buffers))
                (when error-data (cons "error" error-data))))))

(defun codex-ide-harness-execute (args)
  "Evaluate every form in ARGS's `code' field."
  (let* ((code (codex-ide-mcp--object-get args "code"))
         (forms (and (codex-ide-harness--non-empty-string-p code)
                     (codex-ide-harness--read-forms code))))
    (unless forms
      (user-error "Emacs_execute requires at least one readable form"))
    (pcase-let ((`(,buffer ,directory) (codex-ide-harness--context args)))
      (with-temp-buffer
        (let ((output-buffer (current-buffer)))
          (with-current-buffer buffer
            (let* ((default-directory directory)
                   (capture (codex-ide-harness--execute-captured
                             forms output-buffer))
                   (result (codex-ide-harness--execute-result
                            capture output-buffer)))
              (codex-ide-harness--record-event
               (if (plist-get capture :error) "execute-error" "execute")
               result)
              result)))))))

;;; Editing

(defun codex-ide-harness--truthy-default (args key default)
  "Return boolean field KEY from ARGS, defaulting to DEFAULT."
  (if (codex-ide-mcp--object-has-key-p args key)
      (codex-ide-mcp--truthy-p (codex-ide-mcp--object-get args key))
    default))

(defun codex-ide-harness--validate-position (pos)
  "Return POS when it is inside the current buffer."
  (unless (and (integerp pos) (<= (point-min) pos) (<= pos (point-max)))
    (user-error "Buffer position out of range: %s" pos))
  pos)

(defun codex-ide-harness--line-column-position (line column)
  "Return buffer position for one-based LINE and zero-based COLUMN."
  (unless (and (integerp line) (> line 0))
    (user-error "Line must be a positive integer"))
  (unless (or (null column) (and (integerp column) (>= column 0)))
    (user-error "Column must be a non-negative integer"))
  (save-excursion
    (goto-char (point-min))
    (unless (zerop (forward-line (1- line)))
      (user-error "Line out of range: %s" line))
    (let ((target (or column 0)))
      (move-to-column target)
      (unless (= (current-column) target)
        (user-error "Column out of range on line %s: %s" line target)))
    (point)))

(defun codex-ide-harness--position-from-args
    (args pos-key line-key column-key default)
  "Return a buffer position described by ARGS.
POS-KEY, LINE-KEY, and COLUMN-KEY name position fields, and DEFAULT is
used when no explicit position is present."
  (let ((pos (codex-ide-mcp--object-get args pos-key))
        (line (codex-ide-mcp--object-get args line-key))
        (column (codex-ide-mcp--object-get args column-key)))
    (cond
     ((integerp pos) (codex-ide-harness--validate-position pos))
     ((integerp line) (codex-ide-harness--line-column-position line column))
     (default (codex-ide-harness--validate-position default))
     (t nil))))

(defun codex-ide-harness--edit-result (operation beg end)
  "Return JSON-ready edit result for OPERATION over BEG to END."
  (list (cons "operation" operation)
        (cons "buffer" (codex-ide-harness--buffer-summary))
        (cons "range" (codex-ide-mcp--point-range beg end))
        (cons "point" (codex-ide-mcp--line-column))
        (cons "modified" (codex-ide-mcp--json-false
                          (buffer-modified-p)))))

(defun codex-ide-harness--indent-change (beg end indent)
  "Indent BEG to END when INDENT is non-nil."
  (when (and indent (< beg end))
    (indent-region beg end)))

(defun codex-ide-harness--insert-text (text pos indent)
  "Insert TEXT at POS and return the changed range.
When INDENT is non-nil, indent the inserted region."
  (goto-char pos)
  (let ((beg (point)))
    (insert text)
    (let ((end (point)))
      (codex-ide-harness--indent-change beg end indent)
      (list beg end))))

(defun codex-ide-harness--replace-range (text beg end indent)
  "Replace BEG to END with TEXT and return the changed range."
  (goto-char beg)
  (delete-region beg end)
  (insert text)
  (let ((new-end (point)))
    (codex-ide-harness--indent-change beg new-end indent)
    (list beg new-end)))

(defun codex-ide-harness-edit (args)
  "Apply a structured edit described by ARGS to an explicitly named buffer.
ARGS must specify a buffer name or open file path.
Roll back buffer text changes if editing or indentation exits abnormally."
  (let ((operation (codex-ide-mcp--object-get args "operation"))
        (text (codex-ide-mcp--object-get args "text"))
        (indent (codex-ide-harness--truthy-default args "indent" t)))
    (pcase-let ((`(,buffer ,directory)
                 (codex-ide-harness--context args t)))
      (with-current-buffer buffer
        (let ((default-directory directory))
          (atomic-change-group
            (pcase operation
              ("insert"
               (unless (stringp text)
                 (user-error "Insert requires text"))
               (pcase-let ((`(,beg ,end)
                            (codex-ide-harness--insert-text
                             text
                             (codex-ide-harness--position-from-args
                              args "start" "line" "column" (point))
                             indent)))
                 (codex-ide-harness--edit-result operation beg end)))
              ("replace"
               (unless (stringp text)
                 (user-error "Replace requires text"))
               (let ((beg (codex-ide-harness--position-from-args
                           args "start" "line" "column" nil))
                     (end (codex-ide-harness--position-from-args
                           args "end" "end_line" "end_column" nil)))
                 (unless beg
                   (user-error "Replace requires start or line"))
                 (unless end
                   (user-error "Replace requires end or end_line"))
                 (pcase-let ((`(,new-beg ,new-end)
                              (codex-ide-harness--replace-range
                               text beg end indent)))
                   (codex-ide-harness--edit-result
                    operation new-beg new-end))))
              ("delete"
               (let ((beg (codex-ide-harness--position-from-args
                           args "start" "line" "column" nil))
                     (end (codex-ide-harness--position-from-args
                           args "end" "end_line" "end_column" nil)))
                 (unless beg
                   (user-error "Delete requires start or line"))
                 (unless end
                   (user-error "Delete requires end or end_line"))
                 (delete-region beg end)
                 (codex-ide-harness--edit-result operation beg beg)))
              (_ (user-error "Unknown edit operation: %s" operation)))))))))

(defun codex-ide-harness-insert (text &optional args)
  "Insert TEXT using optional edit ARGS."
  (codex-ide-harness-edit
   (append (list :operation "insert" :text text) args)))

;;; Context

(defun codex-ide-harness-context (&optional args)
  "Return selected buffer, project, windows, region, and state for ARGS."
  (pcase-let ((`(,buffer ,directory)
               (codex-ide-harness--context args)))
    (with-current-buffer buffer
      (let ((default-directory directory))
        (list (cons "buffer" (codex-ide-harness--buffer-summary))
              (cons "project" (codex-ide-harness--project-summary))
              (cons "windows" (codex-ide-harness--window-summaries))
              (cons "region" (codex-ide-harness--region-summary))
              (cons "diagnostics" (codex-ide-harness-diagnostics))
              (cons "messages"
                    (codex-ide-harness--message-tail
                    (codex-ide-mcp--bounded-integer
                     (codex-ide-mcp--object-get args "messages") 40 1)))
              (cons "jobs" (codex-ide-harness--job-summaries))
              (cons "eventsCursor" codex-ide-harness--event-cursor))))))

;;; Jobs

(defun codex-ide-harness--job-terminal-p (job)
  "Return non-nil when JOB already has a terminal status."
  (member (plist-get job :status) '("done" "failed" "canceled")))

(defun codex-ide-harness--prune-jobs ()
  "Drop oldest finished jobs when over `codex-ide-harness-job-limit'."
  (let ((limit (if (and (integerp codex-ide-harness-job-limit)
                        (> codex-ide-harness-job-limit 0))
                   codex-ide-harness-job-limit
                 32)))
    (when (> (hash-table-count codex-ide-harness--jobs) limit)
      (let (finished)
        (maphash
         (lambda (id job)
           (when (codex-ide-harness--job-terminal-p job)
             (push (cons id (or (plist-get job :finished)
                                (plist-get job :started)
                                ""))
                   finished)))
         codex-ide-harness--jobs)
        (setq finished
              (sort finished
                    (lambda (a b)
                      (string-lessp (cdr a) (cdr b)))))
        (while (and finished
                    (> (hash-table-count codex-ide-harness--jobs) limit))
          (remhash (car (pop finished)) codex-ide-harness--jobs))))))

(defun codex-ide-harness--put-job (job)
  "Store JOB and return it."
  (puthash (plist-get job :id) job codex-ide-harness--jobs)
  (codex-ide-harness--prune-jobs)
  job)

(defun codex-ide-harness--update-job (id function)
  "Replace job ID with the result of FUNCTION."
  (when-let* ((job (gethash id codex-ide-harness--jobs)))
    (codex-ide-harness--put-job (funcall function job))))

(defun codex-ide-harness--append-job-output (job chunk)
  "Return JOB with CHUNK appended to its retained output tail."
  (let* ((output (or (plist-get job :output) ""))
         (start (or (plist-get job :output-start) 0))
         (next (+ (or (plist-get job :output-next)
                      (+ start (length output)))
                  (length chunk)))
         (combined (concat output chunk))
         (limit codex-ide-harness-job-output-limit)
         (drop (if (and (integerp limit)
                        (> limit 0)
                        (> (length combined) limit))
                   (- (length combined) limit)
                 0))
         (with-output (plist-put (copy-sequence job) :output
                                 (substring combined drop)))
         (with-start (plist-put with-output :output-start (+ start drop))))
    (plist-put with-start :output-next next)))

(defun codex-ide-harness--job-output (job since)
  "Return JOB output starting at SINCE."
  (let* ((output (or (plist-get job :output) ""))
         (start (or (plist-get job :output-start) 0))
         (next (or (plist-get job :output-next)
                   (+ start (length output))))
         (requested (if (and (integerp since) (>= since 0)) since 0))
         (offset (min next (max start requested))))
    (list (cons "offset" offset)
          (cons "nextOffset" next)
          (cons "truncated"
                (codex-ide-mcp--json-false (< requested start)))
          (cons "text" (substring output (- offset start))))))

(defun codex-ide-harness--job-summary (job &optional output)
  "Return JSON-ready metadata for JOB.
When OUTPUT is non-nil, include output data."
  (delq nil
        (list (cons "id" (plist-get job :id))
              (cons "command" (plist-get job :command))
              (cons "directory" (plist-get job :directory))
              (cons "status" (plist-get job :status))
              (cons "exitCode" (plist-get job :exit-code))
              (cons "started" (plist-get job :started))
              (cons "finished" (plist-get job :finished))
              (cons "outputLength"
                    (or (plist-get job :output-next)
                        (length (or (plist-get job :output) ""))))
              (when output (cons "output" output)))))

(defun codex-ide-harness--job-summaries ()
  "Return summaries for all known harness jobs."
  (let (jobs)
    (maphash (lambda (_id job)
               (push (codex-ide-harness--job-summary job) jobs))
             codex-ide-harness--jobs)
    (vconcat (nreverse jobs))))

(defun codex-ide-harness--job-by-id (id)
  "Return job ID, or signal `user-error'."
  (let ((job (and (codex-ide-harness--non-empty-string-p id)
                  (gethash id codex-ide-harness--jobs))))
    (unless job
      (user-error "Unknown harness job: %s" id))
    job))

(defun codex-ide-harness--job-finished-status (process)
  "Return final status string for PROCESS."
  (if (eq (process-status process) 'exit)
      (if (zerop (process-exit-status process)) "done" "failed")
    "canceled"))

(defun codex-ide-harness--job-filter (id _process string)
  "Append STRING to job ID output."
  (codex-ide-harness--update-job
   id (lambda (job)
        (codex-ide-harness--append-job-output job string))))

(defun codex-ide-harness--job-sentinel (id process _event)
  "Record final state for PROCESS belonging to job ID."
  (unless (eq (process-status process) 'run)
    (let ((current (gethash id codex-ide-harness--jobs)))
      ;; Cancel owns the terminal lifecycle via job-canceled.  Do not
      ;; rewrite status or emit a second terminal event.
      (unless (and current
                   (equal (plist-get current :status) "canceled"))
        (let ((job (codex-ide-harness--update-job
                    id (lambda (old-job)
                         (if (codex-ide-harness--job-terminal-p old-job)
                             old-job
                           (setq old-job
                                 (plist-put old-job :status
                                            (codex-ide-harness--job-finished-status
                                             process)))
                           (setq old-job
                                 (plist-put old-job :exit-code
                                            (process-exit-status process)))
                           (plist-put old-job :finished
                                      (codex-ide-harness--time-string)))))))
          (when (and job
                     (not (equal (plist-get job :status) "canceled")))
            (codex-ide-harness--record-event
             "job-finished" (codex-ide-harness--job-summary job))))))))

(defun codex-ide-harness-start-job (args)
  "Start an async process job described by ARGS."
  (let ((command (codex-ide-mcp--object-get args "command")))
    (unless (codex-ide-harness--non-empty-string-p command)
      (user-error "Job start requires command"))
    (let ((live 0))
      (maphash (lambda (_id job)
                 (unless (codex-ide-harness--job-terminal-p job)
                   (setq live (1+ live))))
               codex-ide-harness--jobs)
      (when (>= live codex-ide-harness-job-limit)
        (user-error "Harness live job limit reached: %d"
                    codex-ide-harness-job-limit)))
    (let* ((buffer (current-buffer))
           (directory (codex-ide-harness--directory-for-args args buffer))
           (id (format "job-%d" (cl-incf codex-ide-harness--next-job-id)))
           (default-directory directory)
           (process (start-file-process-shell-command id nil command))
           (job (list :id id :command command :directory directory
                      :status "running" :exit-code nil
                      :started (codex-ide-harness--time-string)
                      :finished nil :output "" :output-start 0
                      :output-next 0 :process process)))
      (set-process-query-on-exit-flag process nil)
      (set-process-filter
       process (lambda (proc string)
                 (codex-ide-harness--job-filter id proc string)))
      (set-process-sentinel
       process (lambda (proc event)
                 (codex-ide-harness--job-sentinel id proc event)))
      (codex-ide-harness--put-job job)
      (codex-ide-harness--record-event
       "job-started" (codex-ide-harness--job-summary job))
      (codex-ide-harness--job-summary job))))

(defun codex-ide-harness--cancel-job (job)
  "Cancel JOB and return its updated summary.
Already-terminal jobs are left unchanged and emit no new events."
  (if (codex-ide-harness--job-terminal-p job)
      (codex-ide-harness--job-summary job)
    (let ((process (plist-get job :process)))
      (setq job (plist-put job :status "canceled"))
      (setq job (plist-put job :finished
                           (codex-ide-harness--time-string)))
      (codex-ide-harness--put-job job)
      (codex-ide-harness--record-event
       "job-canceled" (codex-ide-harness--job-summary job))
      (when (process-live-p process)
        (ignore-errors (delete-process process)))
      (codex-ide-harness--job-summary job))))

(defun codex-ide-harness-job-result (args)
  "Start, poll, read, or cancel a harness job described by ARGS."
  (let ((action (codex-ide-mcp--object-get args "action"))
        (job-id (codex-ide-mcp--object-get args "job_id"))
        (since (codex-ide-mcp--object-get args "since")))
    (pcase action
      ("start" (codex-ide-harness-start-job args))
      ("poll" (codex-ide-harness--job-summary
               (codex-ide-harness--job-by-id job-id)))
      ("read" (let ((job (codex-ide-harness--job-by-id job-id)))
                (codex-ide-harness--job-summary
                 job (codex-ide-harness--job-output job since))))
      ("cancel" (codex-ide-harness--cancel-job
                 (codex-ide-harness--job-by-id job-id)))
      (_ (user-error "Unknown job action: %s" action)))))

;;; MCP tool wrappers

(defun codex-ide-mcp--tool-execute (args)
  "Evaluate Elisp ARGS through the harness."
  (codex-ide-mcp--json-text-result
   (codex-ide-harness-execute args)))

(defun codex-ide-mcp--tool-context (args)
  "Return harness context for ARGS."
  (codex-ide-mcp--json-text-result
   (codex-ide-harness-context args)))

(defun codex-ide-mcp--tool-edit (args)
  "Apply a live-buffer edit described by ARGS."
  (let ((result (codex-ide-harness-edit args)))
    (codex-ide-harness--record-event "edit" result)
    (codex-ide-mcp--json-text-result result)))

(defun codex-ide-mcp--tool-job (args)
  "Run a harness job action described by ARGS."
  (codex-ide-mcp--json-text-result
   (codex-ide-harness-job-result args)))

(defun codex-ide-mcp--tool-events (args)
  "Return recent harness events described by ARGS."
  (codex-ide-mcp--json-text-result
   (codex-ide-harness--events-since
    (codex-ide-mcp--object-get args "since")
    (codex-ide-mcp--bounded-integer
     (codex-ide-mcp--object-get args "limit") 100 1))))

;;; Tool registry

(defconst codex-ide-mcp--tools
  (list
   (list :name "emacs_execute"
         :description "Evaluate a multi-form Emacs Lisp script."
         :args (list (list :name "code"
                           :type 'string
                           :description "Emacs Lisp forms to evaluate.")
                     (list :name "buffer"
                           :type 'string
                           :description "Optional live buffer name."
                           :optional t)
                     (list :name "path"
                           :type 'string
                           :description "Optional file path context."
                           :optional t)
                     (list :name "directory"
                           :type 'string
                           :description "Optional default directory."
                           :optional t))
         :annotations (list (cons "destructiveHint" t)
                            (cons "openWorldHint" :json-false))
         :function #'codex-ide-mcp--tool-execute)
   (list :name "emacs_context"
         :description "Return selected Emacs harness context."
         :args (list (list :name "buffer"
                           :type 'string
                           :description "Optional live buffer name."
                           :optional t)
                     (list :name "path"
                           :type 'string
                           :description "Optional file path context."
                           :optional t)
                     (list :name "directory"
                           :type 'string
                           :description "Optional default directory."
                           :optional t)
                     (list :name "messages"
                           :type 'integer
                           :description "Number of message lines to include."
                           :optional t))
         :annotations (list (cons "readOnlyHint" t)
                            (cons "idempotentHint" t)
                            (cons "openWorldHint" :json-false))
         :function #'codex-ide-mcp--tool-context)
   (list :name "emacs_edit"
         :description "Edit a live buffer.  Specify buffer or path explicitly."
         :required-any '("buffer" "path")
         :args (list (list :name "operation"
                           :type 'string
                           :description "insert, replace, or delete.")
                     (list :name "text"
                           :type 'string
                           :description "Text for insert or replace."
                           :optional t)
                     (list :name "buffer"
                           :type 'string
                           :description "Optional live buffer name."
                           :optional t)
                     (list :name "path"
                           :type 'string
                           :description "Optional open buffer path."
                           :optional t)
                     (list :name "directory"
                           :type 'string
                           :description "Optional default directory."
                           :optional t)
                     (list :name "start"
                           :type 'integer
                           :description "Start buffer position."
                           :optional t)
                     (list :name "end"
                           :type 'integer
                           :description "End buffer position."
                           :optional t)
                     (list :name "line"
                           :type 'integer
                           :description "One-based start line."
                           :optional t)
                     (list :name "column"
                           :type 'integer
                           :description "Zero-based start column."
                           :optional t)
                     (list :name "end_line"
                           :type 'integer
                           :description "One-based end line."
                           :optional t)
                     (list :name "end_column"
                           :type 'integer
                           :description "Zero-based end column."
                           :optional t)
                     (list :name "indent"
                           :type 'boolean
                           :description "Indent changed text."
                           :optional t))
         :annotations (list (cons "destructiveHint" t)
                            (cons "openWorldHint" :json-false))
         :function #'codex-ide-mcp--tool-edit)
   (list :name "emacs_job"
         :description "Start, poll, read, or cancel async harness jobs."
         :args (list (list :name "action"
                           :type 'string
                           :description "start, poll, read, or cancel.")
                     (list :name "command"
                           :type 'string
                           :description "Shell command for start."
                           :optional t)
                     (list :name "job_id"
                           :type 'string
                           :description "Harness job id."
                           :optional t)
                     (list :name "directory"
                           :type 'string
                           :description "Optional job directory."
                           :optional t)
                     (list :name "since"
                           :type 'integer
                           :description "Output offset for read."
                           :optional t))
         :annotations (list (cons "openWorldHint" t))
         :function #'codex-ide-mcp--tool-job)
   (list :name "emacs_events"
         :description "Return recent Emacs harness events."
         :args (list (list :name "since"
                           :type 'integer
                           :description "Event cursor to read after."
                           :optional t)
                     (list :name "limit"
                           :type 'integer
                           :description "Maximum events to return."
                           :optional t))
         :annotations (list (cons "readOnlyHint" t)
                            (cons "idempotentHint" t)
                            (cons "openWorldHint" :json-false))
         :function #'codex-ide-mcp--tool-events))
  "Registered MCP harness tools.")

(defvar codex-ide-mcp--custom-tools nil
  "Custom tool descriptors, separate from the built-in catalog.")

(defun codex-ide-mcp--catalog ()
  "Return built-in and custom tool descriptors."
  (append codex-ide-mcp--tools codex-ide-mcp--custom-tools))

(defun codex-ide-mcp--valid-tool-name-p (name)
  "Return non-nil when NAME is a supported tool or argument name."
  (and (stringp name) (<= 1 (length name) 128)
       (string-match-p "\\`[a-zA-Z0-9_.-]+\\'" name)))

(defun codex-ide-mcp--validate-custom-schema (args)
  "Reject unsupported or ambiguous argument descriptors in ARGS."
  (unless (proper-list-p args)
    (user-error "Tool arguments must be a proper list"))
  (let (names)
    (dolist (arg args)
      (unless (and (proper-list-p arg) (cl-evenp (length arg)))
        (user-error "Tool argument must be a property list"))
      (let (keys)
        (cl-loop for (key _value) on arg by #'cddr do
                 (unless (and (memq key '(:name :type :description :optional))
                              (not (memq key keys)))
                   (user-error "Unsupported or duplicate argument property: %s" key))
                 (push key keys)))
      (let ((name (plist-get arg :name)))
        (unless (and (codex-ide-mcp--valid-tool-name-p name)
                     (not (member name names))
                     (memq (plist-get arg :type) '(string integer number boolean))
                     (or (not (plist-member arg :description))
                         (stringp (plist-get arg :description)))
                     (memq (plist-get arg :optional) '(nil t)))
          (user-error "Invalid tool argument descriptor: %S" arg))
        (push name names)))))

;;;###autoload
(defun codex-ide-mcp-register-tool (name description args function)
  "Register a custom MCP tool NAME with DESCRIPTION, ARGS and FUNCTION.
ARGS is a list of plists with :name (string), :type (one of string,
integer, number, boolean), optional :description (string), and :optional
\(nil or t).  Unknown properties and duplicate argument names are errors.
FUNCTION receives positional values in ARGS order, with nil for omitted
optional values and t or :json-false for JSON booleans.  Its return value
must be JSON-encodable and is returned as MCP text content.  Errors become
MCP tool errors.  Handlers run synchronously; they must return promptly.
Built-in and duplicate tool names cannot be registered.  Register tools
before starting Codex; restart sessions after changing the catalog since
clients may cache discovery.  Return NAME on success."
  (unless (and (codex-ide-mcp--valid-tool-name-p name)
               (stringp description) (not (string-empty-p description))
               (functionp function))
    (user-error "Invalid tool name, description, or function"))
  (when (codex-ide-mcp--tool-by-name name)
    (user-error "MCP tool already registered: %s" name))
  (codex-ide-mcp--validate-custom-schema args)
  (let ((schema (mapcar (lambda (arg)
                          (cl-loop for (key value) on arg by #'cddr
                                   append (list key (if (stringp value)
                                                        (copy-sequence value)
                                                      value))))
                        args)))
    (setq codex-ide-mcp--custom-tools
          (append codex-ide-mcp--custom-tools
                  (list (list :name (copy-sequence name)
                              :description (copy-sequence description)
                              :args schema :custom t
                              :function
                              (lambda (values)
                                (codex-ide-mcp--json-text-result
                                 (apply function
                                        (mapcar (lambda (arg)
                                                  (codex-ide-mcp--object-get
                                                   values (plist-get arg :name)))
                                                schema)))))))))
  name)

;;;###autoload
(defun codex-ide-mcp-unregister-tool (name)
  "Unregister custom tool NAME, returning non-nil if it existed.
Built-ins cannot be removed.  Restart Codex sessions after catalog changes."
  (when (cl-find name codex-ide-mcp--tools
                 :key (lambda (tool) (plist-get tool :name)) :test #'equal)
    (user-error "Cannot unregister built-in MCP tool: %s" name))
  (when-let* ((tool (cl-find name codex-ide-mcp--custom-tools
                           :key (lambda (item) (plist-get item :name))
                           :test #'equal)))
    (setq codex-ide-mcp--custom-tools (remq tool codex-ide-mcp--custom-tools))
    t))

(defun codex-ide-mcp-tool-names ()
  "Return the names of the local Emacs MCP tools."
  (mapcar (lambda (tool) (copy-sequence (plist-get tool :name)))
          (codex-ide-mcp--catalog)))

(defun codex-ide-mcp--tool-by-name (name)
  "Return registered tool named NAME, or nil."
  (cl-find name (codex-ide-mcp--catalog)
           :key (lambda (tool) (plist-get tool :name))
           :test #'equal))

(provide 'codex-ide-mcp-tools)

;;; codex-ide-mcp-tools.el ends here
