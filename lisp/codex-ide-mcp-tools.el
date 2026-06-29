;;; codex-ide-mcp-tools.el --- MCP tool implementations for Codex  -*- lexical-binding: t; -*-

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

;; Tool implementations and registry for the local MCP bridge.

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'project)
(require 'subr-x)
(require 'xref)
(require 'codex-ide-mcp-core)
(require 'codex-ide-mcp-treesit)

;;; Tool helpers

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

;;; Tool implementations

(defun codex-ide-mcp--tool-current-buffer (_args)
  "Return metadata for the current Emacs buffer."
  (let ((file (buffer-file-name))
        (root (codex-ide-mcp--buffer-project-root)))
    (codex-ide-mcp--json-text-result
     (delq nil
           (list (cons "buffer" (buffer-name))
                 (when file (cons "path" (expand-file-name file)))
                 (when root (cons "projectRoot" root))
                 (cons "majorMode" (symbol-name major-mode))
                 (cons "modified" (codex-ide-mcp--json-false
                                    (buffer-modified-p)))
                 (cons "readOnly" (codex-ide-mcp--json-false
                                    buffer-read-only))
                 (cons "point" (codex-ide-mcp--line-column)))))))

(defun codex-ide-mcp--tool-selection (_args)
  "Return the active region in the current Emacs buffer."
  (let* ((active (use-region-p))
         (beg (if active (region-beginning) (point)))
         (end (if active (region-end) (point)))
         (text (if active (buffer-substring-no-properties beg end) ""))
         (truncated (> (length text) codex-ide-mcp-selection-content-limit))
         (content (if truncated
                      (substring text 0 codex-ide-mcp-selection-content-limit)
                    text)))
    (codex-ide-mcp--json-text-result
     (list (cons "active" (codex-ide-mcp--json-false active))
           (cons "range" (codex-ide-mcp--range beg end))
           (cons "text" content)
           (cons "truncated" (codex-ide-mcp--json-false truncated))))))

(defun codex-ide-mcp--tool-open-file (args)
  "Open a file described by ARGS in Emacs and return the destination."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (line (codex-ide-mcp--object-get args "line"))
         (column (or (codex-ide-mcp--object-get args "column") 0)))
    (unless (and (stringp path) (not (string-empty-p path)))
      (user-error "Emacs_open_file requires a non-empty path"))
    (let ((expanded (expand-file-name path)))
      (unless (file-readable-p expanded)
        (user-error "File is not readable: %s" expanded))
      (let* ((enable-local-variables nil)
             (enable-local-eval nil)
             (buffer (find-file-noselect expanded)))
        (pop-to-buffer buffer)
        (with-current-buffer buffer
          (when line
            (goto-char (point-min))
            (forward-line (max 0 (1- line)))
            (move-to-column column))
          (codex-ide-mcp--json-text-result
           (list (cons "path" (expand-file-name expanded))
                 (cons "buffer" (buffer-name buffer))
                 (cons "point" (codex-ide-mcp--line-column)))))))))

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
               for filename = (funcall (symbol-function 'flycheck-error-filename)
                                       err)
               when (or (not file)
                        (not filename)
                        (equal (file-truename file)
                               (file-truename filename)))
               collect
               (let* ((line (or (funcall (symbol-function 'flycheck-error-line)
                                         err)
                                1))
                      (column (or (funcall (symbol-function 'flycheck-error-column)
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

(defun codex-ide-mcp--tool-diagnostics (args)
  "Return already-known diagnostics for ARGS or the current buffer."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (buffer (if (and (stringp path) (not (string-empty-p path)))
                     (codex-ide-mcp--buffer-for-path path)
                   (current-buffer))))
    (with-current-buffer buffer
      (codex-ide-mcp--json-text-result
       (vconcat (append (codex-ide-mcp--flymake-diagnostics)
                        (codex-ide-mcp--flycheck-diagnostics)))))))

(defun codex-ide-mcp--tool-xref-references (args)
  "Return xref references described by ARGS."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (identifier (codex-ide-mcp--object-get args "identifier"))
         (buffer (codex-ide-mcp--buffer-for-path path)))
    (unless (and (stringp identifier) (not (string-empty-p identifier)))
      (user-error "Tool emacs_xref_references requires a non-empty identifier"))
    (with-current-buffer buffer
      (condition-case err
          (if-let* ((backend (xref-find-backend)))
              (codex-ide-mcp--json-text-result
               (vconcat
                (codex-ide-mcp--xref-items->entries
                 (xref-backend-references backend identifier))))
            (codex-ide-mcp--text-error-result
             (format "No xref backend available for %s" path)))
        (error
         (codex-ide-mcp--text-error-result
          (format "Xref references error: %s"
                  (error-message-string err))))))))

(defun codex-ide-mcp--tool-xref-apropos (args)
  "Return xref apropos matches described by ARGS."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (pattern (codex-ide-mcp--object-get args "pattern"))
         (buffer (codex-ide-mcp--buffer-for-path path)))
    (unless (and (stringp pattern) (not (string-empty-p pattern)))
      (user-error "Tool emacs_xref_apropos requires a non-empty pattern"))
    (with-current-buffer buffer
      (condition-case err
          (if-let* ((backend (xref-find-backend)))
              (codex-ide-mcp--json-text-result
               (vconcat
                (codex-ide-mcp--xref-items->entries
                 (xref-backend-apropos backend pattern))))
            (codex-ide-mcp--text-error-result
             (format "No xref backend available for %s" path)))
        (error
         (codex-ide-mcp--text-error-result
          (format "Xref apropos error: %s"
                  (error-message-string err))))))))

(defun codex-ide-mcp--tool-project-info (_args)
  "Return project metadata for the current buffer."
  (let* ((buffer (current-buffer))
         (root (codex-ide-mcp--buffer-project-root buffer))
         (file-count
          (if root
              (condition-case nil
                  (if-let* ((project (project-current nil root)))
                      (length (project-files project))
                    0)
                (error 0))
            0)))
    (codex-ide-mcp--json-text-result
     (list (cons "root" root)
           (cons "fileCount" file-count)
           (cons "activeBuffer" (buffer-name buffer))
           (cons "majorMode" (symbol-name major-mode))))))

(defun codex-ide-mcp--tool-imenu-symbols (args)
  "Return imenu symbols for the open buffer described by ARGS."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (buffer (codex-ide-mcp--buffer-for-path path)))
    (with-current-buffer buffer
      (condition-case err
          (codex-ide-mcp--json-text-result
           (vconcat
            (codex-ide-mcp--imenu-flatten
             (imenu--make-index-alist t))))
        (error
         (codex-ide-mcp--text-error-result
          (format "Imenu error: %s" (error-message-string err))))))))

(defun codex-ide-mcp--tool-close-buffer (args)
  "Close an Emacs buffer described by ARGS."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (name (codex-ide-mcp--object-get args "buffer"))
         (path-p (and (stringp path) (not (string-empty-p path))))
         (name-p (and (stringp name) (not (string-empty-p name))))
         (label (cond (path-p path) (name-p name) (t "buffer")))
         (buffer (cond (path-p (find-buffer-visiting
                                (expand-file-name path)))
                       (name-p (get-buffer name)))))
    (cond
     ((not (buffer-live-p buffer))
      (codex-ide-mcp--text-error-result
       (format "No open buffer for %s" label)))
     ((with-current-buffer buffer
        (and buffer-file-name (buffer-modified-p)))
      (codex-ide-mcp--text-error-result
       (format "Buffer %s has unsaved changes; not closing"
               (buffer-name buffer))))
     (t
      (let ((closed (buffer-name buffer)))
        (if (kill-buffer buffer)
            (codex-ide-mcp--json-text-result
             (list (cons "closed" closed)))
          (codex-ide-mcp--text-error-result
           (format "Could not close buffer %s" closed))))))))

(defun codex-ide-mcp--tool-execute (args)
  "Evaluate the Emacs Lisp expression in ARGS when enabled."
  (if (not codex-ide-mcp-enable-execute)
      (codex-ide-mcp--text-error-result
       "emacs_execute is disabled; set codex-ide-mcp-enable-execute to enable")
    (let ((code (codex-ide-mcp--object-get args "code")))
      (if (not (and (stringp code)
                    (not (string-empty-p (string-trim code)))))
          (codex-ide-mcp--text-error-result
           "emacs_execute requires a non-empty code argument")
        (condition-case err
            (codex-ide-mcp--json-text-result
             (list (cons "value"
                         (prin1-to-string
                          (eval (car (read-from-string code)) t)
                          t))))
          (error
           (codex-ide-mcp--text-error-result
            (format "Evaluation error: %s"
                    (error-message-string err)))))))))

;;; Tool registry

(defconst codex-ide-mcp--tools
  (list
   (list :name "emacs_current_buffer"
         :description "Return metadata for the current Emacs buffer."
         :args nil
         :function #'codex-ide-mcp--tool-current-buffer)
   (list :name "emacs_selection"
         :description "Return the active Emacs region, if any."
         :args nil
         :function #'codex-ide-mcp--tool-selection)
   (list :name "emacs_open_file"
         :description "Open a file in Emacs and optionally move point."
         :args (list (list :name "path"
                           :type 'string
                           :description "File path to open.")
                     (list :name "line"
                           :type 'integer
                           :description "Optional one-based line number."
                           :optional t)
                     (list :name "column"
                           :type 'integer
                           :description "Optional zero-based column."
                           :optional t))
         :function #'codex-ide-mcp--tool-open-file)
   (list :name "emacs_diagnostics"
         :description "Return already-available Flymake/Flycheck diagnostics."
         :args (list (list :name "path"
                           :type 'string
                           :description "Optional path of an open buffer."
                           :optional t))
         :function #'codex-ide-mcp--tool-diagnostics)
   (list :name "emacs_xref_references"
         :description "Find references to an identifier using xref."
         :args (list (list :name "path"
                           :type 'string
                           :description "Path of an open buffer.")
                     (list :name "identifier"
                           :type 'string
                           :description "Symbol name to find references for."))
         :function #'codex-ide-mcp--tool-xref-references)
   (list :name "emacs_xref_apropos"
         :description "Find symbols matching a pattern using xref."
         :args (list (list :name "path"
                           :type 'string
                           :description "Path of an open buffer.")
                     (list :name "pattern"
                           :type 'string
                           :description "Regexp or substring pattern."))
         :function #'codex-ide-mcp--tool-xref-apropos)
   (list :name "emacs_project_info"
         :description "Return project root, file count, and active buffer."
         :args nil
         :function #'codex-ide-mcp--tool-project-info)
   (list :name "emacs_imenu_symbols"
         :description "Return imenu symbols for an open buffer."
         :args (list (list :name "path"
                           :type 'string
                           :description "Path of an open buffer."))
         :function #'codex-ide-mcp--tool-imenu-symbols)
   (list :name "emacs_tree_sitter_info"
         :description "Return structured tree-sitter node or tree data."
         :args (list (list :name "path"
                           :type 'string
                           :description "Optional path of an open buffer."
                           :optional t)
                     (list :name "line"
                           :type 'integer
                           :description "Optional one-based line number."
                           :optional t)
                     (list :name "column"
                           :type 'integer
                           :description "Optional zero-based column."
                           :optional t)
                     (list :name "whole_file"
                           :type 'boolean
                           :description "Return a bounded root tree."
                           :optional t)
                     (list :name "include_ancestors"
                           :type 'boolean
                           :description "Include ancestors for node output."
                           :optional t)
                     (list :name "include_children"
                           :type 'boolean
                           :description "Include children for node output."
                           :optional t)
                     (list :name "max_depth"
                           :type 'integer
                           :description "Maximum tree depth for whole_file."
                           :optional t)
                     (list :name "max_children"
                           :type 'integer
                           :description "Maximum children per node."
                           :optional t))
         :function #'codex-ide-mcp--tool-tree-sitter-info)
   (list :name "emacs_close_buffer"
         :description "Close an open buffer by path or name."
         :args (list (list :name "path"
                           :type 'string
                           :description "Path of the buffer to close."
                           :optional t)
                     (list :name "buffer"
                           :type 'string
                           :description "Buffer name to close."
                           :optional t))
         :function #'codex-ide-mcp--tool-close-buffer)
   (list :name "emacs_execute"
         :description "Evaluate an Emacs Lisp expression."
         :args (list (list :name "code"
                           :type 'string
                           :description "Emacs Lisp expression to evaluate."))
         :enabled-when 'codex-ide-mcp-enable-execute
         :function #'codex-ide-mcp--tool-execute))
  "Registered MCP tools.")

(defun codex-ide-mcp-tool-names ()
  "Return the names of the local Emacs MCP tools."
  (mapcar (lambda (tool) (plist-get tool :name)) codex-ide-mcp--tools))

(defun codex-ide-mcp--tool-by-name (name)
  "Return registered tool named NAME, or nil."
  (cl-find name codex-ide-mcp--tools
           :key (lambda (tool) (plist-get tool :name))
           :test #'equal))

(defun codex-ide-mcp--tool-enabled-p (tool)
  "Return non-nil when TOOL is currently enabled."
  (let ((gate (plist-get tool :enabled-when)))
    (or (not gate)
        (and (symbolp gate)
             (boundp gate)
             (symbol-value gate)))))

(provide 'codex-ide-mcp-tools)

;;; codex-ide-mcp-tools.el ends here
