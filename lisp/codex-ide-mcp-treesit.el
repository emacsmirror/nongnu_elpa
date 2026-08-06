;;; codex-ide-mcp-treesit.el --- Tree-sitter MCP tool for Codex  -*- lexical-binding: t; -*-

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

;; Lazy tree-sitter helpers for the local MCP bridge.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'codex-ide-mcp-core)

(declare-function treesit-available-p "treesit.c")
(declare-function treesit-parser-language "treesit.c" (parser))
(declare-function treesit-parser-list "treesit.c" (&optional buffer language tag))
(declare-function treesit-parser-root-node "treesit.c" (parser))
(declare-function treesit-node-at "treesit" (pos &optional parser-or-lang named))
(declare-function treesit-node-check "treesit.c" (node property))
(declare-function treesit-node-child "treesit.c" (node n &optional named))
(declare-function treesit-node-child-count "treesit.c" (node &optional named))
(declare-function treesit-node-end "treesit.c" (node))
(declare-function treesit-node-field-name "treesit" (node))
(declare-function treesit-node-parent "treesit.c" (node))
(declare-function treesit-node-start "treesit.c" (node))
(declare-function treesit-node-type "treesit.c" (node))

;;; Constants

(defconst codex-ide-mcp--treesit-preview-limit 160
  "Maximum preview length for tree-sitter node text.")

(defconst codex-ide-mcp--treesit-default-max-depth 2
  "Default depth for bounded tree-sitter tree output.")

(defconst codex-ide-mcp--treesit-default-max-children 40
  "Default child count limit for tree-sitter output.")

(defconst codex-ide-mcp--treesit-required-functions
  '(treesit-parser-list
    treesit-parser-root-node
    treesit-node-at
    treesit-node-check
    treesit-node-child
    treesit-node-child-count
    treesit-node-end
    treesit-node-parent
    treesit-node-start
    treesit-node-type)
  "Runtime tree-sitter functions required by the MCP tool.")

;;; Tree-sitter helpers

(defun codex-ide-mcp--treesit-load ()
  "Load tree-sitter helpers when the runtime provides them."
  (unless (and (fboundp 'treesit-parser-list)
               (fboundp 'treesit-node-at))
    (require 'treesit nil t)))

(defun codex-ide-mcp--treesit-ensure-available ()
  "Signal `user-error' unless tree-sitter can be queried."
  (codex-ide-mcp--treesit-load)
  (unless (and (fboundp 'treesit-available-p)
               (treesit-available-p))
    (user-error "Tree-sitter is not available in this Emacs"))
  (dolist (fn codex-ide-mcp--treesit-required-functions)
    (unless (fboundp fn)
      (user-error "Tree-sitter function is unavailable: %s" fn))))

(defun codex-ide-mcp--treesit-parser ()
  "Return the first tree-sitter parser for the current buffer."
  (codex-ide-mcp--treesit-ensure-available)
  (or (car (treesit-parser-list))
      (user-error "No tree-sitter parser available for buffer %s"
                  (buffer-name))))

(defun codex-ide-mcp--treesit-parser-info (parser)
  "Return JSON-ready metadata for tree-sitter PARSER."
  (delq nil
        (list (when (fboundp 'treesit-parser-language)
                (cons "language"
                      (symbol-name (treesit-parser-language parser)))))))

(defun codex-ide-mcp--treesit-node-preview (beg end)
  "Return a short single-line preview for the node spanning BEG to END."
  (let* ((limit codex-ide-mcp--treesit-preview-limit)
         (preview-end (min end (+ beg limit)))
         (text (buffer-substring-no-properties beg preview-end))
         (single-line (replace-regexp-in-string "[[:space:]\n\r\t]+"
                                                " " (string-trim text))))
    (if (> (length single-line) limit)
        (concat (substring single-line 0 limit) "...")
      single-line)))

(defun codex-ide-mcp--treesit-node-field-name (node)
  "Return NODE's field name when the runtime exposes it."
  (when (fboundp 'treesit-node-field-name)
    (treesit-node-field-name node)))

(defun codex-ide-mcp--treesit-node-summary (node &optional index)
  "Return JSON-ready metadata for tree-sitter NODE.
INDEX is included when non-nil."
  (let* ((beg (treesit-node-start node))
         (end (treesit-node-end node))
         (field (codex-ide-mcp--treesit-node-field-name node)))
    (delq nil
          (list (when index (cons "index" index))
                (cons "type" (treesit-node-type node))
                (cons "named" (codex-ide-mcp--json-false
                               (treesit-node-check node 'named)))
                (when field (cons "fieldName" field))
                (cons "pointRange" (codex-ide-mcp--point-range beg end))
                (cons "byteRange" (codex-ide-mcp--byte-range beg end))
                (cons "range" (codex-ide-mcp--range beg end))
                (cons "childCount" (or (treesit-node-child-count node) 0))
                (cons "text" (codex-ide-mcp--treesit-node-preview
                              beg end))))))

(defun codex-ide-mcp--treesit-child-summaries (node max-children)
  "Return up to MAX-CHILDREN summaries for NODE's children."
  (let ((count (or (treesit-node-child-count node) 0)))
    (cl-loop for index below (min count max-children)
             for child = (treesit-node-child node index)
             when child
             collect (codex-ide-mcp--treesit-node-summary child index))))

(defun codex-ide-mcp--treesit-children-truncated-p (node max-children)
  "Return non-nil when NODE has more children than MAX-CHILDREN."
  (> (or (treesit-node-child-count node) 0) max-children))

(defun codex-ide-mcp--treesit-tree (node max-depth max-children)
  "Return bounded tree-sitter tree data for NODE.
MAX-DEPTH limits recursion and MAX-CHILDREN limits children per node."
  (let ((summary (codex-ide-mcp--treesit-node-summary node)))
    (if (<= max-depth 0)
        summary
      (append
       summary
       (list (cons "children"
                   (vconcat
                    (mapcar (lambda (child)
                              (codex-ide-mcp--treesit-tree
                               child (1- max-depth) max-children))
                            (cl-loop for index below max-children
                                     for child = (treesit-node-child node index)
                                     while child
                                     collect child))))
             (cons "childrenTruncated"
                   (codex-ide-mcp--json-false
                    (codex-ide-mcp--treesit-children-truncated-p
                     node max-children))))))))

(defun codex-ide-mcp--treesit-ancestors (node)
  "Return NODE ancestors from root to parent."
  (let ((parent (treesit-node-parent node))
        ancestors)
    (while parent
      (push (codex-ide-mcp--treesit-node-summary parent) ancestors)
      (setq parent (treesit-node-parent parent)))
    (vconcat ancestors)))

(defun codex-ide-mcp--treesit-move-to-location (line column)
  "Move point to optional LINE and COLUMN."
  (unless (or (null line) (and (integerp line) (> line 0)))
    (user-error "Line must be a positive integer"))
  (unless (or (null column) (and (integerp column) (>= column 0)))
    (user-error "Column must be a non-negative integer"))
  (when line
    (goto-char (point-min))
    (forward-line (1- line)))
  (when (or line column)
    (move-to-column (or column 0))))

(defun codex-ide-mcp--treesit-buffer-for-args (args)
  "Return the buffer selected by tree-sitter ARGS."
  (let ((path (codex-ide-mcp--object-get args "path")))
    (if (and (stringp path) (not (string-empty-p path)))
        (codex-ide-mcp--buffer-for-path path)
      (current-buffer))))

(defun codex-ide-mcp--treesit-result-base (parser)
  "Return common tree-sitter result fields for PARSER."
  (delq nil
        (list (cons "buffer" (buffer-name))
              (when buffer-file-name
                (cons "path" (expand-file-name buffer-file-name)))
              (cons "point" (codex-ide-mcp--line-column))
              (cons "parser"
                    (codex-ide-mcp--treesit-parser-info parser)))))

(defun codex-ide-mcp--treesit-node-extras
    (node include-ancestors include-children max-children)
  "Return optional tree-sitter extras for NODE.
INCLUDE-ANCESTORS controls ancestor output, INCLUDE-CHILDREN controls
child output, and MAX-CHILDREN bounds child output."
  (delq nil
        (list
         (when include-ancestors
           (cons "ancestors"
                 (codex-ide-mcp--treesit-ancestors node)))
         (when include-children
           (cons "children"
                 (vconcat
                  (codex-ide-mcp--treesit-child-summaries
                   node max-children))))
         (when include-children
           (cons "childrenTruncated"
                 (codex-ide-mcp--json-false
                  (codex-ide-mcp--treesit-children-truncated-p
                   node max-children)))))))

(defun codex-ide-mcp--treesit-result
    (parser node whole-file include-ancestors include-children
            max-depth max-children)
  "Return JSON-ready tree-sitter result data for PARSER and NODE.
WHOLE-FILE chooses root tree output.  INCLUDE-ANCESTORS and
INCLUDE-CHILDREN control node extras.  MAX-DEPTH and MAX-CHILDREN bound
tree output."
  (append
   (codex-ide-mcp--treesit-result-base parser)
   (list
    (if whole-file
        (cons "tree"
              (codex-ide-mcp--treesit-tree node max-depth max-children))
      (cons "node" (codex-ide-mcp--treesit-node-summary node))))
   (unless whole-file
     (codex-ide-mcp--treesit-node-extras
      node include-ancestors include-children max-children))))

;;; Tree-sitter query

(defun codex-ide-mcp--tree-sitter-info (args)
  "Return structured tree-sitter information for ARGS."
  (let* ((line (codex-ide-mcp--object-get args "line"))
         (column (codex-ide-mcp--object-get args "column"))
         (whole-file (codex-ide-mcp--truthy-p
                      (codex-ide-mcp--object-get args "whole_file")))
         (include-ancestors (codex-ide-mcp--truthy-p
                             (codex-ide-mcp--object-get
                              args "include_ancestors")))
         (include-children (codex-ide-mcp--truthy-p
                            (codex-ide-mcp--object-get
                             args "include_children")))
         (max-depth (codex-ide-mcp--bounded-integer
                     (codex-ide-mcp--object-get args "max_depth")
                     codex-ide-mcp--treesit-default-max-depth 0))
         (max-children (codex-ide-mcp--bounded-integer
                        (codex-ide-mcp--object-get args "max_children")
                        codex-ide-mcp--treesit-default-max-children 0)))
    (with-current-buffer (codex-ide-mcp--treesit-buffer-for-args args)
      (save-excursion
        (save-restriction
          (widen)
          (codex-ide-mcp--treesit-move-to-location line column)
          (let* ((parser (codex-ide-mcp--treesit-parser))
                 (node (if whole-file
                           (treesit-parser-root-node parser)
                         (treesit-node-at (point) parser))))
            (unless node
              (user-error "No tree-sitter node at point"))
            (codex-ide-mcp--treesit-result
             parser node whole-file include-ancestors include-children
             max-depth max-children)))))))

(provide 'codex-ide-mcp-treesit)

;;; codex-ide-mcp-treesit.el ends here
