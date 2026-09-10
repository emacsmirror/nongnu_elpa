;;; hermes-preview-format.el --- Pure output preview discovery -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Recognize explicit generated-output receipts and closed HTML/SVG fences.
;; Paths are opaque backend tokens, never local filenames.  No I/O or title-
;; based version merging: the file API does not publish immutable revisions.

;;; Code:

(require 'hermes-transport)
(require 'json)

(defun hermes-preview--object (value)
  "Return structured VALUE, decoding a JSON string when necessary."
  (if (stringp value)
      (condition-case nil
          (json-parse-string value :object-type 'hash-table :array-type 'list)
        (error nil))
    value))

(defun hermes-preview--paths (event)
  "Return exact output paths explicitly published in completed tool EVENT.
Do not infer generation from input arguments, links, or filenames in prose."
  (let* ((result (hermes-preview--object (hermes-transport--get event 'result)))
         (name (hermes-transport--get event 'name))
         (keys (append '(output_file output_path generated_file generated_image
                                    generated_path artifact_path screenshot_path
                                    files_created files_written files_modified)
                       (pcase name
                         ("image_generate" '(image))
                         ("text_to_speech" '(file_path file_paths))
                         ((or "write_file" "patch") '(resolved_path)))))
         (success (hermes-transport--get result 'success)))
    (when (and (member (hermes-transport--get event 'status)
                       '("completed" completed done success))
               (not (hermes-transport--get result 'error))
               (not (memq success '(:false :json-false false))))
      (delete-dups
       (seq-filter
        (lambda (path)
          (and (stringp path) (not (string-empty-p path))
               (not (string-match-p "[\0\n\r]" path))))
        (apply #'append
               (mapcar (lambda (key)
                         (let ((value (hermes-transport--get result key)))
                           (cond ((stringp value) (list value))
                                 ((vectorp value) (append value nil))
                                 ((listp value) value))))
                       keys)))))))

(defun hermes-preview--fences (text)
  "Return closed HTML/SVG artifacts in TEXT as label/source plists.
Consume other fences too, so nested examples and unfinished code stay prose."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (artifacts)
      (while (re-search-forward "^ \\{0,3\\}\\(`\\{3,\\}\\|~\\{3,\\}\\)\\([^\n]*\\)$" nil t)
        (let* ((fence (match-string 1))
               (language (downcase (string-trim (match-string 2))))
               (start (progn (forward-line 1) (point)))
               (closing (format "^ \\{0,3\\}%c\\{%d,\\}[ \t]*$"
                                (aref fence 0) (length fence))))
          (if (not (re-search-forward closing nil t))
              (goto-char (point-max))
            (let ((end (match-beginning 0)))
              (when (member language '("html" "htm" "xhtml" "svg"))
                (push (list :label (upcase language) :kind
                            (if (equal language "svg") 'svg 'html)
                            :source (buffer-substring-no-properties start end))
                      artifacts)))
            (forward-line 1))))
      (nreverse artifacts))))

(defun hermes-preview-entry (entry)
  "Return preview descriptors for settled chat ENTRY without changing it."
  (pcase (plist-get entry :role)
    ('tool
     (mapcar (lambda (path) (list :label path :path path))
             (hermes-preview--paths
              (plist-get (plist-get entry :metadata) :event))))
    ('assistant
     (unless (eq (plist-get entry :status) 'streaming)
       (hermes-preview--fences (or (plist-get entry :content) ""))))))

(provide 'hermes-preview-format)
;;; hermes-preview-format.el ends here
