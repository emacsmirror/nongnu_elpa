;;; markdown-ts-appear-image.el --- Persistent image previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael

;; Author: Thysrael <thysrael@163.com>
;; Assisted-by: OpenCode:gpt-6-astra
;; Maintainer: Thysrael <thysrael@163.com>
;; Keywords: text, convenience
;; URL: https://github.com/Thysrael/markdown-ts-appear

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Keep image previews below their source links, reusing unchanged images
;; and maintaining a separate display for each window.

;;; Code:

(require 'markdown-ts-mode)
(require 'image)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(declare-function clear-image-cache "image.c")
(declare-function markdown-ts-appear--active-p "markdown-ts-appear")
(declare-function markdown-ts-appear--literal-block-at "markdown-ts-appear")
(declare-function markdown-ts-appear--node-ancestor "markdown-ts-appear")
(declare-function markdown-ts-appear--add-outline-hook "markdown-ts-appear")
(declare-function markdown-ts-appear--remove-outline-hook "markdown-ts-appear")

(defvar-local markdown-ts-appear-image--sources nil
  "Source overlays holding cached images and their window-specific previews.")
(defvar-local markdown-ts-appear-image--state nil
  "Last text modification tick and native image options checked.")
(defvar-local markdown-ts-appear-image--resize-timer nil
  "Pending image resize timer.")

(defun markdown-ts-appear-image--delete (source)
  "Delete SOURCE and its preview overlays."
  (mapc #'delete-overlay (overlay-get source 'markdown-ts-appear-image--views))
  (delete-overlay source)
  (setq markdown-ts-appear-image--sources
        (delq source markdown-ts-appear-image--sources)))

(defun markdown-ts-appear-image--load (location remote)
  "Load an Emacs image from LOCATION; REMOTE means an HTTP URL."
  (condition-case err
      (if remote
          (when (eq markdown-ts-display-remote-inline-images 'download)
            (require 'url-handlers)
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (url-insert-file-contents location)
              (create-image (buffer-string) nil t :scale 1)))
        (when (and (not (file-remote-p location))
                   (file-exists-p location)
                   (image-supported-file-p location))
          (create-image location nil nil :scale 1)))
    (error
     (message "Markdown image preview failed: %s" (error-message-string err))
     nil)))

(defun markdown-ts-appear-image--window-widths ()
  "Return an alist of windows and preview widths for the current buffer."
  (mapcar (lambda (window)
            (cons window (if (eq markdown-ts-image-max-width 'window)
                             (window-body-width window t)
                           markdown-ts-image-max-width)))
          (or (get-buffer-window-list (current-buffer) nil t) '(nil))))

(defun markdown-ts-appear-image--sync-views (source &optional widths)
  "Synchronize SOURCE's cached image with its buffer's windows.
WIDTHS is an optional alist of windows and preview widths, shared by a batch."
  (let ((image (overlay-get source 'markdown-ts-appear-image--image))
        (alone (overlay-get source 'markdown-ts-appear-image--alone))
        (old (overlay-get source 'markdown-ts-appear-image--views))
        views)
    (when (and (overlay-buffer source) image markdown-ts-inline-images
               (display-images-p)
               (not (markdown-ts--outline-invisible-p (overlay-start source))))
      (dolist (entry (or widths (markdown-ts-appear-image--window-widths)))
        (let* ((window (car entry))
               (width (cdr entry))
               (end (overlay-end source))
               (view (seq-find (lambda (overlay)
                                 (and (overlay-buffer overlay)
                                      (eq window (overlay-get overlay 'window))))
                               old))
               (key (and view (overlay-get view 'markdown-ts-appear-image--view-key))))
          (unless view
            (setq view (make-overlay (1- end) end nil t nil))
            (overlay-put view 'window window)
            (overlay-put view 'markdown-ts-image t)
            (overlay-put view 'markdown-ts-appear-image--owner source)
            (overlay-put view 'evaporate t))
          (unless (and (= (overlay-start view) (1- end)) (= (overlay-end view) end))
            (move-overlay view (1- end) end))
          (unless (and (eq image (car key)) (equal (cdr key) (list width alone)))
            (let* ((spec (cons 'image (plist-put (copy-sequence (cdr image))
                                               :max-width width)))
                   (text (propertize " " 'display spec)))
              (overlay-put view 'after-string (if alone (concat "\n" text) text)))
            (overlay-put view 'markdown-ts-appear-image--view-key (list image width alone)))
          (push view views))))
    (dolist (view old)
      (unless (memq view views) (delete-overlay view)))
    (setq views (nreverse views))
    (unless (equal views old)
      (overlay-put source 'markdown-ts-appear-image--views views))))

(defun markdown-ts-appear-image--update-source (source node)
  "Update SOURCE from image NODE, reusing unchanged image files."
  (overlay-put source 'markdown-ts-appear-image--alone
               (markdown-ts--image-alone-on-line-p node))
  (let ((destination (treesit-search-subtree node "\\`link_destination\\'")))
    (unless destination
      (overlay-put source 'markdown-ts-appear-image--image nil)
      (overlay-put source 'markdown-ts-appear-image--file-key nil))
    (when (and destination markdown-ts-inline-images (display-images-p)
               (not (markdown-ts--outline-invisible-p (overlay-start source))))
      (let* ((url (treesit-node-text destination t))
             (url (if (and (string-prefix-p "<" url) (string-suffix-p ">" url))
                      (substring url 1 -1) url))
             (remote (let ((case-fold-search t)) (string-match-p "\\`https?://" url)))
             ;; Unescape Markdown before percent-decoding, so %25 is decoded once.
             (location (if remote url
                         (expand-file-name
                          (decode-coding-string
                           (url-unhex-string
                            (replace-regexp-in-string "\\\\\\([[:punct:]]\\)" "\\1" url))
                           'utf-8))))
             (attributes (unless (or remote (file-remote-p location))
                           (file-attributes location)))
             (key (list location
                        (if remote markdown-ts-display-remote-inline-images
                          (and attributes
                               (list (file-attribute-modification-time attributes)
                                     (file-attribute-size attributes))))))
             (old (overlay-get source 'markdown-ts-appear-image--file-key)))
        (unless (equal key old)
          (when (and (not remote) (equal location (car old)))
            (clear-image-cache location))
          (overlay-put source 'markdown-ts-appear-image--file-key key)
          (overlay-put source 'markdown-ts-appear-image--image nil)
          (let ((image (markdown-ts-appear-image--load location remote)))
            (when (and (overlay-buffer source)
                       (equal key (overlay-get source 'markdown-ts-appear-image--file-key)))
              (overlay-put source 'markdown-ts-appear-image--image image))))))))

(defun markdown-ts-appear-image--fontify (function node override start end &rest rest)
  "Call image fontifier FUNCTION for NODE, or reuse its managed preview.
OVERRIDE, START, END and REST are passed to the native fontifier when inactive."
  (if (not (markdown-ts-appear--active-p))
      (apply function node override start end rest)
    (when (and markdown-ts-inline-images (display-images-p))
      (let* ((beg (treesit-node-start node))
             (finish (treesit-node-end node))
             (source (seq-find
                      (lambda (overlay)
                        (and (overlay-get overlay 'markdown-ts-appear-image--source)
                             (= (overlay-start overlay) beg)))
                      (overlays-at beg))))
        (unless source
          ;; Replace a preview created before appear mode was enabled.
          (dolist (overlay (overlays-at (1- finish)))
            (when (and (overlay-get overlay 'markdown-ts-image)
                       (= (overlay-end overlay) finish)
                       (not (overlay-get overlay 'markdown-ts-appear-image--owner)))
              (delete-overlay overlay)))
          (setq source (make-overlay beg finish nil t nil))
          (overlay-put source 'markdown-ts-appear-image--source t)
          (overlay-put source 'evaporate t)
          (push source markdown-ts-appear-image--sources))
        (unless (and (= (overlay-start source) beg) (= (overlay-end source) finish))
          (move-overlay source beg finish))
        (markdown-ts-appear-image--update-source source node)
        (markdown-ts-appear-image--sync-views source)))))

(defun markdown-ts-appear-image--refresh (&rest _)
  "Reconcile image sources after edits or native image option changes."
  (when (and (markdown-ts-appear--active-p) markdown-ts-appear-image--sources)
    (let ((state (list (buffer-chars-modified-tick) markdown-ts-inline-images
                       markdown-ts-image-max-width markdown-ts-display-remote-inline-images)))
      (unless (equal state markdown-ts-appear-image--state)
        (setq markdown-ts-appear-image--state state)
        (without-restriction
          (treesit-update-ranges (point-min) (point-max))
          (dolist (source markdown-ts-appear-image--sources)
            (let* ((beg (overlay-start source))
                   (node (and markdown-ts-inline-images beg
                              (not (markdown-ts-appear--literal-block-at beg))
                              (markdown-ts-appear--node-ancestor
                               (treesit-node-at beg 'markdown-inline) "image"))))
              (if (and node (= beg (treesit-node-start node))
                       (= (overlay-end source) (treesit-node-end node)))
                  (markdown-ts-appear-image--update-source source node)
                (markdown-ts-appear-image--delete source))))
          (markdown-ts-appear-image--refresh-views))))))

(defun markdown-ts-appear-image--refresh-views (&optional buffer)
  "Resize cached image views in BUFFER after window changes."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (when markdown-ts-appear-image--resize-timer
        (cancel-timer markdown-ts-appear-image--resize-timer)
        (setq markdown-ts-appear-image--resize-timer nil))
      (when (and (markdown-ts-appear--active-p) markdown-ts-appear-image--sources)
        (let ((widths (markdown-ts-appear-image--window-widths)))
          (dolist (source markdown-ts-appear-image--sources)
            (markdown-ts-appear-image--sync-views source widths)))))))

(defun markdown-ts-appear-image--outline-change ()
  "Reconcile image sources after outline folding or unfolding."
  (setq markdown-ts-appear-image--state nil)
  (markdown-ts-appear-image--refresh))

(defun markdown-ts-appear-image--schedule-resize ()
  "Coalesce window changes before rescaling image previews."
  (when markdown-ts-appear-image--sources
    (when markdown-ts-appear-image--resize-timer
      (cancel-timer markdown-ts-appear-image--resize-timer))
    (setq markdown-ts-appear-image--resize-timer
          (run-at-time 0.2 nil #'markdown-ts-appear-image--refresh-views
                       (current-buffer)))))

(defun markdown-ts-appear-image--setup ()
  "Install image reconciliation and window hooks in the current buffer."
  (font-lock-set-defaults)
  (add-function :after (local 'font-lock-fontify-region-function)
                #'markdown-ts-appear-image--refresh)
  (add-hook 'post-command-hook #'markdown-ts-appear-image--refresh 80 t)
  (add-hook 'window-configuration-change-hook #'markdown-ts-appear-image--schedule-resize nil t)
  (markdown-ts-appear--add-outline-hook #'markdown-ts-appear-image--outline-change 90))

(defun markdown-ts-appear-image--detach ()
  "Detach image state without touching previews inherited from a base buffer."
  (remove-function (local 'font-lock-fontify-region-function)
                   #'markdown-ts-appear-image--refresh)
  (remove-hook 'post-command-hook #'markdown-ts-appear-image--refresh t)
  (remove-hook 'window-configuration-change-hook #'markdown-ts-appear-image--schedule-resize t)
  (markdown-ts-appear--remove-outline-hook #'markdown-ts-appear-image--outline-change)
  (setq markdown-ts-appear-image--sources nil
        markdown-ts-appear-image--state nil
        markdown-ts-appear-image--resize-timer nil))

(defun markdown-ts-appear-image--teardown ()
  "Remove managed image previews, hooks and timers."
  (when markdown-ts-appear-image--resize-timer
    (cancel-timer markdown-ts-appear-image--resize-timer))
  (mapc #'markdown-ts-appear-image--delete markdown-ts-appear-image--sources)
  (markdown-ts-appear-image--detach))

(provide 'markdown-ts-appear-image)
;;; markdown-ts-appear-image.el ends here
