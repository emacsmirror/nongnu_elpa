;;; eldoc-mouse-nov.el --- Preview epub link for mouse hover -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Huang Feiyu

;; Author: Huang Feiyu <sibadake1@163.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (eldoc-mouse "3.0.2") (nov "0.5.0"))
;; Keywords: tools, epub, convenience, mouse, hover
;; URL: https://github.com/huangfeiyu/eldoc-mouse-nov
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of eldoc-mouse-nov.
;;
;; eldoc-mouse-nov is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3
;; of the License, or (at your option) any later version.
;;
;; eldoc-mouse-nov is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with eldoc-mouse-nov; if not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;; This package enhances `nov-mode' by displaying the content of the link in a
;; popup when the mouse hovers over a link.

;; To use, ensure `eldoc-mouse' is installed, then add the following:
;;

;;   (use-package eldoc-mouse :ensure t
;;     ;; replace <f1> <f1> to a key you like, "C-h ." maybe.
;;     :bind (:map eldoc-mouse-mode-map
;;            ("<f1> <f1>" . eldoc-mouse-pop-doc-at-cursor)) ;; optional
;;     ;; enable mouse hover
;;     :hook (eglot-managed-mode emacs-lisp-mode nov-mode))
;; 
;;   (use-package eldoc-mouse-nov
;;     :ensure t
;;     :after (eldoc-mouse)
;;     :hook (nov-mode))

;; to your Emacs configuration.

;;; Code:

(require 'eldoc-mouse)
(require 'nov)


;;;###autoload
(define-minor-mode eldoc-mouse-nov-mode
  "Toggle the `eldoc-mouse-nov-mode'."
  :lighter " eldoc-mouse-nov"
  (if eldoc-mouse-nov-mode
      (eldoc-mouse-nov-enable)
    (eldoc-mouse-nov-disable)))

(defun eldoc-mouse-nov-enable ()
  "Enable eldoc-mouse-nov in buffers."
  (setq eldoc-mouse-bounds-of-thing-at-point-function
        (lambda ()
          (let* ((pos (point))
                 (prop 'shr-url))
            (when (get-text-property pos prop)
              (cons (previous-single-property-change (1+ pos) prop nil (point-min))
                    (next-single-property-change pos prop nil (point-max)))))))
  (add-hook 'eldoc-mouse-eldoc-documentation-functions #'eldoc-mouse-nov--eldoc-documentation-function nil t))

(defun eldoc-mouse-nov-disable ()
  "Disable eldoc-mouse-nov in buffers."
  (remove-hook 'eldoc-mouse-eldoc-documentation-functions #'eldoc-mouse-nov--eldoc-documentation-function t))

(defun eldoc-mouse-nov--eldoc-documentation-function (_cb)
  "The `eldoc-documentation-functions' implementation for nov mode."
  (let ((content (eldoc-mouse-nov--get-link-content-at-point)))
    (when (and (stringp content)
               (not (string-blank-p content)))
      (string-trim content))))

(defun eldoc-mouse-nov--html-to-shr-text (html)
  "Convert HTML to simple text."
  (when (and (stringp html)
             (not (string-blank-p html)))
    (with-temp-buffer
      (insert html)
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (erase-buffer)
        (shr-insert-document dom)
        (buffer-string)))))

(defun eldoc-mouse-nov--get-link-content-at-point ()
  "Get the content of the link at cusor point."
  (let ((url (get-text-property (point) 'shr-url)))
    (when url
      (if (nov-external-url-p url)
          (with-current-buffer (url-retrieve-synchronously url t t 5)
            (unwind-protect
                (progn
                  (goto-char (point-min))
                  (re-search-forward "^$" nil t) ; Find first empty line after headers
                  (forward-line 1)
                  (let ((data (buffer-substring-no-properties (point) (point-max))))
                    (eldoc-mouse-nov--html-to-shr-text data)))
              (kill-buffer)))
        (eldoc-mouse-nov--get-link-content (car (nov-url-filename-and-target url)))))
    ))

(defun eldoc-mouse-nov--get-link-content (filename)
  "Retrieve text content from FILENAME."
  (let (index)
    (when (not (zerop (length filename)))
      (let* ((current-path (cdr (aref nov-documents nov-documents-index)))
             (directory (file-name-directory current-path))
             (path (file-truename (nov-make-path directory filename)))
             (match (nov-find-document
                     (lambda (doc) (equal path (file-truename (cdr doc)))))))
        (when (not match)
          (error "Couldn't locate document"))
        (setq index match)))
    (let* ((document (aref nov-documents index))
           (file-path (cdr document)))
      (with-temp-buffer
        (insert-file-contents file-path)
        (eldoc-mouse-nov--html-to-shr-text (buffer-string))))))

(provide 'eldoc-mouse-nov)

;;; eldoc-mouse-nov.el ends here
