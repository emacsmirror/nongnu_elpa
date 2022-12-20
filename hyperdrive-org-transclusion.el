;;; hyperdrive-org.el --- Org-related functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  USHIN, Inc.

;; Author: Joseph Turner <joseph@ushin.org>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains functionality related to org-transclusion.

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-element)
(require 'org-transclusion)

(require 'hyperdrive-org)

(defvar h/mode)

;;;; Functions

(defun h/org-transclusion-add-file (link _plist)
  "Return callback function when hyperdrive transclusion is appropriate.
Otherwise, return nil.  Intended to be added to
`org-transclusion-add-functions', which see for descriptions of
arguments LINK and PLIST."
  (and (or (string= "hyper" (org-element-property :type link))
           (and h/mode
                (h/org--element-entry link)))
       (h/message "Asynchronously transcluding at point %d, line %d..."
                  (point) (org-current-line))
       #'h/org-transclusion-add-callback))

(add-hook 'org-transclusion-add-functions #'h/org-transclusion-add-file)

;; TODO: Consider excluding the modifications to a hyperdrive file buffer.
;;       Should only saved hyperdrive files be transcluded?

;; TODO: hyperdrive directories?  (skip loading metadata?)

;; TODO: When `org-transclusion-add-src-lines' is pushed onto
;; `org-transclusion-add-functions' after `h/org-transclusion-add-file', then an
;; error is signaled for hyperdrive transclusions with specified :lines (also
;; doesn't work with relative transclusion):
;; #+transclude: [[hyper://sw8dj5y9cs5nb8dzq1h9tbjt3b4u3sci6wfeckbsch9w3q7amipy/item2.org]]  :lines 1-10

(defun h/org-transclusion-add-callback (link plist copy)
  "Load hyperdrive file at LINK and call
`org-transclusion-add-callback' with PAYLOAD, LINK, PLIST, COPY."
  (pcase-let* ((target-mkr (point-marker))
               (entry (if (string= "hyper" (org-element-property :type link))
                          ;; Absolute link
                          (h/url-entry (org-element-property :raw-link link))
                        ;; Relative link
                        (h/org--element-entry link)))
               ((cl-struct h/entry path etc) entry)
               ((map target) etc))
    (h/open
      entry
      :messagep nil
      :then
      (lambda ()
        (when-let ((target-buf (marker-buffer target-mkr)))
          (let* ((payload-without-type
                  (org-with-wide-buffer
                   (org-transclusion-content-org-buffer-or-element
                    (and target
                         (progn
                           (org-link-search target)
                           t))
                    plist)))
                 (type (if (org-transclusion-org-file-p path)
                           ;; For org files, `type' must begin with "org"
                           "org-hyper"
                         "others-hyper"))
                 (payload (append `(:tc-type ,type) payload-without-type)))
            (with-current-buffer target-buf
              (org-with-wide-buffer
               (goto-char (marker-position target-mkr))
               (org-transclusion-add-callback payload link plist copy)))))))))

;;;; Footer

(provide 'hyperdrive-org-transclusion)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:
;;; hyperdrive-org-transclusion.el ends here
