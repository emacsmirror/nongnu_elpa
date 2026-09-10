;;; vm-org.el --- Org links to VM messages and folders  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024  Free Software Foundation, Inc.
;; Copyright (C) 2024-2026  The VM Developers

;; Author: Carsten Dominik <carsten at orgmode dot org>
;;	   Uday S Reddy <reddyuday at launchpad dot net>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of VM

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A `vm:' link in an Org file names a folder and a message in it, so
;; `C-c C-o' on it opens the folder and shows that message, and `C-c l' in a
;; VM folder makes such a link to the message being read.
;;
;; Put (require 'vm-org) in your init file.  Org itself carries no support
;; for VM: this is it.
;;
;; This was contrib/org-vm.el, which Org used to load and VM then carried,
;; and it had gone stale.  It registered the link type with
;; `org-add-link-type', obsolete since Org 9.0, and hooked link storing onto
;; `org-store-link-functions', which Org 9.3 removed: `add-hook' made a
;; variable nothing read, so storing a link did nothing at all and said so
;; nowhere.  Both halves go through `org-link-set-parameters' now
;; (emacs-vm/vm#812).

;;; Code:

(require 'org)
(require 'ol)
(require 'vm-macro)
(require 'vm-message)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function vm-preview-current-message "vm-page" ())
(declare-function vm-follow-summary-cursor "vm-motion" ())
(declare-function vm-isearch-narrow "vm-search" ())
(declare-function vm-isearch-update "vm-search" ())
(declare-function vm-select-folder-buffer "vm-macro" ())
(declare-function vm-su-message-id "vm-summary" (m))
(declare-function vm-su-subject "vm-summary" (m))
(declare-function vm-su-to-names "vm-summary" (m))
(declare-function vm-su-full-name "vm-summary" (m))
(declare-function vm-summarize "vm-summary" (&optional display raise))
(declare-function vm-folder-name "vm-folder" ())
(defvar vm-message-pointer)
(defvar vm-folder-directory)

(defun vm-org-shorten-folder (folder)
  "FOLDER as a link says it, shortened against `vm-folder-directory\\='.
A link made on one machine is then followed on another whose mail lives
somewhere else."
  (let ((folder (abbreviate-file-name folder)))
    (if (and vm-folder-directory
             (string-match (concat "\\`" (regexp-quote
                                        (abbreviate-file-name
                                         vm-folder-directory)))
                           folder))
        (replace-match "" t t folder)
      folder)))

(defun vm-org-folder-name (message)
  "The folder MESSAGE is in, as a link says it."
  (vm-org-shorten-folder
   (with-current-buffer (vm-buffer-of message)
     (or (vm-folder-name) (buffer-file-name)))))

(defun vm-org-store-link ()
  "Store an Org link to the message being read.
On the `:store\\=' property of the `vm\\=' link type, which is how Org asks
for one."
  (when (memq major-mode '(vm-summary-mode vm-presentation-mode))
    (when (eq major-mode 'vm-presentation-mode)
      (vm-summarize))
    (vm-follow-summary-cursor)
    (save-excursion
      (vm-select-folder-buffer)
      (let* ((message (vm-real-message-of (car vm-message-pointer)))
             (message-id (vm-su-message-id message))
             (folder (vm-org-folder-name message)))
        (org-link-store-props :type "vm"
                              :from (vm-su-full-name message)
                              :to (vm-su-to-names message)
                              :subject (vm-su-subject message)
                              :message-id message-id)
        (let ((link (concat "vm:" folder "#"
                            (org-unbracket-string "<" ">" message-id))))
          (org-link-add-props :link link
                              :description (org-link-email-description))
          link)))))

(defun vm-org-remote-folder (folder)
  "FOLDER as a remote file name, or nil if it does not name one.
The form is //user@host:file, which is what a link made on a folder reached
over ftp or ssh holds, and the answer is the Tramp name for it.

The user is matched without the at sign that follows it.  The group used to
take that in and the at sign was then written again, so a link naming a user
answered //me@@host, which Tramp cannot open."
  (when (string-match "\\`//\\(?:\\([a-zA-Z]+\\)@\\)?\\([^:]+\\):\\(.*\\)"
                      folder)
    (format "/%s@%s:%s"
            (or (match-string 1 folder) (user-login-name))
            (match-string 2 folder)
            (match-string 3 folder))))

(defun vm-org-goto-message (message-id)
  "Show the message MESSAGE-ID in the folder now current.
Signals when the folder does not hold it, rather than leaving the reader in
a folder wondering which message was meant."
  (require 'vm-search)
  (vm-select-folder-buffer)
  (widen)
  (let ((case-fold-search t))
    (goto-char (point-min))
    (unless (re-search-forward
             (concat "^message-id: *" (regexp-quote message-id)) nil t)
      (error (concat "This folder holds no message %s.  The link names one"
                     " the folder does not have, so either the message has"
                     " been moved or the link was made against another"
                     " folder")
             message-id))
    (vm-isearch-update)
    (vm-isearch-narrow)
    (vm-preview-current-message)
    (vm-summarize)))

(defun vm-org-follow-link (&optional folder message-id readonly)
  "Visit FOLDER and show the message MESSAGE-ID in it.
READONLY visits the folder read-only."
  (require 'vm)
  (let ((folder (or (and folder (vm-org-remote-folder folder)) folder)))
    (when folder
      (funcall (cdr (assq 'vm org-link-frame-setup)) folder readonly)
      (sit-for 0.1)
      (when message-id
        (vm-org-goto-message (org-link-add-angle-brackets message-id))))))

(defun vm-org-open (path)
  "Follow the `vm:\\=' link PATH, which is a folder and optionally a message.
On the `:follow\\=' property of the link type.  A prefix argument visits the
folder read-only."
  (unless (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path)
    (error (concat "%S is not a vm: link.  One names a folder, and may name a"
                   " message in it after a #, as in vm:inbox#1234@example.com")
           path))
  (vm-org-follow-link (match-string 1 path) (match-string 3 path)
                      current-prefix-arg))

(org-link-set-parameters "vm"
                         :follow #'vm-org-open
                         :store #'vm-org-store-link)

(provide 'vm-org)

;;; vm-org.el ends here
