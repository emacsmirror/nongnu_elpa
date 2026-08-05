;;; jabber-chat-commands.el --- Chat buffer feature commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Assemble commands from chat feature modules after their providers load.

;;; Code:

(require 'keymap-popup)
(require 'jabber-blocking)
(require 'jabber-chat)
(require 'jabber-chatbuffer)
(require 'jabber-httpupload)
(require 'jabber-info)
(require 'jabber-mam)
(require 'jabber-message-correct)
(require 'jabber-message-reply)
(require 'jabber-message-thread)
(require 'jabber-moderation)
(require 'jabber-muc)
(require 'jabber-muc-menu)
(require 'jabber-presence)
(require 'jabber-reactions)

(autoload 'jabber-omemo--prefetch-sessions "jabber-omemo")
(autoload 'jabber-omemo--prefetch-muc-sessions "jabber-omemo")
(autoload 'jabber-omemo--muc-participant-jids "jabber-omemo")
(autoload 'jabber-omemo-fingerprints "jabber-omemo-trust")

(defvar jabber-chat-encryption-menu-map)
(defvar jabber-chat-operations-menu-map)

(defun jabber-chat-attach-file (filepath)
  "Upload FILEPATH and insert the URL into the composition area.
The file is uploaded via HTTP Upload.  Once the upload finishes,
the GET URL is inserted at point so you can preview and edit
before sending with RET."
  (interactive "fFile to upload: ")
  (unless jabber-buffer-connection
    (error "No active connection in this buffer"))
  (let ((buffer (current-buffer)))
    (jabber-httpupload--upload
     jabber-buffer-connection filepath
     (lambda (get-url)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert get-url)
           (setq jabber-httpupload--pending-url get-url)
           (message "Uploaded: %s (send with RET)" get-url)))))))

(defun jabber-chat-encryption-set-omemo ()
  "Set encryption to OMEMO for this chat buffer."
  (interactive)
  (require 'jabber-omemo)
  (unless (eq (bound-and-true-p jabber-omemo--available) t)
    (user-error "OMEMO encryption requires the jabber-omemo-core native module"))
  (setq jabber-chat-encryption 'omemo)
  (jabber-chat-encryption--save 'omemo)
  (jabber-chat-encryption--update-header)
  (force-mode-line-update)
  (when jabber-buffer-connection
    (cond
     ((bound-and-true-p jabber-chatting-with)
      (jabber-omemo--prefetch-sessions
       jabber-buffer-connection jabber-chatting-with))
     ((bound-and-true-p jabber-group)
      (jabber-omemo--prefetch-muc-sessions
       jabber-buffer-connection jabber-group))))
  (when (and (bound-and-true-p jabber-group)
             (null (jabber-omemo--muc-participant-jids
                    jabber-group
                    (cdr (assoc jabber-group jabber-muc-participants)))))
    (message "OMEMO: no participant JIDs visible; room may be anonymous")))

(defun jabber-chat-encryption-set-openpgp ()
  "Set encryption to OpenPGP for this chat buffer."
  (interactive)
  (require 'jabber-openpgp)
  (setq jabber-chat-encryption 'openpgp)
  (jabber-chat-encryption--save 'openpgp)
  (jabber-chat-encryption--update-header)
  (force-mode-line-update))

(defun jabber-chat-encryption-set-openpgp-legacy ()
  "Set encryption to legacy PGP (XEP-0027) for this chat buffer."
  (interactive)
  (require 'jabber-openpgp-legacy)
  (setq jabber-chat-encryption 'openpgp-legacy)
  (jabber-chat-encryption--save 'openpgp-legacy)
  (jabber-chat-encryption--update-header)
  (force-mode-line-update))

(defun jabber-chat-encryption-set-plaintext ()
  "Set encryption to plaintext for this chat buffer."
  (interactive)
  (setq jabber-chat-encryption 'plaintext)
  (jabber-chat-encryption--save 'plaintext)
  (jabber-chat-encryption--update-header)
  (force-mode-line-update))

(keymap-popup-define jabber-chat-encryption-menu-map
  "Select encryption for this chat buffer."
  :description (lambda ()
                 (format "Encryption (current: %s)"
                         (propertize
                          (symbol-name jabber-chat-encryption)
                          'face (if (eq jabber-chat-encryption 'plaintext)
                                    'shadow
                                  'success))))
  "o" ("OMEMO" jabber-chat-encryption-set-omemo)
  "g" ("OpenPGP" jabber-chat-encryption-set-openpgp)
  "l" ("PGP (legacy)" jabber-chat-encryption-set-openpgp-legacy)
  "p" ("Plaintext" jabber-chat-encryption-set-plaintext))

(defun jabber-chat-encryption-menu ()
  "Select encryption for this chat buffer."
  (interactive)
  (keymap-popup jabber-chat-encryption-menu-map))

(defun jabber-chat-show-fingerprints ()
  "Display OMEMO fingerprints for the current chat peer."
  (interactive)
  (require 'jabber-omemo-trust)
  (jabber-omemo-fingerprints))

(defun jabber-chat-get-info ()
  "Show version, disco info and ping for the current chat peer."
  (interactive)
  (unless (bound-and-true-p jabber-chatting-with)
    (user-error "Not in a chat buffer"))
  (jabber-get-info jabber-buffer-connection jabber-chatting-with))

(defun jabber-chat-add-contact ()
  "Add the current chat peer to the roster."
  (interactive)
  (unless (bound-and-true-p jabber-chatting-with)
    (user-error "Not in a chat buffer"))
  (let* ((jid (jabber-jid-user jabber-chatting-with))
         (symbol (jabber-jid-symbol jid)))
    (jabber-roster-change
     jabber-buffer-connection symbol
     (read-string (format "Name for %s: " jid))
     nil)))

(defun jabber-chat-remove-contact ()
  "Remove the current chat peer from the roster."
  (interactive)
  (unless (bound-and-true-p jabber-chatting-with)
    (user-error "Not in a chat buffer"))
  (let ((jid (jabber-jid-user jabber-chatting-with)))
    (when (yes-or-no-p (format "Remove %s from roster? " jid))
      (jabber-roster-delete jabber-buffer-connection jid))))

(defun jabber-chat-muc-actions-menu ()
  "Show MUC actions for the current chat buffer."
  (interactive)
  (keymap-popup jabber-muc-menu-map))

(keymap-popup-define jabber-chat-operations-menu-map
  :description (lambda ()
                 (let ((peer (or (bound-and-true-p jabber-group)
                                 (bound-and-true-p jabber-chatting-with))))
                   (if peer
                       (format "Operations for %s"
                               (propertize peer 'face
                                           'font-lock-constant-face))
                     "Chat operations")))
  :group "Encryption"
  "e" ("Encryption" jabber-chat-encryption-menu)
  "f" ("Fingerprints" jabber-chat-show-fingerprints)
  :group "Files"
  "a" ("Attach file" jabber-chat-attach-file)
  :group "Contact"
  "I" ("Get info" jabber-chat-get-info
       :if (lambda () (bound-and-true-p jabber-chatting-with)))
  "A" ("Add contact" jabber-chat-add-contact
       :if (lambda () (bound-and-true-p jabber-chatting-with)))
  "D" ("Remove contact" jabber-chat-remove-contact
       :if (lambda () (bound-and-true-p jabber-chatting-with)))
  "B" ("Block/unblock user" jabber-blocking-toggle-chat-peer
       :if (lambda () (bound-and-true-p jabber-chatting-with)))
  :group "Messages"
  "E" ("Edit last message" jabber-correct-last-message)
  "r" ("Reply to message" jabber-chat-reply)
  "t" ("Reply in thread" jabber-message-thread-open
       :if #'jabber-message-thread-available-p)
  "T" ("Send draft as new thread" jabber-message-thread-start
       :if #'jabber-message-thread-available-p)
  "l" ("Browse threads..." jabber-message-thread-browse
       :if #'jabber-message-thread-available-p)
  :group "MUC"
  "m" ("MUC Actions" jabber-chat-muc-actions-menu
       :if (lambda () (bound-and-true-p jabber-group)))
  "M" ("Retract message at point" jabber-moderation-retract
       :if (lambda () (bound-and-true-p jabber-group)))
  "X" ("Retract all by occupant" jabber-moderation-retract-by-occupant
       :if (lambda () (bound-and-true-p jabber-group)))
  :group "Buffer"
  "L" ("Set thread title..." jabber-message-thread-set-title
       :if (lambda () (bound-and-true-p jabber-message-thread-id)))
  "n" ((lambda ()
         (format "Message count: %s"
                 (propertize
                  (number-to-string (jabber-chat-buffer-msg-count))
                  'face 'keymap-popup-value)))
       jabber-chat-set-msg-count :stay-open)
  "R" ("Refresh" jabber-chat-buffer-refresh)
  "S" ("Sync & refresh" jabber-mam-sync-buffer))

(defun jabber-chat-set-msg-count (count)
  "Set the message count for the current chat buffer to COUNT."
  (interactive
   (list (read-number "Message count: " (jabber-chat-buffer-msg-count))))
  (setq jabber-chat-buffer-msg-count (and (> count 0) count))
  (message "Buffer message count: %d" (jabber-chat-buffer-msg-count))
  (keymap-popup jabber-chat-operations-menu-map))

(defun jabber-chat-operations-menu ()
  "Chat buffer operations."
  (interactive)
  (keymap-popup jabber-chat-operations-menu-map))

(defconst jabber-chat-command-bindings
  '(("RET" . jabber-chat-goto-reply-target-or-send)
    ("C-c C-a" . jabber-chat-attach-file)
    ("C-c C-o" . jabber-chat-operations-menu)
    ("C-c C-e" . jabber-chat-encryption-menu)
    ("C-c C-m" . jabber-muc-menu)
    ("C-c C-r" . jabber-chat-reply)
    ("C-c C-k" . jabber-chat-cancel-reply)
    ("+" . jabber-chat-image-enlarge-or-self-insert)
    ("=" . jabber-chat-image-enlarge-or-self-insert)
    ("-" . jabber-chat-image-shrink-or-self-insert)
    ("0" . jabber-chat-image-reset-size-or-self-insert)
    ("!" . jabber-reactions-react-at-point-or-insert))
  "Feature command bindings added to `jabber-chat-mode-map'.")

(dolist (binding jabber-chat-command-bindings)
  (keymap-set jabber-chat-mode-map (car binding) (cdr binding)))

(keymap-set jabber-chat-mode-map "C-c C-t" #'jabber-message-thread-open)

(provide 'jabber-chat-commands)

;;; jabber-chat-commands.el ends here
