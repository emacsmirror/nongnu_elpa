;;; jabber-omemo-trust.el --- OMEMO trust management UI  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is part of emacs-jabber.

;; emacs-jabber is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; emacs-jabber is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with emacs-jabber.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tabulated-list-mode interface for managing OMEMO device trust.
;; Shows all known keys for a peer with interactive trust toggling
;; and key deletion.

;;; Code:

(require 'cl-lib)
(require 'jabber-omemo-store)
(require 'transient)

(declare-function jabber-omemo--format-fingerprint "jabber-omemo")
(declare-function jabber-omemo--trust-label "jabber-omemo")
(declare-function jabber-omemo--get-device-id "jabber-omemo")
(declare-function jabber-omemo--get-store "jabber-omemo")
(declare-function jabber-omemo-get-bundle "jabber-omemo")
(declare-function jabber-connection-bare-jid "jabber-util")
(declare-function jabber-jid-user "jabber-util")
(declare-function jabber-read-account "jabber-util")

(defvar jabber-chatting-with)
(defvar jabber-buffer-connection)

(defun jabber-omemo-trust--strip-key-type (identity-key)
  "Strip the 0x05 Curve25519 type prefix from IDENTITY-KEY.
Returns the key without the first byte, or as-is if shorter than 2 bytes."
  (if (and (> (length identity-key) 1)
           (= (aref identity-key 0) #x05))
      (substring identity-key 1)
    identity-key))

(defvar-local jabber-omemo-trust--account nil
  "Bare JID of the account for this trust buffer.")

(defvar-local jabber-omemo-trust--jc nil
  "Jabber connection for this trust buffer.")

(defvar-local jabber-omemo-trust--peer nil
  "Bare JID of the peer for this trust buffer.")

;;; Mode

(defvar-keymap jabber-omemo-trust-mode-map
  :doc "Keymap for `jabber-omemo-trust-mode'."
  "t" #'jabber-omemo-trust-set-verified
  "u" #'jabber-omemo-trust-set-untrusted
  "d" #'jabber-omemo-trust-delete
  "h" #'jabber-omemo-trust-menu
  "?" #'jabber-omemo-trust-menu)

(defun jabber-omemo--list-format ()
  (let ((list-format `[("Device ID" ,(/ (window-width) 10))
		       ("Trust" ,(/ (window-width) 10))
		       ("Fingerprint" ,(/ (window-width) 5))
		       ("First Seen" ,(/ (window-width) 10))]))
    list-format))

(define-derived-mode jabber-omemo-trust-mode tabulated-list-mode "OMEMO-Trust"
  "Major mode for managing OMEMO device trust.
\\<jabber-omemo-trust-mode-map>
\\[jabber-omemo-trust-set-verified] Set trust to verified.
\\[jabber-omemo-trust-set-untrusted] Set trust to untrusted.
\\[jabber-omemo-trust-delete] Delete key and session."
  (setq tabulated-list-format (jabber-omemo--list-format))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (setq tabulated-list-entries #'jabber-omemo-trust--entries)
  (add-hook 'tabulated-list-revert-hook #'jabber-omemo-trust--revert nil t))

(defun jabber-omemo-trust--revert ()
  "Recalculate column widths before revert."
  (setq tabulated-list-format (jabber-omemo--list-format))
  (tabulated-list-init-header))

;;; Entries

(defun jabber-omemo-trust--entries ()
  "Build tabulated-list entries from trust records."
  (let ((records (jabber-omemo-store-all-trust
                  jabber-omemo-trust--account
                  jabber-omemo-trust--peer)))
    (mapcar (lambda (rec)
              (let ((did (plist-get rec :device-id))
                    (ik (plist-get rec :identity-key))
                    (trust (plist-get rec :trust))
                    (first-seen (plist-get rec :first-seen)))
                (list did
                      (vector (number-to-string did)
                              (jabber-omemo--trust-label trust)
                              (jabber-omemo--format-fingerprint
                               (jabber-omemo-trust--strip-key-type ik))
                              (if first-seen
                                  (format-time-string "%Y-%m-%d %H:%M"
                                                      first-seen)
                                "")))))
            records)))

;;; Entry point

;;;###autoload
(defun jabber-omemo-show-trust (jc jid)
  "Display OMEMO trust management for JID via connection JC."
  (interactive
   (let* ((jc (if (and (bound-and-true-p jabber-buffer-connection)
                       (bound-and-true-p jabber-chatting-with))
                  jabber-buffer-connection
                (jabber-read-account)))
          (jid (if (bound-and-true-p jabber-chatting-with)
                   (jabber-jid-user jabber-chatting-with)
                 (read-string "JID: "))))
     (list jc jid)))
  (let* ((account (jabber-connection-bare-jid jc))
         (peer (jabber-jid-user jid))
         (buf-name (format "*OMEMO trust: %s*" peer)))
    (with-current-buffer (get-buffer-create buf-name)
      (jabber-omemo-trust-mode)
      (setq jabber-omemo-trust--jc jc)
      (setq jabber-omemo-trust--account account)
      (setq jabber-omemo-trust--peer peer)
      (tabulated-list-print t)
      (switch-to-buffer (current-buffer)))))

;;; Actions

(defun jabber-omemo-trust--device-at-point ()
  "Return the device ID at point as an integer, or signal an error."
  (or (tabulated-list-get-id)
      (user-error "No device at point")))

(defun jabber-omemo-trust-set-verified ()
  "Mark the device at point as verified."
  (interactive)
  (let ((did (jabber-omemo-trust--device-at-point)))
    (jabber-omemo-store-set-trust jabber-omemo-trust--account
                                  jabber-omemo-trust--peer did 2)
    (tabulated-list-print t)
    (message "Device %d marked as verified" did)))

(defun jabber-omemo-trust-set-untrusted ()
  "Mark the device at point as untrusted."
  (interactive)
  (let ((did (jabber-omemo-trust--device-at-point)))
    (jabber-omemo-store-set-trust jabber-omemo-trust--account
                                  jabber-omemo-trust--peer did -1)
    (tabulated-list-print t)
    (message "Device %d marked as untrusted" did)))

(defun jabber-omemo-trust-delete ()
  "Delete the device key and session at point."
  (interactive)
  (let ((did (jabber-omemo-trust--device-at-point)))
    (when (y-or-n-p (format "Delete key and session for device %d? " did))
      (jabber-omemo-store-delete-trust jabber-omemo-trust--account
                                       jabber-omemo-trust--peer did)
      (jabber-omemo-store-delete-session jabber-omemo-trust--account
                                         jabber-omemo-trust--peer did)
      (tabulated-list-print t)
      (message "Device %d deleted" did))))

;;; Transient

(defun jabber-omemo-trust--menu-description ()
  "Return description string for the transient menu."
  (let ((lines (list (format "Peer: %s  Account: %s"
			     (propertize jabber-omemo-trust--peer 'face
					 'jabber-chat-nick-foreign-encrypted)
                             (propertize jabber-omemo-trust--account 'face
					 'jabber-chat-nick-encrypted)))))
    (string-join (nreverse lines) "\n")))

(transient-define-prefix jabber-omemo-trust-menu ()
  "OMEMO trust commands."
  [:description jabber-omemo-trust--menu-description
   [("t" "Verify" jabber-omemo-trust-set-verified)
    ("u" "Untrust" jabber-omemo-trust-set-untrusted)
    ("d" "Delete" jabber-omemo-trust-delete)
    ("g" "Refresh" revert-buffer)]])

(provide 'jabber-omemo-trust)
;;; jabber-omemo-trust.el ends here
