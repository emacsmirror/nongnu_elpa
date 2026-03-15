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
(declare-function jabber-omemo--fetch-device-list "jabber-omemo"
                  (jc jid callback))
(declare-function jabber-omemo--fetch-bundle "jabber-omemo"
                  (jc jid device-id callback))
(declare-function jabber-omemo--remove-device "jabber-omemo"
                  (jc device-id &optional callback))
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

(defvar-local jabber-omemo-trust--fetched nil
  "List of entries fetched from server bundles.")

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

(defun jabber-omemo-trust--format-entry (rec)
  "Format a single trust record REC as a tabulated-list entry."
  (let ((did (plist-get rec :device-id))
        (ik (plist-get rec :identity-key))
        (trust (plist-get rec :trust))
        (first-seen (plist-get rec :first-seen)))
    (list did
          (vector (number-to-string did)
                  (if trust (jabber-omemo--trust-label trust) "self")
                  (jabber-omemo--format-fingerprint
                   (jabber-omemo-trust--strip-key-type ik))
                  (if first-seen
                      (format-time-string "%Y-%m-%d %H:%M" first-seen)
                    "")))))

(defun jabber-omemo-trust--own-device-entry ()
  "Return an entry for the current device from the OMEMO store, or nil."
  (when-let* ((jc jabber-omemo-trust--jc)
              (own-jid (jabber-connection-bare-jid jc))
              ((string= jabber-omemo-trust--peer own-jid))
              (store (jabber-omemo--get-store jc))
              (bundle (jabber-omemo-get-bundle store))
              (did (jabber-omemo--get-device-id jc))
              (ik (plist-get bundle :identity-key)))
    (let* ((entry (jabber-omemo-trust--format-entry
                    (list :device-id did :identity-key ik
                          :trust nil :first-seen nil)))
           (cols (cadr entry)))
      (aset cols 0 (propertize (aref cols 0)
                               'face 'jabber-chat-nick-encrypted))
      entry)))

(defun jabber-omemo-trust--entries ()
  "Build tabulated-list entries from trust records and fetched bundles."
  (let ((records (jabber-omemo-store-all-trust
                  jabber-omemo-trust--account
                  jabber-omemo-trust--peer))
        (seen (make-hash-table :test #'eql)))
    (append
     (when-let* ((own (jabber-omemo-trust--own-device-entry)))
       (puthash (car own) t seen)
       (list own))
     (mapcar (lambda (rec)
               (puthash (plist-get rec :device-id) t seen)
               (jabber-omemo-trust--format-entry rec))
             records)
     (cl-remove-if (lambda (entry) (gethash (car entry) seen))
                    jabber-omemo-trust--fetched))))

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
      (setq jabber-omemo-trust--jc jc
            jabber-omemo-trust--account account
            jabber-omemo-trust--peer peer
            jabber-omemo-trust--fetched nil)
      (tabulated-list-print t)
      (switch-to-buffer (current-buffer)))
    (jabber-omemo--fetch-device-list
     jc peer
     (lambda (device-ids)
       (dolist (did device-ids)
         (jabber-omemo--fetch-bundle
          jc peer did
          (let ((did did))
            (lambda (bundle)
              (when-let* ((ik (and bundle (plist-get bundle :identity-key)))
                          (buf (get-buffer buf-name)))
                (with-current-buffer buf
                  (push (jabber-omemo-trust--format-entry
                         (list :device-id did
                               :identity-key ik
                               :trust nil
                               :first-seen nil))
                        jabber-omemo-trust--fetched)
                  (tabulated-list-print t)))))))))))

;;;###autoload
(defun jabber-omemo-show-fingerprints (jc)
  "Display own OMEMO fingerprints across all devices for JC.
Fetches the device list and bundles from the server."
  (interactive (list (jabber-read-account)))
  (let* ((own-jid (jabber-connection-bare-jid jc))
         (buf-name (format "*OMEMO fingerprints: %s*" own-jid))
         (our-did (jabber-omemo--get-device-id jc)))
    (with-current-buffer (get-buffer-create buf-name)
      (jabber-omemo-trust-mode)
      (setq jabber-omemo-trust--jc jc
            jabber-omemo-trust--account own-jid
            jabber-omemo-trust--peer own-jid
            jabber-omemo-trust--fetched nil)
      (tabulated-list-print t)
      (switch-to-buffer (current-buffer)))
    (jabber-omemo--fetch-device-list
     jc own-jid
     (lambda (device-ids)
       (dolist (did device-ids)
         (unless (= did our-did)
           (jabber-omemo--fetch-bundle
            jc own-jid did
            (let ((did did))
              (lambda (bundle)
                (when-let* ((ik (and bundle (plist-get bundle :identity-key)))
                            (buf (get-buffer buf-name)))
                  (with-current-buffer buf
                    (let* ((entry (jabber-omemo-trust--format-entry
                                    (list :device-id did
                                          :identity-key ik
                                          :trust nil
                                          :first-seen nil)))
                           (cols (cadr entry)))
                      (aset cols 0 (propertize (aref cols 0)
                                               'face 'jabber-chat-nick-foreign-encrypted))
                      (push entry jabber-omemo-trust--fetched))
                    (tabulated-list-print t))))))))))))

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

(defun jabber-omemo-trust--own-peer-p ()
  "Return non-nil if viewing our own fingerprints."
  (and jabber-omemo-trust--jc
       (string= jabber-omemo-trust--peer
                (jabber-connection-bare-jid jabber-omemo-trust--jc))))

(defun jabber-omemo-trust-delete ()
  "Delete the device key and session at point.
When viewing own fingerprints, also remove the device from the
server-side device list and delete its bundle PubSub node."
  (interactive)
  (let ((did (jabber-omemo-trust--device-at-point)))
    (if (jabber-omemo-trust--own-peer-p)
        (let ((our-did (jabber-omemo--get-device-id jabber-omemo-trust--jc)))
          (when (= did our-did)
            (user-error "Cannot delete the current device"))
          (when (y-or-n-p (format "Remove device %d from server and delete local data? " did))
            (jabber-omemo-store-delete-trust jabber-omemo-trust--account
                                             jabber-omemo-trust--peer did)
            (jabber-omemo-store-delete-session jabber-omemo-trust--account
                                               jabber-omemo-trust--peer did)
            (setq jabber-omemo-trust--fetched
                  (cl-remove-if (lambda (entry) (= (car entry) did))
                                jabber-omemo-trust--fetched))
            (jabber-omemo--remove-device
             jabber-omemo-trust--jc did
             (let ((buf (current-buffer)))
               (lambda ()
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (tabulated-list-print t))))))
            (tabulated-list-print t)
            (message "Device %d removed" did)))
      (when (y-or-n-p (format "Delete key and session for device %d? " did))
        (jabber-omemo-store-delete-trust jabber-omemo-trust--account
                                         jabber-omemo-trust--peer did)
        (jabber-omemo-store-delete-session jabber-omemo-trust--account
                                           jabber-omemo-trust--peer did)
        (tabulated-list-print t)
        (message "Device %d deleted" did)))))

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

;;; Cleanup on disconnect

(defun jabber-omemo-trust--kill-buffers ()
  "Kill all OMEMO trust/fingerprint buffers."
  (dolist (buf (buffer-list))
    (when (eq (buffer-local-value 'major-mode buf) 'jabber-omemo-trust-mode)
      (kill-buffer buf))))

(with-eval-after-load "jabber-core"
  (add-hook 'jabber-post-disconnect-hook #'jabber-omemo-trust--kill-buffers))

(provide 'jabber-omemo-trust)
;;; jabber-omemo-trust.el ends here
