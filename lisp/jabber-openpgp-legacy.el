;;; jabber-openpgp-legacy.el --- XEP-0027 Legacy OpenPGP for jabber.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
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

;; XEP-0027 (Current Jabber OpenPGP Usage) support.
;; Used by Conversations (Android) and other legacy XMPP clients.
;;
;; Protocol summary:
;; - Signed presence: sign <status> text, send as <x xmlns='jabber:x:signed'>
;; - Encrypted message: encrypt <body>, send as <x xmlns='jabber:x:encrypted'>
;; - Armor headers are stripped per XEP-0027; only base64 body is transmitted
;;
;; Uses Emacs's built-in EasyPG (epg.el) for GPG operations.
;; Reuses key lookup from jabber-openpgp.el (XEP-0373).

;;; Code:

(require 'epg)
(require 'jabber-xml)
(require 'jabber-hints)

(eval-when-compile (require 'cl-lib))

(declare-function jabber-openpgp--our-key "jabber-openpgp" (jc))
(declare-function jabber-openpgp--our-key-safe "jabber-openpgp" (jc))
(declare-function jabber-openpgp--recipient-key "jabber-openpgp" (jid))
(declare-function jabber-connection-bare-jid "jabber-util" (jc))
(declare-function jabber-jid-user "jabber-util" (jid))
(declare-function jabber-send-sexp "jabber-core" (jc sexp))
(declare-function jabber-chat--msg-plist-from-stanza "jabber-chat"
                  (xml-data &optional delayed))
(declare-function jabber-maybe-print-rare-time "jabber-chat" (node))
(declare-function ewoc-enter-last "ewoc" (ewoc data))
(declare-function jabber-disco-advertise-feature "jabber-disco" (feature))

(defvar jabber-chatting-with)           ; jabber-chat.el
(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-chat-ewoc)               ; jabber-chatbuffer.el
(defvar jabber-chat-send-hooks)         ; jabber-chat.el
(defvar jabber-chat-printers)           ; jabber-chat.el
(defvar jabber-muc-participants)        ; jabber-muc.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
(defvar jabber-presence-element-functions) ; jabber-presence.el
(defvar jabber-presence-chain)          ; jabber-core.el
(defvar *jabber-current-status*)        ; jabber.el
(defvar jabber-jid-obarray)             ; jabber-util.el

;;; Constants

(defconst jabber-openpgp-legacy-signed-xmlns "jabber:x:signed"
  "Namespace for XEP-0027 signed presence.")

(defconst jabber-openpgp-legacy-encrypted-xmlns "jabber:x:encrypted"
  "Namespace for XEP-0027 encrypted messages.")

(defconst jabber-openpgp-legacy-fallback-body
  "This message is encrypted with OpenPGP."
  "Fallback body for clients that don't support XEP-0027.")

;;; Customization

(defcustom jabber-openpgp-legacy-sign-presence t
  "Whether to sign outgoing presence with OpenPGP.
Only active when a GPG key is configured."
  :type 'boolean
  :group 'jabber)

(defcustom jabber-openpgp-legacy-auto-fetch-keys t
  "Whether to automatically fetch GPG keys from keyservers.
When a signed presence reveals a key ID not in the local keyring,
attempt to fetch it via `gpg --recv-keys'."
  :type 'boolean
  :group 'jabber)

;;; Armor helpers

(defun jabber-openpgp-legacy--strip-armor (armored-text)
  "Strip PGP armor headers and footers from ARMORED-TEXT.
Returns only the base64 body and checksum, as required by XEP-0027."
  (with-temp-buffer
    (insert armored-text)
    (goto-char (point-min))
    ;; Skip the -----BEGIN PGP ... ----- line
    (when (re-search-forward "^-----BEGIN PGP [^-]+-----\n" nil t)
      (delete-region (point-min) (point)))
    ;; Skip header lines (Version:, Hash:, Comment:, etc.) and blank line
    (goto-char (point-min))
    (while (looking-at "^[A-Za-z]+: .*\n")
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    ;; Skip blank line after headers
    (when (looking-at "^\n")
      (delete-region (point) (1+ (point))))
    ;; Remove the -----END PGP ... ----- line
    (goto-char (point-max))
    (when (re-search-backward "^-----END PGP [^-]+-----" nil t)
      (delete-region (line-beginning-position) (point-max)))
    ;; Trim trailing whitespace
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (delete-region (point) (point-max))
    (buffer-string)))

(defun jabber-openpgp-legacy--rearmor-signature (stripped)
  "Wrap STRIPPED base64 back into a PGP SIGNATURE armor block."
  (concat "-----BEGIN PGP SIGNATURE-----\n\n"
          stripped "\n"
          "-----END PGP SIGNATURE-----\n"))

(defun jabber-openpgp-legacy--rearmor-message (stripped)
  "Wrap STRIPPED base64 back into a PGP MESSAGE armor block."
  (concat "-----BEGIN PGP MESSAGE-----\n\n"
          stripped "\n"
          "-----END PGP MESSAGE-----\n"))

;;; Signed presence (outgoing)

(defun jabber-openpgp-legacy--sign-presence (jc)
  "Return signed presence elements for JC.
Added to `jabber-presence-element-functions'.
Signs the current status text (or empty string) with our GPG key."
  (when jabber-openpgp-legacy-sign-presence
    (require 'jabber-openpgp)
    (when-let* ((key (jabber-openpgp--our-key-safe jc)))
      (condition-case err
          (let* ((status (or (bound-and-true-p *jabber-current-status*) ""))
                 (ctx (epg-make-context 'OpenPGP))
                 (_ (setf (epg-context-armor ctx) t))
                 (_ (setf (epg-context-signers ctx) (list key)))
                 (sig (epg-sign-string ctx
                                       (encode-coding-string status 'utf-8)
                                       'detached))
                 (stripped (jabber-openpgp-legacy--strip-armor sig)))
            (list `(x ((xmlns . ,jabber-openpgp-legacy-signed-xmlns))
                      ,stripped)))
        (error
         (message "XEP-0027: signing presence failed: %s"
                  (error-message-string err))
         nil)))))

;;; Signed presence (incoming)

(defun jabber-openpgp-legacy--process-presence (_jc xml-data)
  "Process incoming presence for XEP-0027 signed element.
JC is the connection.  XML-DATA is the presence stanza.
Verifies the signature and optionally fetches missing keys."
  (when-let* ((x-el (jabber-xml-child-with-xmlns
                      xml-data jabber-openpgp-legacy-signed-xmlns))
              (stripped (car (jabber-xml-node-children x-el)))
              ((stringp stripped)))
    (let* ((from (jabber-xml-get-attribute xml-data 'from))
           (status-el (car (jabber-xml-get-children xml-data 'status)))
           (status (or (and status-el
                            (car (jabber-xml-node-children status-el)))
                       ""))
           (armored (jabber-openpgp-legacy--rearmor-signature stripped))
           (ctx (epg-make-context 'OpenPGP)))
      (condition-case err
          (progn
            (epg-verify-string ctx armored
                               (encode-coding-string status 'utf-8))
            (let ((result (epg-context-result-for ctx 'verify)))
              (when result
                (let* ((sig (car result))
                       (fpr (epg-signature-fingerprint sig)))
                  (when fpr
                    (let ((bare (jabber-jid-user from)))
                      (put (intern bare jabber-jid-obarray)
                           'pgp-key-id fpr)))))))
        (error
         (let ((msg (error-message-string err)))
           (when (and jabber-openpgp-legacy-auto-fetch-keys
                      (string-match-p "No public key" msg))
             (jabber-openpgp-legacy--try-fetch-key-from-error msg from))))))))

(defun jabber-openpgp-legacy--try-fetch-key-from-error (msg from)
  "Try to fetch a GPG key based on error MSG.
FROM is the JID that sent the signed presence."
  (when (string-match "\\([0-9A-Fa-f]\\{8,\\}\\)" msg)
    (let ((key-id (match-string 1 msg)))
      (message "XEP-0027: fetching key %s for %s..." key-id from)
      (let ((status (call-process "gpg" nil nil nil
                                  "--recv-keys" key-id)))
        (if (zerop status)
            (message "XEP-0027: fetched key %s for %s" key-id from)
          (message "XEP-0027: failed to fetch key %s for %s"
                   key-id from))))))

;;; Message encryption (send) - 1:1 chat

(defun jabber-openpgp-legacy--send-chat (jc body)
  "Send BODY as XEP-0027 encrypted message via JC.
Must be called from a chat buffer with `jabber-chatting-with' set."
  (require 'jabber-openpgp)
  (let* ((recipient (jabber-jid-user jabber-chatting-with))
         (key (jabber-openpgp--recipient-key recipient))
         (our-key (jabber-openpgp--our-key jc)))
    (unless key
      (user-error "XEP-0027: no public key for %s" recipient))
    (let* ((ctx (epg-make-context 'OpenPGP))
           (_ (setf (epg-context-armor ctx) t))
           (encrypted (epg-encrypt-string ctx
                                          (encode-coding-string body 'utf-8)
                                          (list our-key key)
                                          nil))
           (stripped (jabber-openpgp-legacy--strip-armor encrypted))
           (id (format "emacs-msg-%d" (floor (* 1000 (float-time)))))
           (stanza `(message ((to . ,jabber-chatting-with)
                              (type . "chat")
                              (id . ,id))
                             (body () ,jabber-openpgp-legacy-fallback-body)
                             (x ((xmlns . ,jabber-openpgp-legacy-encrypted-xmlns))
                                ,stripped)
                             ,(jabber-hints-store))))
      (dolist (hook jabber-chat-send-hooks)
        (if (eq hook t)
            (when (local-variable-p 'jabber-chat-send-hooks)
              (dolist (global-hook (default-value 'jabber-chat-send-hooks))
                (nconc stanza (funcall global-hook body id))))
          (nconc stanza (funcall hook body id))))
      (let ((msg-plist (jabber-chat--msg-plist-from-stanza stanza)))
        (plist-put msg-plist :body body)
        (when (run-hook-with-args-until-success 'jabber-chat-printers
                                                msg-plist :local :printp)
          (jabber-maybe-print-rare-time
           (ewoc-enter-last jabber-chat-ewoc (list :local msg-plist)))))
      (jabber-send-sexp jc stanza))))

;;; Message encryption (send) - MUC

(defun jabber-openpgp-legacy--muc-participant-jids (group)
  "Return bare JIDs for participants in GROUP.
Excludes entries without a real JID."
  (let ((participants (cdr (assoc group jabber-muc-participants)))
        jids)
    (dolist (entry participants)
      (when-let* ((plist (cdr entry))
                  (full-jid (plist-get plist 'jid))
                  (bare (jabber-jid-user full-jid)))
        (unless (member bare jids)
          (push bare jids))))
    (nreverse jids)))

(defun jabber-openpgp-legacy--send-muc (jc body)
  "Send BODY as XEP-0027 encrypted groupchat message via JC.
Must be called from a MUC buffer with `jabber-group' set."
  (require 'jabber-openpgp)
  (let* ((group jabber-group)
         (recipient-jids (jabber-openpgp-legacy--muc-participant-jids group)))
    (when (null recipient-jids)
      (user-error "XEP-0027: no participant JIDs available (room may be anonymous)"))
    (let* ((our-key (jabber-openpgp--our-key jc))
           (keys (mapcar (lambda (jid)
                           (or (jabber-openpgp--recipient-key jid)
                               (user-error "XEP-0027: no public key for %s" jid)))
                         recipient-jids))
           (all-keys (cons our-key keys))
           (ctx (epg-make-context 'OpenPGP))
           (_ (setf (epg-context-armor ctx) t))
           (encrypted (epg-encrypt-string ctx
                                          (encode-coding-string body 'utf-8)
                                          all-keys
                                          nil))
           (stripped (jabber-openpgp-legacy--strip-armor encrypted))
           (stanza `(message ((to . ,group)
                              (type . "groupchat"))
                             (body () ,jabber-openpgp-legacy-fallback-body)
                             (x ((xmlns . ,jabber-openpgp-legacy-encrypted-xmlns))
                                ,stripped)
                             ,(jabber-hints-store))))
      (jabber-send-sexp jc stanza))))

;;; Message decryption (receive)

(defun jabber-openpgp-legacy--decrypt-if-needed (orig-fn jc xml-data)
  "Around advice for `jabber-chat--decrypt-if-needed'.
If XML-DATA contains <x xmlns='jabber:x:encrypted'>, decrypt it.
Otherwise delegate to ORIG-FN.  JC is the Jabber connection."
  (let* ((x-el (jabber-xml-child-with-xmlns
                xml-data jabber-openpgp-legacy-encrypted-xmlns))
         (stripped (and x-el (car (jabber-xml-node-children x-el)))))
    (if (not (stringp stripped))
        (funcall orig-fn jc xml-data)
      (condition-case err
          (let* ((armored (jabber-openpgp-legacy--rearmor-message stripped))
                 (ctx (epg-make-context 'OpenPGP))
                 (plaintext (decode-coding-string
                             (epg-decrypt-string ctx armored)
                             'utf-8))
                 (body-el (car (jabber-xml-get-children xml-data 'body))))
            (if body-el
                (setcar (cddr body-el) plaintext)
              (nconc xml-data (list `(body () ,plaintext))))
            xml-data)
        (error
         (message "XEP-0027 decrypt failed: %s" (error-message-string err))
         (let ((body-el (car (jabber-xml-get-children xml-data 'body))))
           (if body-el
               (setcar (cddr body-el) "[PGP: could not decrypt]")
             (nconc xml-data
                    (list '(body () "[PGP: could not decrypt]")))))
         xml-data)))))

;;; Registration

(jabber-disco-advertise-feature jabber-openpgp-legacy-signed-xmlns)

(advice-add 'jabber-chat--decrypt-if-needed :around
            #'jabber-openpgp-legacy--decrypt-if-needed '((depth . 30)))

(add-to-list 'jabber-presence-element-functions
             #'jabber-openpgp-legacy--sign-presence)

(add-to-list 'jabber-presence-chain
             #'jabber-openpgp-legacy--process-presence)

(provide 'jabber-openpgp-legacy)
;;; jabber-openpgp-legacy.el ends here
