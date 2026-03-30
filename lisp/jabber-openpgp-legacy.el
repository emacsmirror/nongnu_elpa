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
(declare-function jabber-chat--run-send-hooks "jabber-chat"
                  (stanza body id))
(declare-function jabber-chat--msg-plist-from-stanza "jabber-chat"
                  (xml-data &optional delayed))
(declare-function jabber-maybe-print-rare-time "jabber-chat" (node))
(declare-function jabber-chat-ewoc-enter "jabber-chatbuffer" (data))
(declare-function jabber-disco-advertise-feature "jabber-disco" (feature))
(declare-function jabber-chat-register-decrypt-handler "jabber-chat"
  (id &rest props))
(declare-function jabber-chat--set-body "jabber-chat" (xml-data text))

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

(defvar jabber-openpgp-legacy--sign-cache nil
  "Cache for presence signature: (STATUS KEY ELEMENTS).
Avoids redundant GPG calls when signing the same status text.")

(defvar jabber-openpgp-legacy--signing-in-progress nil
  "Non-nil when GPG signing is in progress.
`epg-wait-for-status' processes pending timers while waiting for
GPG, which can trigger MUC joins that call this function again.
This guard prevents the re-entrant nesting that causes
`excessive-lisp-nesting'.")

(defun jabber-openpgp-legacy--sign-presence (jc)
  "Return signed presence elements for JC.
Added to `jabber-presence-element-functions'.
Signs the current status text (or empty string) with our GPG key.
Caches the result and guards against re-entrant GPG calls."
  (when jabber-openpgp-legacy-sign-presence
    (require 'jabber-openpgp)
    (when-let* ((key (jabber-openpgp--our-key-safe jc)))
      (let ((status (or (bound-and-true-p *jabber-current-status*) "")))
        (cond
         ;; Cache hit: same status and key, return cached elements.
         ((and jabber-openpgp-legacy--sign-cache
               (equal status (nth 0 jabber-openpgp-legacy--sign-cache))
               (eq key (nth 1 jabber-openpgp-legacy--sign-cache)))
          (nth 2 jabber-openpgp-legacy--sign-cache))
         ;; Re-entrance: GPG is already running, skip signing.
         ;; The MUC join presence will lack the signature this time;
         ;; subsequent presence updates will use the cached result.
         (jabber-openpgp-legacy--signing-in-progress nil)
         ;; Normal case: sign, cache, and return.
         (t
          (condition-case err
              (let ((jabber-openpgp-legacy--signing-in-progress t))
                (let* ((ctx (epg-make-context 'OpenPGP))
                       (_ (setf (epg-context-armor ctx) t))
                       (_ (setf (epg-context-signers ctx) (list key)))
                       (sig (epg-sign-string ctx
                                             (encode-coding-string status 'utf-8)
                                             'detached))
                       (stripped (jabber-openpgp-legacy--strip-armor sig))
                       (elements (list `(x ((xmlns . ,jabber-openpgp-legacy-signed-xmlns))
                                           ,stripped))))
                  (setq jabber-openpgp-legacy--sign-cache
                        (list status key elements))
                  elements))
            (error
             (message "XEP-0027: signing presence failed: %s"
                      (error-message-string err))
             nil))))))))

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
      (jabber-chat--run-send-hooks stanza body id)
      (let ((msg-plist (jabber-chat--msg-plist-from-stanza stanza)))
        (plist-put msg-plist :body body)
        (plist-put msg-plist :status :sent)
        (when (run-hook-with-args-until-success 'jabber-chat-printers
                                                msg-plist :local :printp)
          (jabber-maybe-print-rare-time
           (jabber-chat-ewoc-enter (list :local msg-plist)))))
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

(defun jabber-openpgp-legacy--detect-encrypted (xml-data)
  "Return stripped armor text from <x xmlns='jabber:x:encrypted'>, or nil."
  (when-let* ((x-el (jabber-xml-child-with-xmlns
                      xml-data jabber-openpgp-legacy-encrypted-xmlns))
              (stripped (car (jabber-xml-node-children x-el))))
    (and (stringp stripped) stripped)))

(defun jabber-openpgp-legacy--decrypt-handler (_jc xml-data stripped)
  "Decrypt XEP-0027 message.  STRIPPED is the base64-armored ciphertext."
  (let* ((armored (jabber-openpgp-legacy--rearmor-message stripped))
         (ctx (epg-make-context 'OpenPGP))
         (plaintext (decode-coding-string
                     (epg-decrypt-string ctx armored)
                     'utf-8)))
    (jabber-chat--set-body xml-data plaintext)
    xml-data))

;;; Registration

(jabber-disco-advertise-feature jabber-openpgp-legacy-signed-xmlns)

(jabber-chat-register-decrypt-handler
 'openpgp-legacy
 :detect  #'jabber-openpgp-legacy--detect-encrypted
 :decrypt #'jabber-openpgp-legacy--decrypt-handler
 :priority 30
 :error-label "PGP")

(add-to-list 'jabber-presence-element-functions
             #'jabber-openpgp-legacy--sign-presence)

(with-eval-after-load "jabber-core"
  (jabber-chain-add 'jabber-presence-chain
                    #'jabber-openpgp-legacy--process-presence 30))

(provide 'jabber-openpgp-legacy)
;;; jabber-openpgp-legacy.el ends here
