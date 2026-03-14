;;; jabber-openpgp.el --- XEP-0373 OpenPGP encryption for jabber.el  -*- lexical-binding: t; -*-

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

;; XEP-0373 (OpenPGP for XMPP) encryption support.
;; Uses Emacs's built-in EasyPG (epg.el) for GPG operations.
;; Key management via PubSub (jabber-pubsub.el).

;;; Code:

(require 'cl-lib)
(require 'epg)
(require 'jabber-pubsub)
(require 'jabber-xml)
(require 'jabber-hints)

(eval-when-compile (require 'pcase))

(declare-function jabber-connection-bare-jid "jabber-util" (jc))
(declare-function jabber-jid-user "jabber-util" (jid))
(declare-function jabber-disco-advertise-feature "jabber-disco" (feature))
(declare-function jabber-send-sexp "jabber-core" (jc sexp))
(declare-function jabber-chat--msg-plist-from-stanza "jabber-chat"
                  (xml-data &optional delayed))
(declare-function jabber-maybe-print-rare-time "jabber-chat" (node))
(declare-function ewoc-enter-last "ewoc" (ewoc data))

(defvar jabber-chatting-with)           ; jabber-chat.el
(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-chat-ewoc)               ; jabber-chatbuffer.el
(defvar jabber-chat-send-hooks)         ; jabber-chat.el
(defvar jabber-chat-printers)           ; jabber-chat.el
(defvar jabber-muc-participants)        ; jabber-muc.el
(defvar jabber-chat-encryption)         ; jabber-chatbuffer.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el

;;; Constants

(defconst jabber-openpgp-xmlns "urn:xmpp:openpgp:0"
  "Namespace for XEP-0373 OpenPGP elements.")

(defconst jabber-openpgp-pubkeys-node "urn:xmpp:openpgp:0:public-keys"
  "PubSub node for OpenPGP public key metadata.")

(defconst jabber-openpgp-fallback-body
  "This message is encrypted with OpenPGP (XEP-0373)."
  "Fallback body for clients that don't support OpenPGP.")

;;; Customization

(defcustom jabber-openpgp-key-alist nil
  "Alist mapping account bare JIDs to GPG key fingerprints.
Each entry is (JID . FINGERPRINT).  When nil, falls back to
searching the keyring for a key with User ID \"xmpp:JID\"."
  :type '(alist :key-type string :value-type string)
  :group 'jabber-chat)

;;; Key lookup

(defun jabber-openpgp--our-key (jc)
  "Return the EPG key for JC's account.
Lookup order:
1. `jabber-openpgp-key-alist' (per-account fingerprint)
2. `mml-secure-openpgp-signers' (Gnus/message signing key)
3. Keyring search for \"xmpp:BARE-JID\" User ID"
  (let* ((bare-jid (jabber-connection-bare-jid jc))
         (ctx (epg-make-context 'OpenPGP))
         (fingerprint
          (or (cdr (assoc bare-jid jabber-openpgp-key-alist))
              (car (bound-and-true-p mml-secure-openpgp-signers)))))
    (if fingerprint
        (let ((keys (epg-list-keys ctx fingerprint 'secret)))
          (or (car keys)
              (error "OpenPGP: no secret key for fingerprint %s" fingerprint)))
      (or (car (epg-list-keys ctx (concat "xmpp:" bare-jid) 'secret))
          (car (epg-list-keys ctx bare-jid 'secret))
          (error "OpenPGP: no key for %s; configure `jabber-openpgp-key-alist' or `mml-secure-openpgp-signers'"
                 bare-jid)))))

(defun jabber-openpgp--our-key-safe (jc)
  "Return the EPG key for JC, or nil if not configured."
  (condition-case nil
      (jabber-openpgp--our-key jc)
    (error nil)))

(defun jabber-openpgp--key-fingerprint (key)
  "Return uppercase hex fingerprint of KEY."
  (upcase (epg-sub-key-fingerprint
           (car (epg-key-sub-key-list key)))))

(defun jabber-openpgp--recipient-key (jid)
  "Return EPG key for JID from the local keyring, or nil.
Lookup order:
1. `jabber-openpgp-key-alist' (explicit fingerprint)
2. Keyring search for \"xmpp:JID\"
3. Keyring search for bare JID as-is (email-style UID)"
  (let* ((ctx (epg-make-context 'OpenPGP))
         (fingerprint (cdr (assoc jid jabber-openpgp-key-alist))))
    (if fingerprint
        (car (epg-list-keys ctx fingerprint))
      (or (car (epg-list-keys ctx (concat "xmpp:" jid)))
          (car (epg-list-keys ctx jid))))))

(defun jabber-openpgp--ensure-recipient-keys (jc jids callback)
  "Ensure public keys for all JIDS are available, then call CALLBACK.
For any JID whose key is missing locally, fetch it via PubSub.
CALLBACK is called with no arguments once all keys are resolved.
Signals an error (via `message') if any key remains unavailable."
  (let* ((missing (cl-remove-if #'jabber-openpgp--recipient-key jids))
         (remaining (length missing))
         (failed nil))
    (if (zerop remaining)
        (funcall callback)
      (message "OpenPGP: fetching %d key(s)..." remaining)
      (dolist (jid missing)
        (jabber-openpgp--fetch-key
         jc jid
         (lambda (key)
           (unless key
             (push jid failed))
           (cl-decf remaining)
           (when (zerop remaining)
             (if failed
                 (message "OpenPGP: could not fetch keys for: %s"
                          (string-join failed ", "))
               (funcall callback)))))))))

;;; EPG encrypt/decrypt

(defun jabber-openpgp--random-padding ()
  "Return a random padding string for rpad element."
  (let ((len (+ 1 (random 200)))
        (chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (apply #'string
           (cl-loop repeat len
                    collect (aref chars (random (length chars)))))))

(defun jabber-openpgp--encrypt (jc plaintext-xml recipient-jids &optional sign)
  "Encrypt PLAINTEXT-XML for RECIPIENT-JIDS via JC.
When SIGN is non-nil, also sign with JC's key.
Returns raw (non-armored) OpenPGP message bytes.
All recipient keys must already be in the local keyring."
  (let* ((context (epg-make-context 'OpenPGP))
         (our-key (jabber-openpgp--our-key jc))
         (recipients (mapcar (lambda (jid)
                               (or (jabber-openpgp--recipient-key jid)
                                   (error "OpenPGP: no public key for %s" jid)))
                             recipient-jids)))
    (setf (epg-context-armor context) nil)
    (when sign
      (setf (epg-context-signers context) (list our-key)))
    (epg-encrypt-string context
                        (encode-coding-string plaintext-xml 'utf-8)
                        (cons our-key recipients)
                        sign)))

(defun jabber-openpgp--decrypt (ciphertext)
  "Decrypt CIPHERTEXT (raw OpenPGP bytes).
Returns the decrypted string."
  (let ((context (epg-make-context 'OpenPGP)))
    (decode-coding-string
     (epg-decrypt-string context ciphertext)
     'utf-8)))

;;; Key publishing

(defun jabber-openpgp--publish-key (jc)
  "Publish our OpenPGP public key to PubSub via JC."
  (let* ((key (jabber-openpgp--our-key jc))
         (fingerprint (jabber-openpgp--key-fingerprint key))
         (context (epg-make-context 'OpenPGP))
         (_ (setf (epg-context-armor context) nil))
         (key-data (epg-export-keys-to-string context (list key)))
         (node (concat jabber-openpgp-pubkeys-node ":" fingerprint)))
    (jabber-pubsub-publish
     jc nil node fingerprint
     `(pubkey ((xmlns . ,jabber-openpgp-xmlns))
              (data () ,(base64-encode-string key-data t)))
     '(("pubsub#persist_items" . "true")
       ("pubsub#access_model" . "open")))))

(defun jabber-openpgp--publish-metadata (jc)
  "Publish key fingerprint list to the metadata node via JC."
  (let* ((key (jabber-openpgp--our-key jc))
         (fingerprint (jabber-openpgp--key-fingerprint key))
         (date (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))
    (jabber-pubsub-publish
     jc nil jabber-openpgp-pubkeys-node fingerprint
     `(public-keys-list ((xmlns . ,jabber-openpgp-xmlns))
                        (pubkey-metadata ((v4-fingerprint . ,fingerprint)
                                         (date . ,date))))
     '(("pubsub#persist_items" . "true")
       ("pubsub#access_model" . "open")))))

(defun jabber-openpgp-on-connect (jc)
  "Post-connect hook: publish key if configured.
Added to `jabber-post-connect-hooks'."
  (when (jabber-openpgp--our-key-safe jc)
    (jabber-openpgp--publish-key jc)
    (jabber-openpgp--publish-metadata jc)))

;;; Key fetching

(defun jabber-openpgp--fetch-key (jc jid callback)
  "Fetch OpenPGP key for JID via PubSub through JC.
1. Query metadata node for fingerprint.
2. Fetch key data from per-fingerprint node.
3. Import into GPG keyring.
4. Call (funcall CALLBACK epg-key-or-nil)."
  (jabber-pubsub-request
   jc jid jabber-openpgp-pubkeys-node
   (lambda (_jc xml-data _closure)
     (jabber-openpgp--handle-metadata-response jc jid xml-data callback))
   (lambda (_jc xml-data _closure)
     (message "OpenPGP: failed to fetch metadata for %s: %s"
              jid (jabber-sexp2xml xml-data))
     (funcall callback nil))))

(defun jabber-openpgp--handle-metadata-response (jc jid xml-data callback)
  "Handle PubSub metadata response for JID.
JC is the connection.  XML-DATA is the IQ result.
CALLBACK receives the imported key or nil."
  (let* ((pubsub-el (jabber-xml-path xml-data '(pubsub)))
         (items-el (and pubsub-el
                        (car (jabber-xml-get-children pubsub-el 'items))))
         (item-el (and items-el
                       (car (jabber-xml-get-children items-el 'item))))
         (keys-list (and item-el
                         (car (jabber-xml-get-children
                               item-el 'public-keys-list))))
         (meta (and keys-list
                    (car (jabber-xml-get-children
                          keys-list 'pubkey-metadata))))
         (fingerprint (and meta
                           (jabber-xml-get-attribute meta 'v4-fingerprint))))
    (if (null fingerprint)
        (progn
          (message "OpenPGP: no key metadata for %s" jid)
          (funcall callback nil))
      (let ((node (concat jabber-openpgp-pubkeys-node ":" fingerprint)))
        (jabber-pubsub-request
         jc jid node
         (lambda (_jc xml-data2 _closure)
           (jabber-openpgp--handle-key-response xml-data2 fingerprint callback))
         (lambda (_jc _xml-data2 _closure)
           (message "OpenPGP: failed to fetch key %s for %s" fingerprint jid)
           (funcall callback nil)))))))

(defun jabber-openpgp--handle-key-response (xml-data fingerprint callback)
  "Handle PubSub key data response.
XML-DATA is the IQ result.  FINGERPRINT identifies the expected key.
CALLBACK receives the imported key or nil."
  (let* ((pubsub-el (jabber-xml-path xml-data '(pubsub)))
         (items-el (and pubsub-el
                        (car (jabber-xml-get-children pubsub-el 'items))))
         (item-el (and items-el
                       (car (jabber-xml-get-children items-el 'item))))
         (pubkey-el (and item-el
                         (car (jabber-xml-get-children item-el 'pubkey))))
         (data-el (and pubkey-el
                       (car (jabber-xml-get-children pubkey-el 'data))))
         (b64 (and data-el (car (jabber-xml-node-children data-el)))))
    (if (null b64)
        (progn
          (message "OpenPGP: empty key data for %s" fingerprint)
          (funcall callback nil))
      (let* ((key-data (base64-decode-string b64))
             (context (epg-make-context 'OpenPGP)))
        (condition-case err
            (progn
              (epg-import-keys-from-string context key-data)
              (let ((keys (epg-list-keys context fingerprint)))
                (funcall callback (car keys))))
          (error
           (message "OpenPGP: key import failed: %s"
                    (error-message-string err))
           (funcall callback nil)))))))

;;; Signcrypt XML building

(defun jabber-openpgp--build-signcrypt-xml (recipient-jids body)
  "Build <signcrypt> XML string for RECIPIENT-JIDS containing BODY."
  (jabber-sexp2xml
   `(signcrypt ((xmlns . ,jabber-openpgp-xmlns))
               ,@(mapcar (lambda (jid) `(to ((jid . ,jid)))) recipient-jids)
               (time ((stamp . ,(format-time-string
                                 "%Y-%m-%dT%H:%M:%SZ" nil t))))
               (rpad () ,(jabber-openpgp--random-padding))
               (payload ()
                        (body ((xmlns . "jabber:client")) ,body)))))

(defun jabber-openpgp--build-crypt-xml (recipient-jids body)
  "Build <crypt> XML string for RECIPIENT-JIDS containing BODY.
Used for MUC where signing is optional."
  (jabber-sexp2xml
   `(crypt ((xmlns . ,jabber-openpgp-xmlns))
           ,@(mapcar (lambda (jid) `(to ((jid . ,jid)))) recipient-jids)
           (time ((stamp . ,(format-time-string
                             "%Y-%m-%dT%H:%M:%SZ" nil t))))
           (rpad () ,(jabber-openpgp--random-padding))
           (payload ()
                    (body ((xmlns . "jabber:client")) ,body)))))

;;; Send path: 1:1 chat

(defun jabber-openpgp--send-chat (jc body)
  "Send BODY as OpenPGP-encrypted chat message via JC.
Must be called from a chat buffer with `jabber-chatting-with' set.
Fetches missing recipient keys via PubSub before encrypting."
  (let ((recipient (jabber-jid-user jabber-chatting-with))
        (buffer (current-buffer)))
    (jabber-openpgp--ensure-recipient-keys
     jc (list recipient)
     (lambda ()
       (with-current-buffer buffer
         (jabber-openpgp--send-chat-1 jc body recipient))))))

(defun jabber-openpgp--send-chat-1 (jc body recipient)
  "Internal: encrypt and send BODY to RECIPIENT via JC."
  (let* ((inner-xml (jabber-openpgp--build-signcrypt-xml
                     (list recipient) body))
         (encrypted (jabber-openpgp--encrypt
                     jc inner-xml (list recipient) t))
         (id (apply #'format "emacs-msg-%d.%d.%d" (current-time)))
         (stanza `(message ((to . ,jabber-chatting-with)
                            (type . "chat")
                            (id . ,id))
                           (openpgp ((xmlns . ,jabber-openpgp-xmlns))
                                    ,(base64-encode-string encrypted t))
                           (body () ,jabber-openpgp-fallback-body)
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
    (jabber-send-sexp jc stanza)))

;;; Send path: MUC

(defun jabber-openpgp--muc-participant-jids (group)
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

(defun jabber-openpgp--send-muc (jc body)
  "Send BODY as OpenPGP-encrypted groupchat message via JC.
Must be called from a MUC buffer with `jabber-group' set.
Fetches missing recipient keys via PubSub before encrypting."
  (let* ((group jabber-group)
         (recipient-jids (jabber-openpgp--muc-participant-jids group))
         (our-jid (jabber-jid-user (jabber-connection-bare-jid jc)))
         (all-jids (if (member our-jid recipient-jids)
                       recipient-jids
                     (cons our-jid recipient-jids)))
         (buffer (current-buffer)))
    (when (null recipient-jids)
      (user-error "OpenPGP: no participant JIDs available (room may be anonymous)"))
    (jabber-openpgp--ensure-recipient-keys
     jc all-jids
     (lambda ()
       (with-current-buffer buffer
         (let* ((inner-xml (jabber-openpgp--build-crypt-xml all-jids body))
                (encrypted (jabber-openpgp--encrypt jc inner-xml all-jids))
                (stanza `(message ((to . ,group)
                                   (type . "groupchat"))
                                  (openpgp ((xmlns . ,jabber-openpgp-xmlns))
                                           ,(base64-encode-string encrypted t))
                                  (body () ,jabber-openpgp-fallback-body)
                                  ,(jabber-hints-store))))
           (jabber-send-sexp jc stanza)))))))

;;; Receive path

(defun jabber-openpgp--parse-openpgp-element (xml-data)
  "Return the <openpgp> child element from XML-DATA, or nil."
  (jabber-xml-child-with-xmlns xml-data jabber-openpgp-xmlns))

(defun jabber-openpgp--decrypt-stanza (_jc xml-data openpgp-el)
  "Decrypt the <openpgp> element and replace body in XML-DATA.
OPENPGP-EL is the <openpgp> child element."
  (let* ((b64 (car (jabber-xml-node-children openpgp-el)))
         (ciphertext (base64-decode-string b64))
         (inner-xml-str (jabber-openpgp--decrypt ciphertext))
         (inner-xml (with-temp-buffer
                      (insert inner-xml-str)
                      (car (xml-parse-region (point-min) (point-max)))))
         (inner-name (and inner-xml (jabber-xml-node-name inner-xml)))
         (_ (unless (memq inner-name '(signcrypt crypt))
              (error "OpenPGP: unexpected inner element <%s>" inner-name)))
         (payload (car (jabber-xml-get-children inner-xml 'payload)))
         (inner-body (and payload
                         (car (jabber-xml-get-children payload 'body))))
         (body-text (and inner-body
                         (car (jabber-xml-node-children inner-body)))))
    (if body-text
        (let ((body-el (car (jabber-xml-get-children xml-data 'body))))
          (if body-el
              (setcar (cddr body-el) body-text)
            (nconc xml-data (list `(body () ,body-text)))))
      (let ((body-el (car (jabber-xml-get-children xml-data 'body))))
        (if body-el
            (setcar (cddr body-el) "[OpenPGP: empty payload]")
          (nconc xml-data (list '(body () "[OpenPGP: empty payload]"))))))
    xml-data))

(defun jabber-openpgp--decrypt-if-needed (orig-fn jc xml-data)
  "Around advice for `jabber-chat--decrypt-if-needed'.
If XML-DATA contains an <openpgp> element, decrypt it.
Otherwise delegate to ORIG-FN."
  (let ((openpgp-el (jabber-openpgp--parse-openpgp-element xml-data)))
    (if (null openpgp-el)
        (funcall orig-fn jc xml-data)
      (condition-case err
          (jabber-openpgp--decrypt-stanza jc xml-data openpgp-el)
        (error
         (message "OpenPGP decrypt failed: %s" (error-message-string err))
         (let ((body-el (car (jabber-xml-get-children xml-data 'body))))
           (if body-el
               (setcar (cddr body-el) "[OpenPGP: could not decrypt]")
             (nconc xml-data
                    (list '(body () "[OpenPGP: could not decrypt]")))))
         xml-data)))))

;;; Disco and hooks

(jabber-disco-advertise-feature jabber-openpgp-xmlns)

(advice-add 'jabber-chat--decrypt-if-needed :around
            #'jabber-openpgp--decrypt-if-needed '((depth . 20)))

(with-eval-after-load "jabber-core"
  (add-hook 'jabber-post-connect-hooks #'jabber-openpgp-on-connect))

(provide 'jabber-openpgp)
;;; jabber-openpgp.el ends here
