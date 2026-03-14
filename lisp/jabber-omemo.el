;;; jabber-omemo.el --- OMEMO encryption for jabber.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

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

;; Public Elisp API for OMEMO 0.3 (eu.siacs.conversations.axolotl).
;; Wraps the jabber-omemo-core dynamic module (picomemo).
;;
;; This file handles loading the native module, building it on demand
;; if missing, and re-exports the core functions under the public
;; jabber-omemo- namespace.

;;; Code:

(require 'cl-lib)
(require 'jabber-omemo-store)
(require 'jabber-pubsub)
(require 'jabber-xml)

(declare-function jabber-connection-bare-jid "jabber-util")
(declare-function jabber-jid-user "jabber-util")
(declare-function jabber-disco-advertise-feature "jabber-disco")
(declare-function jabber-send-iq "jabber-iq")

(defvar jabber-post-connect-hooks)
(defvar jabber-pre-disconnect-hook)
(defvar jabber-pubsub-node-handlers)

(unless module-file-suffix
  (error "jabber-omemo requires Emacs compiled with dynamic module support"))

(defvar jabber-omemo-build-command "make -C src"
  "Shell command to build the jabber-omemo-core dynamic module.
Run from the emacs-jabber project root.")

(cl-eval-when (load eval)
  (unless (require 'jabber-omemo-core nil t)
    (let ((src-dir (expand-file-name
                    "../src"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))))
      (if (and (file-exists-p (expand-file-name "jabber-omemo-core.c" src-dir))
               (or noninteractive
                   (yes-or-no-p
                    "jabber-omemo-core module not found.  Build it now? ")))
          (let ((default-directory
                 (file-name-directory
                  (directory-file-name src-dir))))
            (message "Building jabber-omemo-core module...")
            (unless (zerop (call-process-shell-command
                            jabber-omemo-build-command nil
                            (get-buffer-create "*jabber-omemo-build*") t))
              (pop-to-buffer "*jabber-omemo-build*")
              (error "Failed to build jabber-omemo-core module"))
            (require 'jabber-omemo-core))
        (user-error "OMEMO support requires the jabber-omemo-core native module")))))

;; Declare internal C functions for the byte-compiler.
(declare-function jabber-omemo--setup-store "jabber-omemo-core")
(declare-function jabber-omemo--deserialize-store "jabber-omemo-core")
(declare-function jabber-omemo--serialize-store "jabber-omemo-core")
(declare-function jabber-omemo--get-bundle "jabber-omemo-core")
(declare-function jabber-omemo--rotate-signed-pre-key "jabber-omemo-core")
(declare-function jabber-omemo--refill-pre-keys "jabber-omemo-core")
(declare-function jabber-omemo--encrypt-message "jabber-omemo-core")
(declare-function jabber-omemo--decrypt-message "jabber-omemo-core")
(declare-function jabber-omemo--make-session "jabber-omemo-core")
(declare-function jabber-omemo--initiate-session "jabber-omemo-core")
(declare-function jabber-omemo--serialize-session "jabber-omemo-core")
(declare-function jabber-omemo--deserialize-session "jabber-omemo-core")
(declare-function jabber-omemo--encrypt-key "jabber-omemo-core")
(declare-function jabber-omemo--decrypt-key "jabber-omemo-core")
(declare-function jabber-omemo--heartbeat "jabber-omemo-core")

;; Public API

(defun jabber-omemo-setup-store ()
  "Generate a new OMEMO device store.
Returns a serialized store as a unibyte string."
  (jabber-omemo--setup-store))

(defun jabber-omemo-deserialize-store (blob)
  "Deserialize BLOB into an OMEMO store object.
Returns a user-ptr; freed automatically by GC."
  (jabber-omemo--deserialize-store blob))

(defun jabber-omemo-serialize-store (store-ptr)
  "Serialize STORE-PTR back to a unibyte string."
  (jabber-omemo--serialize-store store-ptr))

(defun jabber-omemo-get-bundle (store-ptr)
  "Extract the public bundle from STORE-PTR.
Returns a plist with keys :identity-key, :signed-pre-key,
:signed-pre-key-id, :signature, :pre-keys."
  (jabber-omemo--get-bundle store-ptr))

(defun jabber-omemo-rotate-signed-pre-key (store-ptr)
  "Rotate the signed pre-key in STORE-PTR.
Mutates the store; caller must re-serialize."
  (jabber-omemo--rotate-signed-pre-key store-ptr))

(defun jabber-omemo-refill-pre-keys (store-ptr)
  "Refill removed pre-keys in STORE-PTR.
Mutates the store; caller must re-serialize."
  (jabber-omemo--refill-pre-keys store-ptr))

(defun jabber-omemo-encrypt-message (plaintext)
  "Encrypt PLAINTEXT (a unibyte string) with OMEMO 0.3.
Returns a plist (:key KEY :iv IV :ciphertext CT),
all unibyte strings."
  (jabber-omemo--encrypt-message plaintext))

(defun jabber-omemo-decrypt-message (key iv ciphertext)
  "Decrypt an OMEMO 0.3 message.
KEY is a unibyte string (>= 32 bytes: 16 AES key + auth tag).
IV is a 12-byte unibyte string.
CIPHERTEXT is the encrypted payload.
Returns the plaintext as a unibyte string."
  (jabber-omemo--decrypt-message key iv ciphertext))

(defun jabber-omemo-make-session ()
  "Allocate an empty OMEMO session.
Returns a session user-ptr; freed automatically by GC.
Use for the receiving side of a pre-key message."
  (jabber-omemo--make-session))

(defun jabber-omemo-initiate-session (store-ptr sig spk ik pk spk-id pk-id)
  "Initiate an OMEMO session with a remote device's bundle.
STORE-PTR is the local OMEMO store.
SIG is a 64-byte signature, SPK/IK/PK are 33-byte serialized keys.
SPK-ID and PK-ID are integer key IDs.
Returns a session user-ptr; freed automatically by GC."
  (jabber-omemo--initiate-session store-ptr sig spk ik pk spk-id pk-id))

(defun jabber-omemo-serialize-session (session-ptr)
  "Serialize SESSION-PTR to a unibyte string."
  (jabber-omemo--serialize-session session-ptr))

(defun jabber-omemo-deserialize-session (blob)
  "Deserialize BLOB into an OMEMO session object.
Returns a session user-ptr; freed automatically by GC."
  (jabber-omemo--deserialize-session blob))

(defun jabber-omemo-encrypt-key (session-ptr key)
  "Encrypt KEY for a recipient using SESSION-PTR.
KEY is a unibyte string (the message encryption key).
Returns a plist (:data BYTES :pre-key-p BOOL)."
  (jabber-omemo--encrypt-key session-ptr key))

(defun jabber-omemo-decrypt-key (session-ptr store-ptr pre-key-p msg)
  "Decrypt an encrypted key message.
SESSION-PTR is the session with the sender.
STORE-PTR is the local OMEMO store.
PRE-KEY-P is non-nil if this is a pre-key message.
MSG is the encrypted key message as a unibyte string.
Returns the decrypted key as a unibyte string."
  (jabber-omemo--decrypt-key session-ptr store-ptr pre-key-p msg))

(defun jabber-omemo-heartbeat (session-ptr store-ptr)
  "Check if a heartbeat message is needed after decryption.
SESSION-PTR is the session to check.
STORE-PTR is the local OMEMO store.
Returns heartbeat message bytes or nil."
  (jabber-omemo--heartbeat session-ptr store-ptr))

;;; Protocol constants

(defconst jabber-omemo-xmlns "eu.siacs.conversations.axolotl"
  "OMEMO 0.3 XML namespace.")

(defconst jabber-omemo-devicelist-node
  "eu.siacs.conversations.axolotl.devicelist"
  "PubSub node for OMEMO device lists.")

(defconst jabber-omemo-bundles-node-prefix
  "eu.siacs.conversations.axolotl.bundles:"
  "PubSub node prefix for OMEMO bundles (append device ID).")

(defconst jabber-omemo--devicelist-publish-options
  '(("pubsub#access_model" . "open"))
  "Publish-options for the OMEMO device list PubSub node.")

(defconst jabber-omemo--bundle-publish-options
  '(("pubsub#persist_items" . "true")
    ("pubsub#max_items" . "max")
    ("pubsub#access_model" . "open"))
  "Publish-options for OMEMO bundle PubSub nodes.")

;;; In-memory state

(defvar jabber-omemo--device-ids (make-hash-table :test 'equal)
  "Cache of account -> device ID (integer).")

(defvar jabber-omemo--stores (make-hash-table :test 'equal)
  "Cache of account -> deserialized store user-ptr.")

(defvar jabber-omemo--device-lists (make-hash-table :test 'equal)
  "Cache of \"account\\0jid\" -> list of device ID integers.")

(defvar jabber-omemo--sessions (make-hash-table :test 'equal)
  "Cache of \"account\\0jid\\0device-id\" -> deserialized session user-ptr.")

;;; Internal helpers

(defun jabber-omemo--device-list-key (account jid)
  "Return hash key for ACCOUNT and JID device list cache."
  (concat account "\0" jid))

(defun jabber-omemo--session-key (account jid device-id)
  "Return hash key for ACCOUNT, JID, DEVICE-ID session cache."
  (concat account "\0" jid "\0" (number-to-string device-id)))

(defun jabber-omemo--generate-device-id ()
  "Generate a random OMEMO device ID (1 to 2^31 - 1)."
  (1+ (random (1- (ash 1 31)))))

(defun jabber-omemo--get-store (jc)
  "Load or create the OMEMO store for connection JC.
Returns a deserialized store user-ptr, cached for future calls."
  (let ((account (jabber-connection-bare-jid jc)))
    (or (gethash account jabber-omemo--stores)
        (let* ((blob (jabber-omemo-store-load account))
               (store-ptr (if blob
                              (jabber-omemo-deserialize-store blob)
                            (let ((new-blob (jabber-omemo-setup-store)))
                              (jabber-omemo-store-save account new-blob)
                              (jabber-omemo-deserialize-store new-blob)))))
          (puthash account store-ptr jabber-omemo--stores)
          store-ptr))))

(defun jabber-omemo--get-device-id (jc)
  "Load or generate the OMEMO device ID for connection JC.
Returns an integer, cached for future calls."
  (let ((account (jabber-connection-bare-jid jc)))
    (or (gethash account jabber-omemo--device-ids)
        (let ((id (or (jabber-omemo-store-load-device-id account)
                      (let ((new-id (jabber-omemo--generate-device-id)))
                        (jabber-omemo-store-save-device-id account new-id)
                        new-id))))
          (puthash account id jabber-omemo--device-ids)
          id))))

(defun jabber-omemo--get-session (jc jid device-id)
  "Load session for JID's DEVICE-ID via connection JC.
Returns a deserialized session user-ptr, or nil."
  (let* ((account (jabber-connection-bare-jid jc))
         (key (jabber-omemo--session-key account jid device-id)))
    (or (gethash key jabber-omemo--sessions)
        (when-let* ((blob (jabber-omemo-store-load-session
                           account jid device-id)))
          (let ((session-ptr (jabber-omemo-deserialize-session blob)))
            (puthash key session-ptr jabber-omemo--sessions)
            session-ptr)))))

(defun jabber-omemo--save-session (jc jid device-id session-ptr)
  "Serialize and persist SESSION-PTR for JID's DEVICE-ID via JC.
Updates both the database and in-memory cache."
  (let* ((account (jabber-connection-bare-jid jc))
         (key (jabber-omemo--session-key account jid device-id))
         (blob (jabber-omemo-serialize-session session-ptr)))
    (jabber-omemo-store-save-session account jid device-id blob)
    (puthash key session-ptr jabber-omemo--sessions)))

;;; Device list XML helpers

(defun jabber-omemo--parse-device-list (items)
  "Parse PubSub ITEMS into a list of device ID integers.
ITEMS is a list of child elements from the PubSub <items> node.
Extracts <device id=\"N\"/> from the <list> element."
  (let (ids)
    (dolist (item items)
      (when (eq (jabber-xml-node-name item) 'item)
        (let ((list-el (car (jabber-xml-get-children item 'list))))
          (when list-el
            (dolist (dev (jabber-xml-get-children list-el 'device))
              (let ((id-str (jabber-xml-get-attribute dev 'id)))
                (when id-str
                  (push (string-to-number id-str) ids))))))))
    (nreverse ids)))

(defun jabber-omemo--build-device-list-xml (device-ids)
  "Build XML sexp for a device list containing DEVICE-IDS."
  `(list ((xmlns . ,jabber-omemo-xmlns))
         ,@(mapcar (lambda (id)
                     `(device ((id . ,(number-to-string id)))))
                   device-ids)))

;;; Bundle XML helpers

(defun jabber-omemo--build-bundle-xml (store-ptr)
  "Build XML sexp from STORE-PTR's bundle data.
Calls `jabber-omemo-get-bundle' and base64-encodes all keys."
  (let* ((bundle (jabber-omemo-get-bundle store-ptr))
         (ik (plist-get bundle :identity-key))
         (spk (plist-get bundle :signed-pre-key))
         (spk-id (plist-get bundle :signed-pre-key-id))
         (sig (plist-get bundle :signature))
         (pre-keys (plist-get bundle :pre-keys)))
    `(bundle ((xmlns . ,jabber-omemo-xmlns))
             (signedPreKeyPublic
              ((signedPreKeyId . ,(number-to-string spk-id)))
              ,(base64-encode-string spk t))
             (signedPreKeySignature ()
              ,(base64-encode-string sig t))
             (identityKey ()
              ,(base64-encode-string ik t))
             (prekeys ()
              ,@(mapcar (lambda (pk)
                          `(preKeyPublic
                            ((preKeyId . ,(number-to-string (car pk))))
                            ,(base64-encode-string (cdr pk) t)))
                        pre-keys)))))

(defun jabber-omemo--parse-bundle-xml (xml)
  "Parse bundle XML into a plist for session initiation.
XML is a <bundle> element sexp.  Returns
  (:signature BYTES :signed-pre-key BYTES :identity-key BYTES
   :signed-pre-key-id INT :pre-keys ((ID . BYTES) ...))
All key material is base64-decoded to unibyte strings."
  (let* ((spk-el (car (jabber-xml-get-children xml 'signedPreKeyPublic)))
         (sig-el (car (jabber-xml-get-children xml 'signedPreKeySignature)))
         (ik-el (car (jabber-xml-get-children xml 'identityKey)))
         (pks-el (car (jabber-xml-get-children xml 'prekeys)))
         (spk-id (string-to-number
                  (or (jabber-xml-get-attribute spk-el 'signedPreKeyId) "0")))
         (spk-data (base64-decode-string
                    (car (jabber-xml-node-children spk-el))))
         (sig-data (base64-decode-string
                    (car (jabber-xml-node-children sig-el))))
         (ik-data (base64-decode-string
                   (car (jabber-xml-node-children ik-el))))
         pre-keys)
    (dolist (pk (jabber-xml-get-children pks-el 'preKeyPublic))
      (let ((pk-id (string-to-number
                    (or (jabber-xml-get-attribute pk 'preKeyId) "0")))
            (pk-data (base64-decode-string
                      (car (jabber-xml-node-children pk)))))
        (push (cons pk-id pk-data) pre-keys)))
    (list :signature sig-data
          :signed-pre-key spk-data
          :identity-key ik-data
          :signed-pre-key-id spk-id
          :pre-keys (nreverse pre-keys))))

(provide 'jabber-omemo)
;;; jabber-omemo.el ends here
