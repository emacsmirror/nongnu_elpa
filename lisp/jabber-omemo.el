;;; jabber-omemo.el --- OMEMO encryption for jabber.el  -*- lexical-binding: t; -*-

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
(require 'jabber-hints)
(require 'jabber-omemo-trust)

(declare-function jabber-connection-bare-jid "jabber-util")
(declare-function jabber-jid-user "jabber-util")
(declare-function jabber-disco-advertise-feature "jabber-disco")
(declare-function jabber-send-iq "jabber-iq")
(declare-function jabber-send-sexp "jabber-core")
(declare-function jabber-chat--msg-plist-from-stanza "jabber-chat")
(declare-function jabber-maybe-print-rare-time "jabber-chat")
(declare-function jabber-httpupload--upload "jabber-httpupload")
(declare-function jabber-httpupload--send-url "jabber-httpupload")

(defvar jabber-post-connect-hooks)
(defvar jabber-pre-disconnect-hook)
(defvar jabber-pubsub-node-handlers)
(defvar jabber-chat-send-hooks)
(defvar jabber-chat-ewoc)
(defvar jabber-chatting-with)
(defvar jabber-chat-encryption)
(defvar jabber-chat-printers)
(defvar jabber-group)
(defvar jabber-muc-participants)

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

;; Declare internal C functions from the dynamic module for the byte-compiler.
;; "ext:" prefix tells check-declare to skip file verification.
(declare-function jabber-omemo--setup-store "ext:jabber-omemo-core")
(declare-function jabber-omemo--deserialize-store "ext:jabber-omemo-core")
(declare-function jabber-omemo--serialize-store "ext:jabber-omemo-core")
(declare-function jabber-omemo--get-bundle "ext:jabber-omemo-core")
(declare-function jabber-omemo--rotate-signed-pre-key "ext:jabber-omemo-core")
(declare-function jabber-omemo--refill-pre-keys "ext:jabber-omemo-core")
(declare-function jabber-omemo--encrypt-message "ext:jabber-omemo-core")
(declare-function jabber-omemo--decrypt-message "ext:jabber-omemo-core")
(declare-function jabber-omemo--make-session "ext:jabber-omemo-core")
(declare-function jabber-omemo--initiate-session "ext:jabber-omemo-core")
(declare-function jabber-omemo--serialize-session "ext:jabber-omemo-core")
(declare-function jabber-omemo--deserialize-session "ext:jabber-omemo-core")
(declare-function jabber-omemo--encrypt-key "ext:jabber-omemo-core")
(declare-function jabber-omemo--decrypt-key "ext:jabber-omemo-core")
(declare-function jabber-omemo--heartbeat "ext:jabber-omemo-core")
(declare-function jabber-omemo--aesgcm-decrypt "ext:jabber-omemo-core")
(declare-function jabber-omemo--aesgcm-encrypt "ext:jabber-omemo-core")

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

(defun jabber-omemo-aesgcm-decrypt (key iv ciphertext-with-tag)
  "Decrypt CIPHERTEXT-WITH-TAG using AES-256-GCM.
KEY is a 32-byte unibyte string, IV is a 12-byte unibyte string.
The last 16 bytes of CIPHERTEXT-WITH-TAG are the GCM auth tag."
  (jabber-omemo--aesgcm-decrypt key iv ciphertext-with-tag))

(defun jabber-omemo-aesgcm-encrypt (plaintext)
  "Encrypt PLAINTEXT using AES-256-GCM for aesgcm:// media sharing.
PLAINTEXT is a unibyte string.  Returns a plist
\(:key KEY :iv IV :ciphertext CIPHERTEXT-WITH-TAG)."
  (jabber-omemo--aesgcm-encrypt plaintext))

(defun jabber-omemo--build-aesgcm-url (https-url iv key)
  "Build an aesgcm:// URL from HTTPS-URL, IV, and KEY.
IV is a 12-byte unibyte string, KEY is a 32-byte unibyte string.
Returns a string like aesgcm://HOST/PATH#IVHEX_KEYHEX."
  (let ((fragment (concat (encode-hex-string iv)
                          (encode-hex-string key))))
    (concat "aesgcm://"
            (substring https-url (length "https://"))
            "#" fragment)))

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

(defconst jabber-omemo-fallback-body
  "I sent you an OMEMO encrypted message but your client doesn't seem to support that."
  "Plaintext fallback body for non-OMEMO clients.")

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

;;; Device list management

(defun jabber-omemo--fetch-device-list (jc jid callback)
  "Fetch the OMEMO device list for JID via connection JC.
On success, parse and call (funcall CALLBACK device-id-list).
Updates the in-memory cache and database."
  (jabber-pubsub-request
   jc jid jabber-omemo-devicelist-node
   (lambda (jc xml-data _closure)
     (let* ((pubsub (car (jabber-xml-get-children xml-data 'pubsub)))
            (items-node (car (jabber-xml-get-children pubsub 'items)))
            (items (jabber-xml-node-children items-node))
            (ids (jabber-omemo--parse-device-list items))
            (account (jabber-connection-bare-jid jc))
            (bare-jid (jabber-jid-user jid)))
       (puthash (jabber-omemo--device-list-key account bare-jid)
                ids jabber-omemo--device-lists)
       (dolist (id ids)
         (jabber-omemo-store-save-device account bare-jid id))
       (when callback
         (funcall callback ids))))
   (lambda (_jc _xml-data _closure)
     (when callback
       (funcall callback nil)))))

(defun jabber-omemo--publish-device-list (jc device-ids)
  "Publish DEVICE-IDS as our OMEMO device list via JC."
  (jabber-pubsub-publish
   jc nil jabber-omemo-devicelist-node "current"
   (jabber-omemo--build-device-list-xml device-ids)
   jabber-omemo--devicelist-publish-options))

(defun jabber-omemo--ensure-device-listed (jc)
  "Ensure our device ID is on our published device list via JC.
Fetches the current list, adds our ID if missing, re-publishes."
  (let ((our-id (jabber-omemo--get-device-id jc)))
    (jabber-omemo--fetch-device-list
     jc (jabber-connection-bare-jid jc)
     (lambda (ids)
       (unless (memq our-id ids)
         (jabber-omemo--publish-device-list
          jc (cons our-id (or ids '()))))))))

(defun jabber-omemo--handle-device-list (jc from _node items)
  "Handle incoming PubSub device list notification.
JC is the connection, FROM is the sender JID, ITEMS is the
list of child elements from the event."
  (let* ((account (jabber-connection-bare-jid jc))
         (bare-jid (jabber-jid-user from))
         (ids (jabber-omemo--parse-device-list items)))
    (puthash (jabber-omemo--device-list-key account bare-jid)
             ids jabber-omemo--device-lists)
    (dolist (id ids)
      (jabber-omemo-store-save-device account bare-jid id))))

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

;;; Bundle management

(defun jabber-omemo--publish-bundle (jc)
  "Publish our OMEMO bundle to PubSub via JC."
  (let* ((store-ptr (jabber-omemo--get-store jc))
         (device-id (jabber-omemo--get-device-id jc))
         (node (concat jabber-omemo-bundles-node-prefix
                       (number-to-string device-id))))
    (jabber-pubsub-publish
     jc nil node (number-to-string device-id)
     (jabber-omemo--build-bundle-xml store-ptr)
     jabber-omemo--bundle-publish-options)))

(defun jabber-omemo--fetch-bundle (jc jid device-id callback)
  "Fetch OMEMO bundle for JID's DEVICE-ID via JC.
On success, parse and call (funcall CALLBACK bundle-plist)
where bundle-plist has keys from `jabber-omemo--parse-bundle-xml'.
On error, calls (funcall CALLBACK nil)."
  (let ((node (concat jabber-omemo-bundles-node-prefix
                      (number-to-string device-id))))
    (jabber-pubsub-request
     jc jid node
     (lambda (_jc xml-data _closure)
       (let* ((pubsub (car (jabber-xml-get-children xml-data 'pubsub)))
              (items-node (car (jabber-xml-get-children pubsub 'items)))
              (item (car (jabber-xml-get-children items-node 'item)))
              (bundle-el (car (jabber-xml-get-children item 'bundle)))
              (parsed (when bundle-el
                        (jabber-omemo--parse-bundle-xml bundle-el))))
         (funcall callback parsed)))
     (lambda (_jc _xml-data _closure)
       (funcall callback nil)))))

;;; Session establishment

(defun jabber-omemo--establish-session (jc jid device-id bundle)
  "Establish an OMEMO session with JID's DEVICE-ID using BUNDLE.
BUNDLE is a plist from `jabber-omemo--parse-bundle-xml'.
Selects a random pre-key, initiates the session, saves to DB
and cache, and stores an undecided trust record (TOFU)."
  (let* ((store-ptr (jabber-omemo--get-store jc))
         (pre-keys (plist-get bundle :pre-keys))
         (pk (nth (random (length pre-keys)) pre-keys))
         (session-ptr (jabber-omemo-initiate-session
                       store-ptr
                       (plist-get bundle :signature)
                       (plist-get bundle :signed-pre-key)
                       (plist-get bundle :identity-key)
                       (cdr pk)
                       (plist-get bundle :signed-pre-key-id)
                       (car pk)))
         (account (jabber-connection-bare-jid jc)))
    (jabber-omemo--save-session jc jid device-id session-ptr)
    (jabber-omemo-store-save-trust account jid device-id
                                   (plist-get bundle :identity-key)
                                   0)
    session-ptr))

(defun jabber-omemo--load-device-list-from-db (account jid)
  "Load cached device IDs for ACCOUNT + JID from the database.
Returns a list of active device ID integers, or nil."
  (let ((records (jabber-omemo-store-load-devices account jid)))
    (mapcar (lambda (r) (plist-get r :device-id))
            (cl-remove-if-not (lambda (r) (plist-get r :active)) records))))

(defun jabber-omemo--ensure-sessions (jc jid callback)
  "Ensure sessions exist for all active devices of JID via JC.
Checks in-memory cache, then DB, then PubSub for the device list.
For each device lacking a session, fetches the bundle and establishes one.
Calls (funcall CALLBACK sessions) when done, where sessions is
a list of (DEVICE-ID . SESSION-PTR) for all active devices."
  (let* ((account (jabber-connection-bare-jid jc))
         (bare-jid (jabber-jid-user jid))
         (cache-key (jabber-omemo--device-list-key account bare-jid))
         (cached-ids (or (gethash cache-key jabber-omemo--device-lists)
                         (let ((db-ids (jabber-omemo--load-device-list-from-db
                                        account bare-jid)))
                           (when db-ids
                             (puthash cache-key db-ids
                                      jabber-omemo--device-lists))
                           db-ids))))
    (if cached-ids
        (jabber-omemo--ensure-sessions-for-ids jc bare-jid cached-ids callback)
      (jabber-omemo--fetch-device-list
       jc bare-jid
       (lambda (ids)
         (if ids
             (jabber-omemo--ensure-sessions-for-ids jc bare-jid ids callback)
           (funcall callback nil)))))))

(defun jabber-omemo--ensure-sessions-for-ids (jc jid device-ids callback)
  "Ensure sessions for DEVICE-IDS of JID via JC, then call CALLBACK.
CALLBACK receives a list of (DEVICE-ID . SESSION-PTR)."
  (let ((our-id (jabber-omemo--get-device-id jc))
        (pending 0)
        (results nil))
    (dolist (did device-ids)
      (unless (= did our-id)
        (let ((existing (jabber-omemo--get-session jc jid did)))
          (if existing
              (push (cons did existing) results)
            (cl-incf pending)
            (jabber-omemo--fetch-bundle
             jc jid did
             (lambda (bundle)
               (when bundle
                 (let ((session (jabber-omemo--establish-session
                                 jc jid did bundle)))
                   (push (cons did session) results)))
               (cl-decf pending)
               (when (zerop pending)
                 (funcall callback results))))))))
    (when (zerop pending)
      (funcall callback results))))

;;; Message encryption XML

(defun jabber-omemo--build-encrypted-xml (jc sessions enc-result)
  "Build <encrypted> XML sexp for an OMEMO 0.3 message.
JC is the Jabber connection (for our device ID).
SESSIONS is a list of (DEVICE-ID . SESSION-PTR) for all recipients
\(including our own other devices).
ENC-RESULT is the plist from `jabber-omemo-encrypt-message'."
  (let* ((our-sid (jabber-omemo--get-device-id jc))
         (key (plist-get enc-result :key))
         (iv (plist-get enc-result :iv))
         (ciphertext (plist-get enc-result :ciphertext))
         key-elements)
    (dolist (entry sessions)
      (let* ((did (car entry))
             (session-ptr (cdr entry))
             (encrypted-key (jabber-omemo-encrypt-key session-ptr key))
             (data (plist-get encrypted-key :data))
             (pre-key-p (plist-get encrypted-key :pre-key-p)))
        (push `(key ((rid . ,(number-to-string did))
                     ,@(when pre-key-p '((prekey . "true"))))
                    ,(base64-encode-string data t))
              key-elements)
        (jabber-omemo--save-session
         jc (jabber-jid-user (jabber-omemo--session-jid-for-did jc did))
         did session-ptr)))
    (jabber-omemo--persist-store jc)
    `(encrypted ((xmlns . ,jabber-omemo-xmlns))
                (header ((sid . ,(number-to-string our-sid)))
                        ,@(nreverse key-elements)
                        (iv () ,(base64-encode-string iv t)))
                (payload () ,(base64-encode-string ciphertext t)))))

(defun jabber-omemo--session-jid-for-did (jc device-id)
  "Look up the JID associated with DEVICE-ID in the session cache for JC.
Searches through `jabber-omemo--sessions' hash keys."
  (let ((account (jabber-connection-bare-jid jc))
        result)
    (maphash (lambda (key _val)
               (unless result
                 (let* ((parts (split-string key "\0"))
                        (acct (nth 0 parts))
                        (jid (nth 1 parts))
                        (did (string-to-number (nth 2 parts))))
                   (when (and (string= acct account)
                              (= did device-id))
                     (setq result jid)))))
             jabber-omemo--sessions)
    result))

;;; Message decryption XML

(defun jabber-omemo--parse-encrypted (xml-data)
  "Parse OMEMO <encrypted> element from XML-DATA.
Returns plist (:sid INT :iv BYTES :payload BYTES :keys ALIST)
where :keys is ((DEVICE-ID :data BYTES :pre-key-p BOOL) ...).
Returns nil if no <encrypted> element."
  (when-let* ((encrypted (jabber-xml-child-with-xmlns
                          xml-data jabber-omemo-xmlns)))
    (let* ((header (car (jabber-xml-get-children encrypted 'header)))
           (sid (string-to-number
                 (or (jabber-xml-get-attribute header 'sid) "0")))
           (iv-el (car (jabber-xml-get-children header 'iv)))
           (iv (base64-decode-string
                (car (jabber-xml-node-children iv-el))))
           (payload-el (car (jabber-xml-get-children encrypted 'payload)))
           (payload (when payload-el
                      (let ((text (car (jabber-xml-node-children payload-el))))
                        (when (and text (not (string-empty-p text)))
                          (base64-decode-string text)))))
           keys)
      (dolist (key-el (jabber-xml-get-children header 'key))
        (let ((rid (string-to-number
                    (or (jabber-xml-get-attribute key-el 'rid) "0")))
              (pre-key-p (equal (jabber-xml-get-attribute key-el 'prekey)
                                "true"))
              (data (base64-decode-string
                     (car (jabber-xml-node-children key-el)))))
          (push (list rid :data data :pre-key-p pre-key-p) keys)))
      (list :sid sid :iv iv :payload payload
            :keys (nreverse keys)))))

(defun jabber-omemo--persist-store (jc)
  "Serialize and save the OMEMO store for JC to the database."
  (let* ((account (jabber-connection-bare-jid jc))
         (store-ptr (gethash account jabber-omemo--stores)))
    (when store-ptr
      (jabber-omemo-store-save account
                               (jabber-omemo-serialize-store store-ptr)))))

;;; Receive path

(defun jabber-omemo--decrypt-stanza (jc xml-data parsed)
  "Decrypt OMEMO message in XML-DATA using PARSED data.
Returns modified XML-DATA with decrypted body, or nil on failure."
  (let* ((our-did (jabber-omemo--get-device-id jc))
         (account (jabber-connection-bare-jid jc))
         (from (jabber-xml-get-attribute xml-data 'from))
         (sender-jid (jabber-jid-user from))
         (sender-did (plist-get parsed :sid))
         (iv (plist-get parsed :iv))
         (payload (plist-get parsed :payload))
         (keys (plist-get parsed :keys))
         (our-key-entry (cl-find our-did keys :key #'car)))
    (unless our-key-entry
      (user-error "OMEMO message not encrypted for our device %d" our-did))
    (let* ((key-data (plist-get (cdr our-key-entry) :data))
           (pre-key-p (plist-get (cdr our-key-entry) :pre-key-p))
           (store-ptr (jabber-omemo--get-store jc))
           (session-ptr (or (jabber-omemo--get-session jc sender-jid sender-did)
                            (when pre-key-p
                              (jabber-omemo-make-session))))
           (decrypted-key (jabber-omemo-decrypt-key
                           session-ptr store-ptr pre-key-p key-data)))
      (jabber-omemo--save-session jc sender-jid sender-did session-ptr)
      (jabber-omemo--persist-store jc)
      (let ((trust (jabber-omemo-store-load-trust
                    account sender-jid sender-did)))
        (when (and trust (zerop (plist-get trust :trust)))
          (jabber-omemo-store-set-trust
           account sender-jid sender-did 1)))
      (when pre-key-p
        (when-let* ((hb (jabber-omemo-heartbeat session-ptr store-ptr)))
          (jabber-omemo--send-heartbeat jc sender-jid sender-did hb)))
      (if payload
          (let* ((plaintext (jabber-omemo-decrypt-message
                             decrypted-key iv payload))
                 (text (decode-coding-string plaintext 'utf-8))
                 (body-el (car (jabber-xml-get-children xml-data 'body))))
            (if body-el
                (setcar (cddr body-el) text)
              (nconc xml-data (list `(body () ,text))))
            xml-data)
        xml-data))))

(defun jabber-omemo--decrypt-if-needed (orig-fn jc xml-data)
  "Around advice for `jabber-chat--decrypt-if-needed'.
If XML-DATA contains an OMEMO <encrypted> element, decrypt it.
Otherwise delegate to ORIG-FN."
  (let ((parsed (jabber-omemo--parse-encrypted xml-data)))
    (if (null parsed)
        (funcall orig-fn jc xml-data)
      (condition-case err
          (jabber-omemo--decrypt-stanza jc xml-data parsed)
        (error
         (message "OMEMO decrypt failed: %s" (error-message-string err))
         (let ((body-el (car (jabber-xml-get-children xml-data 'body))))
           (if body-el
               (setcar (cddr body-el) "[OMEMO: could not decrypt]")
             (nconc xml-data (list '(body () "[OMEMO: could not decrypt]")))))
         xml-data)))))

(defun jabber-omemo--send-heartbeat (jc to device-id heartbeat-bytes)
  "Send OMEMO heartbeat (empty encrypted message, no payload).
JC is the connection.  TO is the recipient bare JID.
DEVICE-ID is the recipient's device.  HEARTBEAT-BYTES is the
encrypted key material to send."
  (let* ((our-sid (jabber-omemo--get-device-id jc))
         (iv (make-string 12 0))
         (stanza `(message ((to . ,to)
                            (type . "chat"))
                           (encrypted ((xmlns . ,jabber-omemo-xmlns))
                                      (header ((sid . ,(number-to-string our-sid)))
                                              (key ((rid . ,(number-to-string device-id)))
                                                   ,(base64-encode-string heartbeat-bytes t))
                                              (iv () ,(base64-encode-string iv t))))
                           ,(jabber-hints-store))))
    (jabber-send-sexp jc stanza)))

;;; MUC helpers

(defun jabber-omemo--muc-participant-jids (_group participants)
  "Return deduplicated list of bare JIDs for PARTICIPANTS.
PARTICIPANTS is the alist from `jabber-muc-participants'.
Entries without a real JID are excluded."
  (let (jids)
    (dolist (entry participants)
      (when-let* ((full-jid (plist-get (cdr entry) 'jid))
                  (bare (jabber-jid-user full-jid)))
        (unless (member bare jids)
          (push bare jids))))
    (nreverse jids)))

(defun jabber-omemo--ensure-sessions-multi (jc jids callback)
  "Ensure OMEMO sessions for all JIDS via JC.
Calls (funcall CALLBACK all-sessions) when done, where
all-sessions is a list of (DEVICE-ID . SESSION-PTR)."
  (if (null jids)
      (funcall callback nil)
    (let ((pending (length jids))
          (all-sessions nil))
      (dolist (jid jids)
        (jabber-omemo--ensure-sessions
         jc jid
         (lambda (sessions)
           (setq all-sessions (append sessions all-sessions))
           (cl-decf pending)
           (when (zerop pending)
             (funcall callback all-sessions))))))))

;;; Send path

(defvar-local jabber-omemo--pending-messages nil
  "Queue of messages awaiting session establishment.")

(defun jabber-omemo--send-chat (jc body)
  "Send BODY as OMEMO-encrypted message via JC.
Must be called from a chat buffer with `jabber-chatting-with' set."
  (let ((recipient (jabber-jid-user jabber-chatting-with)))
    (jabber-omemo--ensure-sessions
     jc recipient
     (lambda (recipient-sessions)
       (if (null recipient-sessions)
           (message "OMEMO: no sessions for %s, cannot send" recipient)
         (jabber-omemo--ensure-sessions
          jc (jabber-connection-bare-jid jc)
          (lambda (own-sessions)
            (jabber-omemo--send-encrypted
             jc body recipient
             (append recipient-sessions own-sessions)))))))))

(defun jabber-omemo--send-encrypted (jc body _recipient all-sessions)
  "Build and send an OMEMO-encrypted stanza.
JC is the connection.  BODY is the plaintext.  _RECIPIENT is the
bare JID (unused; `jabber-chatting-with' is used for addressing).
ALL-SESSIONS is a list of (DEVICE-ID . SESSION-PTR)
for recipient + own other devices."
  (let* ((plaintext (encode-coding-string body 'utf-8))
         (enc-result (jabber-omemo-encrypt-message plaintext))
         (encrypted-xml (jabber-omemo--build-encrypted-xml
                         jc all-sessions enc-result))
         (id (apply #'format "emacs-msg-%d.%d.%d" (current-time)))
         (stanza `(message ((to . ,jabber-chatting-with)
                            (type . "chat")
                            (id . ,id))
                           (body () ,jabber-omemo-fallback-body)
                           ,encrypted-xml
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

(defun jabber-omemo--send-muc (jc body)
  "Send BODY as OMEMO-encrypted groupchat message via JC.
Must be called from a MUC buffer with `jabber-group' set."
  (let* ((group jabber-group)
         (participants (cdr (assoc group jabber-muc-participants)))
         (bare-jids (jabber-omemo--muc-participant-jids group participants)))
    (if (null bare-jids)
        (user-error "OMEMO: no participant JIDs available (room may be anonymous)")
      (jabber-omemo--ensure-sessions-multi
       jc bare-jids
       (lambda (all-sessions)
         (jabber-omemo--ensure-sessions
          jc (jabber-connection-bare-jid jc)
          (lambda (own-sessions)
            (jabber-omemo--send-encrypted-muc
             jc body group
             (append all-sessions own-sessions)))))))))

(defun jabber-omemo--send-encrypted-muc (jc body group all-sessions)
  "Build and send an OMEMO-encrypted MUC stanza.
JC is the connection.  BODY is the plaintext.  GROUP is the room JID.
ALL-SESSIONS is a list of (DEVICE-ID . SESSION-PTR) for all
participants plus own other devices.
No local echo: the MUC server mirrors the message back."
  (let* ((plaintext (encode-coding-string body 'utf-8))
         (enc-result (jabber-omemo-encrypt-message plaintext))
         (encrypted-xml (jabber-omemo--build-encrypted-xml
                         jc all-sessions enc-result))
         (id (apply #'format "emacs-msg-%d.%d.%d" (current-time)))
         (stanza `(message ((to . ,group)
                            (type . "groupchat")
                            (id . ,id))
                           (body () ,jabber-omemo-fallback-body)
                           ,encrypted-xml
                           ,(jabber-hints-store))))
    (dolist (hook jabber-chat-send-hooks)
      (if (eq hook t)
          (when (local-variable-p 'jabber-chat-send-hooks)
            (dolist (global-hook (default-value 'jabber-chat-send-hooks))
              (nconc stanza (funcall global-hook body id))))
        (nconc stanza (funcall hook body id))))
    (jabber-send-sexp jc stanza)))

(defun jabber-omemo--prefetch-sessions (jc jid)
  "Pre-fetch OMEMO sessions for JID via JC in the background.
Called when OMEMO is enabled in a chat buffer."
  (jabber-omemo--ensure-sessions jc jid #'ignore))

(defun jabber-omemo--prefetch-muc-sessions (jc group)
  "Pre-fetch OMEMO sessions for all participants in GROUP via JC.
Called when OMEMO is enabled in a MUC buffer."
  (let* ((participants (cdr (assoc group jabber-muc-participants)))
         (bare-jids (jabber-omemo--muc-participant-jids group participants)))
    (when bare-jids
      (jabber-omemo--ensure-sessions-multi jc bare-jids #'ignore))))

;;; Trust and fingerprints

(defun jabber-omemo--format-fingerprint (identity-key)
  "Format IDENTITY-KEY as space-separated hex pairs."
  (mapconcat (lambda (byte) (format "%02X" byte))
             identity-key " "))

(defun jabber-omemo--trust-label (level)
  "Return a human-readable label for trust LEVEL."
  (pcase level
    (0 "undecided")
    (1 "TOFU")
    (2 "verified")
    (-1 "UNTRUSTED")
    (_ (format "unknown(%d)" level))))

(defun jabber-omemo-fingerprints ()
  "Display OMEMO trust management for the current chat peer.
Opens a tabulated-list buffer with interactive trust controls."
  (interactive)
  (unless (bound-and-true-p jabber-chatting-with)
    (user-error "Not in a chat buffer"))
  (jabber-omemo-show-trust jabber-buffer-connection jabber-chatting-with))

(defalias 'jabber-omemo-trust-device #'jabber-omemo-fingerprints)
(defalias 'jabber-omemo-untrust-device #'jabber-omemo-fingerprints)

;;; Connect/disconnect hooks

(defun jabber-omemo-on-connect (jc)
  "Post-connect hook for OMEMO initialization.
Loads or creates the store, ensures our device is listed,
publishes our bundle, and pre-fetches sessions for open chat buffers."
  (jabber-omemo--get-store jc)
  (jabber-omemo--get-device-id jc)
  (jabber-omemo--ensure-device-listed jc)
  (jabber-omemo--publish-bundle jc)
  (jabber-omemo--prefetch-open-chats jc))

(defun jabber-omemo--prefetch-open-chats (jc)
  "Pre-fetch OMEMO sessions for all open OMEMO chat buffers on JC."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (eq major-mode 'jabber-chat-mode)
                 (eq jabber-buffer-connection jc)
                 (eq jabber-chat-encryption 'omemo)
                 (bound-and-true-p jabber-chatting-with))
        (jabber-omemo--prefetch-sessions
         jc (jabber-jid-user jabber-chatting-with))))))

(defun jabber-omemo--on-disconnect ()
  "Pre-disconnect hook.  Clear OMEMO in-memory caches."
  (clrhash jabber-omemo--device-ids)
  (clrhash jabber-omemo--stores)
  (clrhash jabber-omemo--device-lists)
  (clrhash jabber-omemo--sessions))

;;; XEP-0454: aesgcm file upload

(defun jabber-omemo--httpupload-around (orig-fn jc filepath callback)
  "Advice around `jabber-httpupload--upload' for aesgcm encryption.
When the current buffer has OMEMO active, encrypt the file with
AES-256-GCM before uploading.  The CALLBACK receives an aesgcm://
URL instead of an https:// URL."
  (if (eq jabber-chat-encryption 'omemo)
      (condition-case err
          (let* ((plaintext (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally filepath)
                              (buffer-string)))
                 (enc (jabber-omemo-aesgcm-encrypt plaintext))
                 (key (plist-get enc :key))
                 (iv (plist-get enc :iv))
                 (ciphertext (plist-get enc :ciphertext))
                 (tmp (make-temp-file "jabber-aesgcm-" nil
                                      (file-name-extension filepath t))))
            (with-temp-file tmp
              (set-buffer-multibyte nil)
              (insert ciphertext))
            (funcall orig-fn jc tmp
                     (lambda (get-url)
                       (ignore-errors (delete-file tmp))
                       (funcall callback
                                (jabber-omemo--build-aesgcm-url
                                 get-url iv key)))))
        (error (message "aesgcm: file encryption failed: %s" (error-message-string err))))
    (funcall orig-fn jc filepath callback)))

(defun jabber-omemo--httpupload-send-url-around (orig-fn jc jid get-url)
  "Advice around `jabber-httpupload--send-url' for OMEMO.
When GET-URL is an aesgcm:// URL, send it as an OMEMO-encrypted
message instead of plaintext with OOB."
  (if (string-prefix-p "aesgcm://" get-url)
      (if (bound-and-true-p jabber-group)
          (jabber-omemo--send-muc jc get-url)
        (jabber-omemo--ensure-sessions
         jc (jabber-jid-user jid)
         (lambda (recipient-sessions)
           (jabber-omemo--ensure-sessions
            jc (jabber-connection-bare-jid jc)
            (lambda (own-sessions)
              (jabber-omemo--send-encrypted
               jc get-url jid
               (append recipient-sessions own-sessions)))))))
    (funcall orig-fn jc jid get-url)))

;;; Disco and PubSub registration

(jabber-disco-advertise-feature jabber-omemo-xmlns)

(with-eval-after-load "jabber-pubsub"
  (push (cons jabber-omemo-devicelist-node
              #'jabber-omemo--handle-device-list)
        jabber-pubsub-node-handlers))

(with-eval-after-load "jabber-core"
  (add-hook 'jabber-post-connect-hooks #'jabber-omemo-on-connect)
  (add-hook 'jabber-pre-disconnect-hook #'jabber-omemo--on-disconnect))

(advice-add 'jabber-httpupload--upload :around
            #'jabber-omemo--httpupload-around)

(advice-add 'jabber-httpupload--send-url :around
            #'jabber-omemo--httpupload-send-url-around)

(advice-add 'jabber-chat--decrypt-if-needed :around
            #'jabber-omemo--decrypt-if-needed '((depth . 10)))

(provide 'jabber-omemo)
;;; jabber-omemo.el ends here
