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
;; This file handles loading the native module and re-exports the core
;; functions under the public jabber-omemo- namespace.

;;; Code:

(require 'cl-lib)
(require 'hex-util)
(require 'jabber-util)
(require 'jabber-omemo-store)
(require 'jabber-pubsub)
(require 'jabber-xml)
(require 'jabber-hints)
(require 'jabber-eme)
(require 'jabber-chat)
(require 'jabber-db)
(require 'jabber-disco)
(require 'jabber-httpupload)
(require 'jabber-iq)
(require 'jabber-message-thread-protocol)
(require 'jabber-muc-state)

(declare-function jabber-muc-modify-participant "jabber-muc"
                  (group nickname new-plist))
(declare-function jabber-muc-participant-plist "jabber-muc"
                  (group nickname))
(declare-function jabber-muc-create-buffer "jabber-muc" (jc group))
(defvar jabber-muc--room-jids)

(defcustom jabber-omemo-enable t
  "Whether to enable OMEMO encryption support.
When nil, the native module is not loaded and OMEMO features are
disabled.  Set to nil if you do not have the build toolchain to
compile jabber-omemo-core."
  :type 'boolean
  :group 'jabber)

(defcustom jabber-omemo-signed-pre-key-rotation-period (* 7 86400)
  "Seconds between OMEMO signed pre-key rotations.
Checked on connect.  XEP-0384 recommends rotating once a week to
once a month.  The previous signed pre-key is retained for one
rotation, so in-flight pre-key messages still decrypt."
  :type 'integer
  :group 'jabber)

(defvar jabber-omemo--reconfigured-nodes (make-hash-table :test 'equal)
  "Nodes already reconfigured this session to prevent retry loops.")

(defvar jabber-post-connect-hooks)
(defvar jabber-pre-disconnect-hook)
(defvar jabber-pubsub-node-handlers)
(defvar jabber-chat-send-hooks)
(defvar jabber-chat-ewoc)
(defvar jabber-chatting-with)
(defvar jabber-chat-encryption)
(defvar jabber-chat-printers)
(defvar jabber-chat--decrypt-consumed-p)
(defvar jabber-group)
(defvar jabber-muc-participants)
(defvar jabber-httpupload-pre-upload-transform)
(defvar jabber-httpupload-send-url-function)
(defvar jabber-message-reply--id)       ; jabber-message-reply.el
(defvar jabber-message-reply--jid)      ; jabber-message-reply.el
(defvar jabber-message-reply--fallback-text) ; jabber-message-reply.el
(defvar jabber-message-reply--thread)   ; jabber-message-reply.el
(defvar jabber-message-thread-id)       ; jabber-message-thread.el
(defvar jabber-message-thread-parent-id) ; jabber-message-thread.el

(defvar jabber-omemo--available nil
  "Non-nil when the jabber-omemo-core native module is loaded.")

;; Module availability check.  Runs once at load time; `defvar' above
;; preserves `jabber-omemo--available' across repeated loads.
(unless (or jabber-omemo--available
            (not jabber-omemo-enable))
  (if (require 'jabber-omemo-core nil t)
      (setq jabber-omemo--available t)
    (setq jabber-omemo--available 'unavailable)
    (message "OMEMO: native module not found, encryption disabled")))

(defun jabber-omemo--require-module ()
  "Return non-nil if the native OMEMO module is available.
Signal a `user-error' otherwise."
  (if (eq jabber-omemo--available t)
      t
    (user-error "OMEMO module not compiled")))

;; Declare internal C functions from the dynamic module for the byte-compiler.
;; "ext:" prefix tells check-declare to skip file verification.
(declare-function jabber-omemo--setup-store "ext:jabber-omemo-core")
(declare-function jabber-omemo--deserialize-store "ext:jabber-omemo-core")
(declare-function jabber-omemo--serialize-store "ext:jabber-omemo-core")
(declare-function jabber-omemo--get-bundle "ext:jabber-omemo-core")
(declare-function jabber-omemo--rotate-signed-pre-key "ext:jabber-omemo-core")
(declare-function jabber-omemo--refill-pre-keys "ext:jabber-omemo-core")
(declare-function jabber-omemo--remove-pre-key "ext:jabber-omemo-core")
(declare-function jabber-omemo--used-pre-key-id "ext:jabber-omemo-core")
(declare-function jabber-omemo--encrypt-message "ext:jabber-omemo-core")
(declare-function jabber-omemo--decrypt-message "ext:jabber-omemo-core")
(declare-function jabber-omemo--make-session "ext:jabber-omemo-core")
(declare-function jabber-omemo--initiate-session "ext:jabber-omemo-core")
(declare-function jabber-omemo--serialize-session "ext:jabber-omemo-core")
(declare-function jabber-omemo--deserialize-session "ext:jabber-omemo-core")
(declare-function jabber-omemo--legacy-session-blob-p "ext:jabber-omemo-core")
(declare-function jabber-omemo--encrypt-key "ext:jabber-omemo-core")
(declare-function jabber-omemo--decrypt-key "ext:jabber-omemo-core")
(declare-function jabber-omemo--session-skipped-keys "ext:jabber-omemo-core")
(declare-function jabber-omemo--session-set-skipped-keys "ext:jabber-omemo-core")
(declare-function jabber-omemo--heartbeat "ext:jabber-omemo-core")
(declare-function jabber-omemo--aesgcm-decrypt "ext:jabber-omemo-core")
(declare-function jabber-omemo--aesgcm-encrypt "ext:jabber-omemo-core")

;;; Errors
;;
;; The C module defines `jabber-omemo-error' as the parent condition
;; on init.  We redefine it here so subtype declarations work even
;; when the native module is not available.

(define-error 'jabber-omemo-error "OMEMO error")

(define-error 'jabber-omemo-not-for-us
	      "OMEMO message not encrypted for this device" 'jabber-omemo-error)

(define-error 'jabber-omemo-no-session
	      "No OMEMO session with sender device" 'jabber-omemo-error)

(define-error 'jabber-omemo-prekey-failed
	      "OMEMO pre-key decryption failed" 'jabber-omemo-error)

;; Public API

(defun jabber-omemo-setup-store ()
  "Generate a new OMEMO device store.
Returns a serialized store as a unibyte string."
  (jabber-omemo--require-module)
  (jabber-omemo--setup-store))

(defun jabber-omemo-deserialize-store (blob)
  "Deserialize BLOB into an OMEMO store object.
Returns a user-ptr; freed automatically by GC."
  (jabber-omemo--require-module)
  (jabber-omemo--deserialize-store blob))

(defun jabber-omemo-serialize-store (store-ptr)
  "Serialize STORE-PTR back to a unibyte string."
  (jabber-omemo--require-module)
  (jabber-omemo--serialize-store store-ptr))

(defun jabber-omemo-get-bundle (store-ptr)
  "Extract the public bundle from STORE-PTR.
Returns a plist with keys :identity-key, :signed-pre-key,
:signed-pre-key-id, :signature, :pre-keys."
  (jabber-omemo--require-module)
  (jabber-omemo--get-bundle store-ptr))

(defun jabber-omemo-rotate-signed-pre-key (store-ptr)
  "Rotate the signed pre-key in STORE-PTR.
Mutates the store; caller must re-serialize."
  (jabber-omemo--require-module)
  (jabber-omemo--rotate-signed-pre-key store-ptr))

(defun jabber-omemo-remove-pre-key (store-ptr id)
  "Remove one-time pre-key ID from STORE-PTR.
Zeroes the matching slot per XEP-0384 one-time use;
`jabber-omemo-refill-pre-keys' regenerates zeroed slots.  The
store must be re-serialized afterwards.  Returns non-nil when a
slot was removed."
  (jabber-omemo--remove-pre-key store-ptr id))

(defun jabber-omemo-used-pre-key-id (session-ptr)
  "Return the one-time pre-key id consumed by SESSION-PTR.
Zero when the session never consumed a pre-key.  Only meaningful
right after a fresh session decrypted a pre-key message; the
value persists in the serialized session."
  (jabber-omemo--used-pre-key-id session-ptr))

(defun jabber-omemo-refill-pre-keys (store-ptr)
  "Refill removed pre-keys in STORE-PTR.
Mutates the store; caller must re-serialize."
  (jabber-omemo--require-module)
  (jabber-omemo--refill-pre-keys store-ptr))

(defun jabber-omemo-encrypt-message (plaintext)
  "Encrypt PLAINTEXT (a unibyte string) with OMEMO 0.3.
Returns a plist (:key KEY :iv IV :ciphertext CT),
all unibyte strings."
  (jabber-omemo--require-module)
  (jabber-omemo--encrypt-message plaintext))

(defun jabber-omemo-decrypt-message (key iv ciphertext)
  "Decrypt an OMEMO 0.3 message.
KEY is a unibyte string (>= 32 bytes: 16 AES key + auth tag).
IV is a 12-byte unibyte string.
CIPHERTEXT is the encrypted payload.
Returns the plaintext as a unibyte string."
  (jabber-omemo--require-module)
  (jabber-omemo--decrypt-message key iv ciphertext))

(defun jabber-omemo-make-session ()
  "Allocate an empty OMEMO session.
Returns a session user-ptr; freed automatically by GC.
Use for the receiving side of a pre-key message."
  (jabber-omemo--require-module)
  (jabber-omemo--make-session))

(defun jabber-omemo-initiate-session (store-ptr sig spk ik pk spk-id pk-id)
  "Initiate an OMEMO session with a remote device's bundle.
STORE-PTR is the local OMEMO store.
SIG is a 64-byte signature, SPK/IK/PK are 33-byte serialized keys.
SPK-ID and PK-ID are integer key IDs.
Returns a session user-ptr; freed automatically by GC."
  (jabber-omemo--require-module)
  (jabber-omemo--initiate-session store-ptr sig spk ik pk spk-id pk-id))

(defun jabber-omemo-serialize-session (session-ptr)
  "Serialize SESSION-PTR to a unibyte string."
  (jabber-omemo--require-module)
  (jabber-omemo--serialize-session session-ptr))

(defun jabber-omemo-deserialize-session (blob)
  "Deserialize BLOB into an OMEMO session object.
Returns a session user-ptr; freed automatically by GC."
  (jabber-omemo--require-module)
  (jabber-omemo--deserialize-session blob))

(defun jabber-omemo-encrypt-key (session-ptr key)
  "Encrypt KEY for a recipient using SESSION-PTR.
KEY is a unibyte string (the message encryption key).
Returns a plist (:data BYTES :pre-key-p BOOL)."
  (jabber-omemo--require-module)
  (jabber-omemo--encrypt-key session-ptr key))

(defun jabber-omemo-decrypt-key (session-ptr store-ptr pre-key-p msg)
  "Decrypt an encrypted key message.
SESSION-PTR is the session with the sender.
STORE-PTR is the local OMEMO store.
PRE-KEY-P is non-nil if this is a pre-key message.
MSG is the encrypted key message as a unibyte string.
Returns the decrypted key as a unibyte string."
  (jabber-omemo--require-module)
  (jabber-omemo--decrypt-key session-ptr store-ptr pre-key-p msg))

(defun jabber-omemo-heartbeat (session-ptr store-ptr)
  "Check if a heartbeat message is needed after decryption.
SESSION-PTR is the session to check.
STORE-PTR is the local OMEMO store.
Returns heartbeat message bytes or nil."
  (jabber-omemo--require-module)
  (jabber-omemo--heartbeat session-ptr store-ptr))

(defun jabber-omemo-aesgcm-decrypt (key iv ciphertext-with-tag)
  "Decrypt CIPHERTEXT-WITH-TAG using AES-256-GCM.
KEY is a 32-byte unibyte string, IV is a 12-byte unibyte string.
The last 16 bytes of CIPHERTEXT-WITH-TAG are the GCM auth tag."
  (jabber-omemo--require-module)
  (jabber-omemo--aesgcm-decrypt key iv ciphertext-with-tag))

(defun jabber-omemo-aesgcm-encrypt (plaintext)
  "Encrypt PLAINTEXT using AES-256-GCM for aesgcm:// media sharing.
PLAINTEXT is a unibyte string.  Returns a plist
\(:key KEY :iv IV :ciphertext CIPHERTEXT-WITH-TAG)."
  (jabber-omemo--require-module)
  (jabber-omemo--aesgcm-encrypt plaintext))

(defun jabber-omemo--build-aesgcm-url (https-url iv key)
  "Build an aesgcm:// URL from HTTPS-URL, IV, and KEY.
IV is a 12-byte unibyte string, KEY is a 32-byte unibyte string.
Returns a string like aesgcm://HOST/PATH#IVHEX_KEYHEX."
  (unless (string-prefix-p "https://" https-url)
    (error "Expected https:// URL, got: %s"
           (substring https-url 0 (min 40 (length https-url)))))
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
  "This message is encrypted with OMEMO and could not be displayed."
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

(defvar jabber-omemo--bundle-publishes-in-flight (make-hash-table :test 'equal)
  "Set of bundle publish requests currently in flight.
Keyed by \"BARE-JID:DEVICE-ID\".  Mirrors Dino's
`active_bundle_requests' to dedup concurrent self-bundle fetches.")

(defconst jabber-omemo--prekey-min-count 100
  "Minimum number of pre-keys our published bundle should advertise.
Below this we refill locally and republish.  Matches picomemo's
`OMEMO_NUMPREKEYS' refill target and Dino's `NUM_KEYS_TO_PUBLISH',
so any drift between local and published state triggers a republish.")

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
            (when (jabber-omemo--legacy-session-blob-p blob)
              (jabber-omemo--session-set-skipped-keys
               session-ptr
               (jabber-omemo-store-all-skipped-keys
                account jid device-id)))
            (puthash key session-ptr jabber-omemo--sessions)
            session-ptr)))))

(defun jabber-omemo--save-session (jc jid device-id session-ptr)
  "Serialize and persist SESSION-PTR for JID's DEVICE-ID via JC.
Updates both the database and in-memory cache."
  (let* ((account (jabber-connection-bare-jid jc))
         (key (jabber-omemo--session-key account jid device-id))
         (blob (jabber-omemo-serialize-session session-ptr)))
    (jabber-omemo-store-save-session-and-clear-legacy-keys
     account jid device-id blob)
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

(defun jabber-omemo--deactivate-stale-devices (account jid current-ids)
  "Mark devices for ACCOUNT+JID not in CURRENT-IDS as inactive."
  (dolist (rec (jabber-omemo-store-load-devices account jid))
    (let ((did (plist-get rec :device-id)))
      (when (and (plist-get rec :active)
                 (not (memq did current-ids)))
        (jabber-omemo-store-set-device-active account jid did nil)))))

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
       (jabber-omemo--deactivate-stale-devices account bare-jid ids)
       (when callback
         (funcall callback ids))))
   (lambda (_jc xml-data _closure)
     (message "jabber-omemo: failed to fetch device list for %s: %s"
              jid (jabber-parse-error
                   (jabber-iq-error xml-data)))
     (when callback
       (funcall callback nil)))))

(defun jabber-omemo--handle-publish-conflict (jc node item-id payload
                                                 options xml-data label)
  "Handle a PubSub publish error for LABEL.
If the error is a publish-options conflict, retry without options
and reconfigure the node.  Otherwise just warn.
JC is the connection, NODE and ITEM-ID identify the item,
PAYLOAD is the XML to publish, OPTIONS is the original
publish-options alist, and XML-DATA is the error IQ stanza."
  (let* ((err (jabber-iq-error xml-data))
         (condition (and err (jabber-error-condition err))))
    (if (eq condition 'conflict)
        (if (gethash node jabber-omemo--reconfigured-nodes)
            (warn "jabber-omemo: giving up on %s (already reconfigured)" label)
          (puthash node t jabber-omemo--reconfigured-nodes)
          (message "OMEMO: publish-options conflict for %s, retrying" label)
          (jabber-pubsub-publish
           jc nil node item-id payload nil #'ignore
           (lambda (_jc xml-data2 _closure)
             (warn "jabber-omemo: failed to publish %s (retry): %s"
                   label (jabber-parse-error
                          (jabber-iq-error xml-data2)))))
          (jabber-pubsub-configure-node
           jc nil node options nil
           (lambda (_jc xml-data2 _closure)
             (warn "jabber-omemo: failed to reconfigure %s node: %s"
                   label (jabber-parse-error
                          (jabber-iq-error xml-data2))))))
      (warn "jabber-omemo: failed to publish %s: %s"
            label (if err (jabber-parse-error err) "unknown error")))))

(defun jabber-omemo--publish-device-list (jc device-ids)
  "Publish DEVICE-IDS as our OMEMO device list via JC."
  (let ((payload (jabber-omemo--build-device-list-xml device-ids))
        (node jabber-omemo-devicelist-node))
    (jabber-pubsub-publish
     jc nil node "current" payload
     jabber-omemo--devicelist-publish-options
     #'ignore
     (lambda (_jc xml-data _closure)
       (jabber-omemo--handle-publish-conflict
        jc node "current" payload
        jabber-omemo--devicelist-publish-options
        xml-data "device list")))))

(defun jabber-omemo--ensure-device-listed (jc)
  "Ensure our device ID is on our published device list via JC.
Fetches the current list, adds our ID if missing, re-publishes.
When our ID was missing (new installation), also checks other
listed devices for stale copies sharing our identity key and
removes them."
  (let ((our-id (jabber-omemo--get-device-id jc)))
    (jabber-omemo--fetch-device-list
     jc (jabber-connection-bare-jid jc)
     (lambda (ids)
       (if (memq our-id ids)
           ;; Already listed, nothing to do.
           nil
         (jabber-omemo--publish-device-list
          jc (cons our-id (or ids '())))
         ;; New installation: check for stale devices with our key.
         (jabber-omemo--cleanup-stale-devices jc ids))))))

(defun jabber-omemo--cleanup-stale-devices (jc other-ids)
  "Remove devices from OTHER-IDS that share our identity key.
JC is the Jabber connection.  Fetches the bundle for each device
in OTHER-IDS, collects stale device IDs, then removes them all in
a single device list republish to avoid race conditions."
  (let* ((store (jabber-omemo--get-store jc))
         (our-bundle (jabber-omemo-get-bundle store))
         (our-ik (plist-get our-bundle :identity-key))
         (own-jid (jabber-connection-bare-jid jc))
         (remaining (length other-ids))
         (stale nil))
    (if (zerop remaining)
        nil
      (dolist (did other-ids)
        (jabber-omemo--fetch-bundle
         jc own-jid did
         (let ((did did))
           (lambda (bundle)
             (when-let* ((ik (and bundle (plist-get bundle :identity-key)))
                         ((string= ik our-ik)))
               (push did stale))
             (cl-decf remaining)
             (when (zerop remaining)
               (jabber-omemo--remove-stale-devices jc stale)))))))))


(defun jabber-omemo--remove-stale-devices (jc stale-ids)
  "Remove STALE-IDS from the device list and delete their bundles.
JC is the Jabber connection.  Does a single fetch-filter-republish
for all stale devices, then deletes each bundle node."
  (when stale-ids
    (message "OMEMO: removing %d stale device(s): %s"
             (length stale-ids) stale-ids)
    (jabber-omemo--fetch-device-list
     jc (jabber-connection-bare-jid jc)
     (lambda (ids)
       (let ((new-ids (cl-remove-if (lambda (id) (memq id stale-ids)) ids)))
         (jabber-omemo--publish-device-list jc new-ids)
         (dolist (did stale-ids)
           (jabber-omemo--delete-bundle-node jc did)))))))

(defun jabber-omemo--delete-bundle-node (jc device-id)
  "Delete the bundle PubSub node for DEVICE-ID via JC."
  (jabber-pubsub-delete-node
   jc nil
   (concat jabber-omemo-bundles-node-prefix (number-to-string device-id))
   nil
   (lambda (_jc xml _closure)
     (message "OMEMO: failed to delete bundle for %d: %s"
              device-id (jabber-xml-path xml '(error))))))

(defun jabber-omemo--remove-device (jc device-id &optional callback)
  "Remove DEVICE-ID from JC's published device list and delete its bundle.
Fetches the current list, filters out DEVICE-ID, re-publishes,
then deletes the bundle PubSub node.  Calls CALLBACK when done."
  (jabber-omemo--fetch-device-list
   jc (jabber-connection-bare-jid jc)
   (lambda (ids)
     (let ((new-ids (cl-remove device-id ids)))
       (jabber-omemo--publish-device-list jc new-ids)
       (message "OMEMO: republished device list without %d (%d -> %d devices)"
                device-id (length ids) (length new-ids)))
     (jabber-pubsub-delete-node
      jc nil (concat jabber-omemo-bundles-node-prefix
                     (number-to-string device-id))
      (when callback
        (lambda (_jc _xml _closure) (funcall callback)))
      (lambda (_jc xml _closure)
        (message "OMEMO: failed to delete bundle for %d: %s"
                 device-id (jabber-xml-path xml '(error))))))))

(defun jabber-omemo--handle-device-list (jc from _node items)
  "Handle incoming PubSub device list notification.
JC is the connection, FROM is the sender JID, ITEMS is the
list of child elements from the event.  When our own device is
missing from our device list, re-add and re-publish."
  (let* ((account (jabber-connection-bare-jid jc))
         (bare-jid (jabber-jid-user from))
         (ids (jabber-omemo--parse-device-list items)))
    (when (string= bare-jid account)
      (let ((our-id (jabber-omemo--get-device-id jc)))
        (unless (memq our-id ids)
          (message "OMEMO: own device %d dropped from device list, re-adding"
                   our-id)
          (setq ids (cons our-id ids))
          (jabber-omemo--publish-device-list jc ids))))
    (when (string= bare-jid account)
      (jabber-omemo--publish-bundle-if-needed jc))
    (puthash (jabber-omemo--device-list-key account bare-jid)
             ids jabber-omemo--device-lists)
    (dolist (id ids)
      (jabber-omemo-store-save-device account bare-jid id))
    (jabber-omemo--deactivate-stale-devices account bare-jid ids)))

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
All key material is base64-decoded to unibyte strings.
Returns nil if any required element is missing or empty."
  (let* ((spk-el (car (jabber-xml-get-children xml 'signedPreKeyPublic)))
         (sig-el (car (jabber-xml-get-children xml 'signedPreKeySignature)))
         (ik-el (car (jabber-xml-get-children xml 'identityKey)))
         (pks-el (car (jabber-xml-get-children xml 'prekeys)))
         (spk-text (car (jabber-xml-node-children spk-el)))
         (sig-text (car (jabber-xml-node-children sig-el)))
         (ik-text (car (jabber-xml-node-children ik-el))))
    (if (not (and (stringp spk-text) (stringp sig-text) (stringp ik-text)))
        (progn
          (message "jabber-omemo: malformed bundle XML (missing key data)")
          nil)
      (let ((spk-id (string-to-number
                     (or (jabber-xml-get-attribute spk-el 'signedPreKeyId) "0")))
            (spk-data (base64-decode-string spk-text))
            (sig-data (base64-decode-string sig-text))
            (ik-data (base64-decode-string ik-text))
            pre-keys)
        (dolist (pk (jabber-xml-get-children pks-el 'preKeyPublic))
          (let ((pk-text (car (jabber-xml-node-children pk))))
            (when (stringp pk-text)
              (let ((pk-id (string-to-number
                            (or (jabber-xml-get-attribute pk 'preKeyId) "0")))
                    (pk-data (base64-decode-string pk-text)))
                (push (cons pk-id pk-data) pre-keys)))))
        (list :signature sig-data
              :signed-pre-key spk-data
              :identity-key ik-data
              :signed-pre-key-id spk-id
              :pre-keys (nreverse pre-keys))))))

;;; Bundle management

(defun jabber-omemo--publish-bundle (jc)
  "Publish our OMEMO bundle to PubSub via JC."
  (let* ((store-ptr (jabber-omemo--get-store jc))
         (device-id (jabber-omemo--get-device-id jc))
         (node (concat jabber-omemo-bundles-node-prefix
                       (number-to-string device-id))))
    (let ((payload (jabber-omemo--build-bundle-xml store-ptr))
          (item-id (number-to-string device-id)))
      (jabber-pubsub-publish
       jc nil node item-id payload
       jabber-omemo--bundle-publish-options
       #'ignore
       (lambda (_jc xml-data _closure)
         (jabber-omemo--handle-publish-conflict
          jc node item-id payload
          jabber-omemo--bundle-publish-options
          xml-data (format "bundle for device %d" device-id)))))))

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
     (lambda (_jc xml-data _closure)
       (warn "jabber-omemo: failed to fetch bundle for %s device %d: %s"
             jid device-id
             (jabber-parse-error
              (jabber-iq-error xml-data)))
       (funcall callback nil)))))

(defun jabber-omemo--bundle-needs-republish-p (local published)
  "Return non-nil if PUBLISHED bundle is out of date vs LOCAL.
Both arguments are bundle plists (see `jabber-omemo-get-bundle'
and `jabber-omemo--parse-bundle-xml').  PUBLISHED may be nil
when no bundle is published yet.

The pre-key drift check assumes the server prunes consumed
pre-keys from the published bundle per XEP-0384 Section 4.3;
Prosody, ejabberd, MongooseIM, Tigase and Openfire all do.
Against a non-compliant server that never prunes, rotation of
pre-key ids without a size drop will be silently missed.  Dino's
structural intersection in `stream_module.vala:254-273' catches
that edge case; matching it would require plumbing the local
pre-key set into this predicate."
  (or (null published)
      (not (equal (plist-get local :identity-key)
                  (plist-get published :identity-key)))
      (not (equal (plist-get local :signed-pre-key-id)
                  (plist-get published :signed-pre-key-id)))
      (not (equal (plist-get local :signed-pre-key)
                  (plist-get published :signed-pre-key)))
      (< (length (plist-get published :pre-keys))
         jabber-omemo--prekey-min-count)))

(defun jabber-omemo--publish-bundle-if-needed (jc)
  "Fetch our published bundle and republish only if out of date.
Dedups concurrent calls per JC via
`jabber-omemo--bundle-publishes-in-flight'."
  (let* ((bare-jid (jabber-connection-bare-jid jc))
         (device-id (jabber-omemo--get-device-id jc))
         (key (format "%s:%d" bare-jid device-id)))
    (unless (gethash key jabber-omemo--bundle-publishes-in-flight)
      (puthash key t jabber-omemo--bundle-publishes-in-flight)
      (jabber-omemo--fetch-bundle
       jc bare-jid device-id
       (lambda (published)
         (unwind-protect
             (let* ((store-ptr (jabber-omemo--get-store jc))
                    (local (jabber-omemo-get-bundle store-ptr)))
               (when (jabber-omemo--bundle-needs-republish-p local published)
                 (message "OMEMO: republishing bundle (out of date)")
                 (jabber-omemo-refill-pre-keys store-ptr)
                 (jabber-omemo--persist-store jc)
                 (jabber-omemo--publish-bundle jc)))
           (remhash key jabber-omemo--bundle-publishes-in-flight)))))))

;;; One-time pre-key removal (XEP-0384 section 4.3)

(defvar jabber-omemo--pending-prekey-removals (make-hash-table :test #'equal)
  "Account to list of consumed one-time pre-key ids awaiting removal.
Filled after a fresh-session pre-key decrypt.  Removal is
deferred until MAM catchup completes so a repeated or corrected
pre-key message from the same catchup still decrypts; the
established session and the decrypt dedup cache cover the live
window in the meantime.")

(defvar jabber-omemo--prekey-flush-timer nil
  "Debounce timer for `jabber-omemo--flush-prekey-removals'.")

(defconst jabber-omemo--prekey-flush-delay 30
  "Seconds to wait before removing consumed pre-keys.
Covers accounts without MAM: long enough for an offline-push
duplicate of the pre-key message to arrive first.")

(defvar jabber-omemo--prekey-exports-warned nil
  "Non-nil after warning once about a stale native module.")

(defun jabber-omemo--prekey-exports-p ()
  "Return non-nil when the native module has the pre-key exports.
Warn once when it does not (stale jabber-omemo-core.so)."
  (or (and (fboundp 'jabber-omemo--used-pre-key-id)
           (fboundp 'jabber-omemo--remove-pre-key))
      (prog1 nil
        (unless jabber-omemo--prekey-exports-warned
          (setq jabber-omemo--prekey-exports-warned t)
          (message "OMEMO: jabber-omemo-core.so predates pre-key removal; \
run `make module' to rebuild")))))

(defun jabber-omemo--mam-syncing-p ()
  "Return non-nil when a MAM catchup is in progress."
  (and (fboundp 'jabber-mam-syncing-p) (jabber-mam-syncing-p)))

(defun jabber-omemo--note-consumed-prekey (jc session-ptr)
  "Record SESSION-PTR's consumed one-time pre-key for later removal.
Called after a fresh-session pre-key decrypt on JC; the reuse
path must not call this, since `usedpk_id' persists in serialized
sessions.  Schedules a debounced flush."
  (when (jabber-omemo--prekey-exports-p)
    (let ((id (jabber-omemo-used-pre-key-id session-ptr))
          (account (jabber-connection-bare-jid jc)))
      (when (> id 0)
        (cl-pushnew id (gethash account
                                jabber-omemo--pending-prekey-removals))
        (jabber-omemo--schedule-prekey-flush jc)))))

(defun jabber-omemo--schedule-prekey-flush (jc)
  "Restart the debounced pre-key removal flush for JC."
  (when (timerp jabber-omemo--prekey-flush-timer)
    (cancel-timer jabber-omemo--prekey-flush-timer))
  (setq jabber-omemo--prekey-flush-timer
        (run-with-timer jabber-omemo--prekey-flush-delay nil
                        #'jabber-omemo--flush-prekey-removals jc)))

(defun jabber-omemo--flush-prekey-removals (jc)
  "Remove consumed one-time pre-keys for JC's account and republish.
No-op while a MAM sync is running (the sync-complete hook retries)
or when nothing is pending.  Removes each pending pre-key from the
store, refills, persists, and republishes the bundle; the drift
check in `jabber-omemo--bundle-needs-republish-p' cannot see an
id-level replacement, so the publish is unconditional."
  (let* ((account (jabber-connection-bare-jid jc))
         (ids (gethash account jabber-omemo--pending-prekey-removals)))
    (when (and ids
               (not (jabber-omemo--mam-syncing-p))
               (jabber-omemo--prekey-exports-p))
      (remhash account jabber-omemo--pending-prekey-removals)
      (let ((store-ptr (jabber-omemo--get-store jc)))
        (dolist (id ids)
          (jabber-omemo-remove-pre-key store-ptr id))
        (jabber-omemo-refill-pre-keys store-ptr)
        (jabber-omemo--persist-store jc)
        (message "OMEMO: removed %d consumed pre-key(s)" (length ids))
        (jabber-omemo--publish-bundle jc)))))

(defun jabber-omemo--on-mam-sync-complete (_peers)
  "Flush pending pre-key removals once MAM catchup has finished."
  (unless (jabber-omemo--mam-syncing-p)
    (dolist (jc jabber-connections)
      (jabber-omemo--flush-prekey-removals jc))))

;;; Session establishment

(defun jabber-omemo--establish-session (jc jid device-id bundle)
  "Establish an OMEMO session on JC with JID's DEVICE-ID using BUNDLE.
BUNDLE is a plist from `jabber-omemo--parse-bundle-xml'.
Selects a random pre-key, initiates the session, saves to DB
and cache, and stores an undecided trust record (TOFU)."
  (let* ((store-ptr (jabber-omemo--get-store jc))
         (pre-keys (plist-get bundle :pre-keys))
         (signed-pre-key (plist-get bundle :signed-pre-key))
         (identity-key (plist-get bundle :identity-key))
         (signed-pre-key-id (plist-get bundle :signed-pre-key-id)))
    (unless (and pre-keys signed-pre-key identity-key signed-pre-key-id)
      (user-error "OMEMO: incomplete bundle for %s device %d (missing %s)"
                  jid device-id
                  (string-join
                   (delq nil
                         (list (unless pre-keys "pre-keys")
                               (unless signed-pre-key "signed-pre-key")
                               (unless identity-key "identity-key")
                               (unless signed-pre-key-id "signed-pre-key-id")))
                   ", ")))
    (let* ((pk (nth (random (length pre-keys)) pre-keys))
           (session-ptr (jabber-omemo-initiate-session
                         store-ptr
                         (plist-get bundle :signature)
                         signed-pre-key
                         identity-key
                         (cdr pk)
                         signed-pre-key-id
                         (car pk)))
           (account (jabber-connection-bare-jid jc)))
      (jabber-omemo--save-session jc jid device-id session-ptr)
      (jabber-omemo-store-save-trust account jid device-id
                                     identity-key 0)
      session-ptr)))

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

(defun jabber-omemo--trusted-sessions (jc sessions)
  "Filter SESSIONS to exclude devices marked untrusted via JC.
SESSIONS is a list of (DEVICE-ID . SESSION-PTR).
Returns the filtered list, dropping any device with trust = -1."
  (let ((account (jabber-connection-bare-jid jc)))
    (cl-remove-if
     (lambda (entry)
       (let* ((did (car entry))
              (jid (jabber-omemo--session-jid-for-did jc did))
              (trust-rec (and jid (jabber-omemo-store-load-trust
                                   account jid did))))
         (and trust-rec (= (plist-get trust-rec :trust) -1))))
     sessions)))

(defun jabber-omemo--build-encrypted-xml (jc sessions enc-result)
  "Build <encrypted> XML sexp for an OMEMO 0.3 message.
JC is the Jabber connection (for our device ID).
SESSIONS is a list of (DEVICE-ID . SESSION-PTR) for all recipients
\(including our own other devices).
ENC-RESULT is the plist from `jabber-omemo-encrypt-message'."
  (setq sessions (jabber-omemo--trusted-sessions jc sessions))
  (unless sessions
    (user-error "OMEMO: no trusted devices for any recipient"))
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

(defun jabber-omemo--match-jid-by-affiliation (group nick)
  "Try to match NICK in GROUP to a bare JID from affiliation data.
Finds JIDs in `jabber-muc--room-jids' not yet assigned to any
participant.  If exactly one unassigned JID exists, return it
and store the mapping for future lookups."
  (when-let* ((room-jids (gethash group jabber-muc--room-jids)))
    (let* ((participants (cdr (assoc group jabber-muc-participants)))
           (assigned (make-hash-table :test #'equal)))
      (dolist (entry participants)
        (when-let* ((jid (plist-get (cdr entry) 'jid)))
          (puthash (jabber-jid-user jid) t assigned)))
      (let (candidates)
        (maphash (lambda (bare-jid _aff)
                   (unless (gethash bare-jid assigned)
                     (push bare-jid candidates)))
                 room-jids)
        (when (= (length candidates) 1)
          (let ((jid (car candidates)))
            (jabber-muc-modify-participant
             group nick (list 'jid jid))
            jid))))))

(defun jabber-omemo--resolve-sender-jid (xml-data)
  "Return the real bare JID of the sender of XML-DATA.
For 1:1 messages, this is `jabber-jid-user' of the from attribute.
For MUC messages (type=groupchat), try in order:
1. Nickname lookup in `jabber-muc-participants'
2. Match by affiliation between participants and `jabber-muc--room-jids'"
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
         (msg-type (jabber-xml-get-attribute xml-data 'type)))
    (if (not (equal msg-type "groupchat"))
        (and from (jabber-jid-user from))
      (let* ((group (jabber-jid-user from))
             (nick (jabber-jid-resource from))
             (plist (jabber-muc-participant-plist group nick))
             (real-jid (plist-get plist 'jid)))
        (or (and real-jid (jabber-jid-user real-jid))
            (jabber-omemo--match-jid-by-affiliation group nick))))))

(defun jabber-omemo--decrypt-key-with-session (jc sender-jid sender-did
                                                  store-ptr pre-key-p key-data)
  "Decrypt KEY-DATA from SENDER-JID's device SENDER-DID via JC.
STORE-PTR is the local OMEMO store.  For a pre-key message
\(PRE-KEY-P non-nil) an existing session is tried first: an
established ratchet must not re-run the pre-key handshake, or a
repeated pre-key message (offline edit, replay) would consume the
pre-key twice.  When that attempt fails, fall back to a fresh
session; picomemo restores session state on a failed decrypt, so
the retry is safe.  This also resolves a peer that reset their
session and simultaneous initiations.  A regular message requires
an existing session.

Skipped ratchet keys are owned by the native session and persisted
inside its serialized blob, so out-of-order messages survive restarts.

Returns (SESSION-PTR DECRYPTED-KEY FRESH-P), FRESH-P non-nil when
the fresh-session pre-key path was used.  Signals
`jabber-omemo-no-session' or `jabber-omemo-prekey-failed'."
  (let ((existing (jabber-omemo--get-session jc sender-jid sender-did)))
    (cl-flet ((decrypt-existing (prekey)
                (let ((key (jabber-omemo-decrypt-key
                            existing store-ptr prekey key-data)))
                  (list existing key nil))))
      (cond
       ((not pre-key-p)
        (unless existing
          (signal 'jabber-omemo-no-session (list sender-jid sender-did)))
        (decrypt-existing nil))
       (t
        (or (and existing
                 (condition-case nil
                     (decrypt-existing t)
                   (jabber-omemo-error nil)))
            (let ((fresh (jabber-omemo-make-session)))
              (condition-case err
                  (let ((key (jabber-omemo-decrypt-key
                              fresh store-ptr t key-data)))
                    (list fresh key t))
                (jabber-omemo-error
                 (signal 'jabber-omemo-prekey-failed
                         (list sender-jid sender-did
                               (error-message-string err))))))))))))

(defun jabber-omemo--decrypt-stanza (jc xml-data parsed)
  "Decrypt OMEMO message on JC in XML-DATA using PARSED data.
Returns modified XML-DATA with decrypted body.

Signals structured errors that callers can dispatch on:
- `jabber-omemo-not-for-us' when the stanza has no key entry for
  our device (heartbeat or message addressed to a different device).
- `jabber-omemo-no-session' for a non-prekey message when we have
  no local session with the sender's device.
- `jabber-omemo-prekey-failed' when the C decrypt fails on a
  pre-key message on both the existing-session and fresh-session
  paths (usually a stale local pre-key).
- `jabber-omemo-error' (the parent) for all other crypto failures."
  (let* ((our-did (jabber-omemo--get-device-id jc))
         (account (jabber-connection-bare-jid jc))
         (sender-jid (jabber-omemo--resolve-sender-jid xml-data)))
    (if (not sender-jid)
        (error "Sender JID unknown (anonymous room?)")
      (let* ((sender-did (plist-get parsed :sid))
             (iv (plist-get parsed :iv))
             (payload (plist-get parsed :payload))
             (keys (plist-get parsed :keys))
             (our-key-entry (cl-find our-did keys :key #'car)))
	(unless our-key-entry
          (signal 'jabber-omemo-not-for-us (list our-did)))
	(pcase-let* ((key-data (plist-get (cdr our-key-entry) :data))
                     (pre-key-p (plist-get (cdr our-key-entry) :pre-key-p))
                     (store-ptr (jabber-omemo--get-store jc))
                     (`(,session-ptr ,decrypted-key ,fresh-p)
                      (jabber-omemo--decrypt-key-with-session
                       jc sender-jid sender-did store-ptr
                       pre-key-p key-data)))
          (setq jabber-chat--decrypt-consumed-p t)
          (jabber-omemo--save-session jc sender-jid sender-did session-ptr)
          (jabber-omemo--persist-store jc)
          (when fresh-p
            (jabber-omemo--note-consumed-prekey jc session-ptr))
          (let ((trust (jabber-omemo-store-load-trust
			account sender-jid sender-did)))
            (when (and trust (zerop (plist-get trust :trust)))
              (jabber-omemo-store-set-trust
               account sender-jid sender-did 1)
              (message "%s auto-trusted device %d for %s (TOFU)"
                       (propertize "OMEMO:" 'face 'warning)
                       sender-did sender-jid)))
          (when-let* ((hb (jabber-omemo-heartbeat session-ptr store-ptr)))
            (jabber-omemo--send-heartbeat jc sender-jid sender-did hb))
          (if payload
              (let* ((plaintext (jabber-omemo-decrypt-message
				 decrypted-key iv payload))
                     (text (decode-coding-string plaintext 'utf-8)))
		(jabber-chat--set-body xml-data text))
            xml-data))))))

(defvar jabber-omemo--sent-muc-plaintexts (make-hash-table :test #'equal)
  "Cache of recently-sent OMEMO MUC message plaintexts.
Keys contain the connection, room, expected local occupant JID,
and message ID.  Entries are consumed when the matching MUC server
echo is received, so the cache is normally near-empty.")

(defun jabber-omemo--muc-echo-key (jc group from id)
  "Return the sent-plaintext cache key for JC, GROUP, FROM, and ID."
  (list jc group from id))

(defun jabber-omemo--detect-encrypted (xml-data)
  "Detect OMEMO encryption in XML-DATA.
Returns a detection plist or nil."
  (when-let* ((parsed (jabber-omemo--parse-encrypted xml-data)))
    (list :type 'omemo :parsed parsed)))

(defun jabber-omemo--recover-prekey-failure (jc sender-jid sender-did)
  "Drop the stale session for SENDER-JID's device SENDER-DID and rebuild.
Called when a pre-key message failed to decrypt on both the
existing-session and fresh-session paths: the local session state
is unusable, so delete it (database and cache) and re-fetch the
peer's sessions so the next exchange re-establishes cleanly.  JC
is the connection."
  (let ((account (jabber-connection-bare-jid jc)))
    (jabber-omemo-store-delete-session account sender-jid sender-did)
    (remhash (jabber-omemo--session-key account sender-jid sender-did)
             jabber-omemo--sessions)
    (message "OMEMO: rebuilding session for %s device %s"
             sender-jid sender-did)
    (jabber-omemo--ensure-sessions jc sender-jid #'ignore)))

(defun jabber-omemo--empty-error-result (xml-data payload err)
  "Return XML-DATA for an empty OMEMO stanza, or re-signal ERR.
PAYLOAD is non-nil when the stanza carries user content."
  (if payload
      (signal (car err) (cdr err))
    (setq jabber-chat--decrypt-retryable-failure-p
          (not jabber-chat--decrypt-consumed-p))
    xml-data))

(defun jabber-omemo--decrypt-handler (jc xml-data detected)
  "Decrypt OMEMO message on JC in XML-DATA.
DETECTED is the plist from `jabber-omemo--detect-encrypted'.

Catches structured OMEMO errors:
- `jabber-omemo-not-for-us': silently return XML-DATA unchanged
  (the stanza is for a different device on the same JID, or a
  heartbeat that doesn't concern us).
- Other failures on empty OMEMO messages also return XML-DATA
  unchanged because those stanzas carry no user content.
- `jabber-omemo-prekey-failed': drop the stale session and
  schedule a rebuild via `jabber-omemo--recover-prekey-failure',
  then re-signal payload failures so the dispatcher reports them to
  the user.  Empty failures remain bodyless.  Bundle repair happens
  via the lifecycle-driven
  `--publish-bundle-if-needed' trigger, not from the decrypt path.
Other payload failures propagate unchanged so the dispatcher can
replace the body with a generic decrypt-failed placeholder.  Other
empty failures remain bodyless."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
         (id (jabber-xml-get-attribute xml-data 'id))
         (group (and from (jabber-jid-user from)))
         (echo-key (and id group
                        (jabber-omemo--muc-echo-key jc group from id)))
         (cached (and echo-key
                      (gethash echo-key
                               jabber-omemo--sent-muc-plaintexts)))
         (payload (plist-get (plist-get detected :parsed) :payload)))
    (if cached
        (progn
          (remhash echo-key jabber-omemo--sent-muc-plaintexts)
          (jabber-chat--set-body xml-data cached))
      (pcase (plist-get detected :type)
       ('omemo
     (condition-case err
         (jabber-omemo--decrypt-stanza
          jc xml-data (plist-get detected :parsed))
       (jabber-omemo-not-for-us
        (jabber-omemo--empty-error-result xml-data payload err))
       (jabber-omemo-prekey-failed
        (message "OMEMO: pre-key decrypt failed: %s"
                 (error-message-string err))
        (pcase-let ((`(,sender-jid ,sender-did ,_reason) (cdr err)))
          (let ((recovery-error
                 (condition-case recovery-err
                     (progn
                       (jabber-omemo--recover-prekey-failure
                        jc sender-jid sender-did)
                       nil)
                   (error recovery-err))))
            (jabber-omemo--empty-error-result
             xml-data payload (or recovery-error err)))))
       (error
        (jabber-omemo--empty-error-result xml-data payload err))))
       (_ xml-data)))))

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

(defun jabber-omemo--muc-participant-jids (group participants)
  "Return deduplicated list of bare JIDs for GROUP.
Collects JIDs from PARTICIPANTS (the alist from
`jabber-muc-participants') and from affiliation query results
in `jabber-muc--room-jids'."
  (let ((jid-set (make-hash-table :test #'equal)))
    (dolist (entry participants)
      (when-let* ((full-jid (plist-get (cdr entry) 'jid))
                  (bare (jabber-jid-user full-jid)))
        (puthash bare t jid-set)))
    (when-let* ((room-jids (gethash group jabber-muc--room-jids)))
      (maphash (lambda (bare _aff) (puthash bare t jid-set)) room-jids))
    (hash-table-keys jid-set)))

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

(defvar jabber-omemo--pending-send-operations
  (make-hash-table :test #'eq)
  "Active OMEMO sends grouped by connection.")

(defun jabber-omemo--send-operation-register (jc success failure)
  "Register an OMEMO send on JC with SUCCESS and FAILURE callbacks."
  (let ((operation (list :active t :connection jc
                         :success success :failure failure)))
    (puthash jc
             (cons operation
                   (gethash jc jabber-omemo--pending-send-operations))
             jabber-omemo--pending-send-operations)
    operation))

(defun jabber-omemo--send-operation-active-p (operation)
  "Return non-nil when OPERATION may still complete."
  (or (null operation) (plist-get operation :active)))

(defun jabber-omemo--send-operation-finish (operation result &optional reason)
  "Finish OPERATION once with RESULT and optional failure REASON."
  (when (and operation (plist-get operation :active))
    (plist-put operation :active nil)
    (let* ((jc (plist-get operation :connection))
           (remaining
            (delq operation
                  (gethash jc jabber-omemo--pending-send-operations))))
      (if remaining
          (puthash jc remaining jabber-omemo--pending-send-operations)
        (remhash jc jabber-omemo--pending-send-operations)))
    (condition-case err
        (if (eq result 'success)
            (when-let* ((callback (plist-get operation :success)))
              (funcall callback))
          (when-let* ((callback (plist-get operation :failure)))
            (funcall callback reason)))
      (error
       (message "OMEMO send callback failed: %s"
                (error-message-string err))))))

(defun jabber-omemo--fail-send-operations (jc reason)
  "Fail every active OMEMO send on JC with REASON."
  (dolist (operation
           (copy-sequence
            (gethash jc jabber-omemo--pending-send-operations)))
    (jabber-omemo--send-operation-finish operation 'failure reason)))

(defun jabber-omemo--fail-all-send-operations (reason)
  "Fail every active OMEMO send with REASON."
  (dolist (jc (hash-table-keys jabber-omemo--pending-send-operations))
    (jabber-omemo--fail-send-operations jc reason)))

(defun jabber-omemo--pending-thread (extra-elements)
  "Return pending thread metadata from EXTRA-ELEMENTS or buffer state."
  (or (and extra-elements
           (jabber-message-thread-protocol-fields
            `(message () ,@extra-elements)))
      (bound-and-true-p jabber-message-reply--thread)
      (when (bound-and-true-p jabber-message-thread-id)
        (list :thread-id jabber-message-thread-id
              :thread-parent-id
              (bound-and-true-p jabber-message-thread-parent-id)))))

(defun jabber-omemo--pending-message (body id extra-elements)
  "Return the pending message plist for BODY, ID, and EXTRA-ELEMENTS."
  (let* ((stanza `(message () (body () ,body) ,@extra-elements))
         (reply (jabber-db--extract-reply-fields stanza))
         (thread (jabber-omemo--pending-thread extra-elements))
         (msg (list :id id :body body :timestamp (current-time)
                    :status :sending :encrypted t)))
    (append msg reply thread)))

(defun jabber-omemo--enter-pending (msg)
  "Enter MSG as pending, or reuse its freshly loaded database node."
  (when (run-hook-with-args-until-success
         'jabber-chat-printers msg :local :printp)
    (let* ((entered (jabber-chat-ewoc-enter (list :local msg)))
           (node (or entered
                     (jabber-chat-ewoc-find-by-id (plist-get msg :id)))))
      (when node
        (let ((stored (cadr (ewoc-data node))))
          (plist-put stored :status :sending)
          (when-let* ((thread-id (plist-get msg :thread-id)))
            (plist-put stored :thread-id thread-id)
            (plist-put stored :thread-parent-id
                       (plist-get msg :thread-parent-id)))
          (unless entered
            (jabber-chat-ewoc-invalidate node)))
        (when entered
          (jabber-maybe-print-rare-time node))
        node))))

(defun jabber-omemo--display-pending (buffer body id &optional jc extra-elements)
  "Display BODY in BUFFER as a message with :sending status.
ID is the stanza id.  JC and EXTRA-ELEMENTS provide send context.
Persists to DB immediately.  Return the owning buffer and ewoc node."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((msg (jabber-omemo--pending-message body id extra-elements))
             (thread (and (plist-get msg :thread-id) msg))
             (reply (and (plist-get msg :reply-to-id) msg)))
        (jabber-db--outgoing-handler body id reply thread)
        (when-let* ((target
                     (if jc
                         (jabber-chat--local-message-buffer jc msg)
                       (and (or (null thread)
                                (bound-and-true-p jabber-message-thread-id))
                            buffer))))
          (with-current-buffer target
            (when-let* ((node (jabber-omemo--enter-pending msg)))
              (list :buffer target :node node))))))))

(defun jabber-omemo--send-failed (buffer node body reason &optional node-buffer)
  "Mark NODE as :undelivered and restore BODY to input area.
BUFFER is the composition buffer.  REASON is shown via `message'.
NODE-BUFFER owns NODE when it differs from BUFFER."
  (when (and node (buffer-live-p (or node-buffer buffer)))
    (with-current-buffer (or node-buffer buffer)
      (plist-put (cadr (ewoc-data node)) :status :undelivered)
      (jabber-chat-ewoc-invalidate node)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert body)))
  (message "%s" reason))

(defun jabber-omemo--send-chat
    (jc body &optional extra-elements success-callback failure-callback)
  "Send BODY as OMEMO-encrypted message via JC.
Must be called from a chat buffer with `jabber-chatting-with' set.
EXTRA-ELEMENTS are spliced into the stanza outside the encryption
envelope (e.g. XEP-0308 replace)."
  (let* ((recipient (jabber-jid-user jabber-chatting-with))
         (chat-with jabber-chatting-with)
         (is-correction (assq 'replace extra-elements))
         (buffer (current-buffer))
         (id (format "emacs-msg-%.6f" (float-time)))
         (send-context
          (jabber-chat--capture-send-context body extra-elements))
         (extra-elements (plist-get send-context :extra-elements))
         (pending (unless is-correction
                    (jabber-omemo--display-pending
                     buffer body id jc extra-elements)))
         (node (plist-get pending :node))
         (node-buffer (plist-get pending :buffer))
         (raw-failed
         (lambda (reason)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (jabber-chat--restore-send-context send-context)))
            (jabber-omemo--send-failed
             buffer node body reason node-buffer)
            (when failure-callback
              (funcall failure-callback reason))))
         (operation
          (jabber-omemo--send-operation-register
           jc success-callback raw-failed))
         (succeeded
          (and operation
               (lambda ()
                 (jabber-omemo--send-operation-finish
                  operation 'success))))
         (failed
          (if operation
              (lambda (reason)
                (jabber-omemo--send-operation-finish
                 operation 'failure reason))
            raw-failed)))
    (condition-case err
        (jabber-omemo--ensure-sessions
         jc recipient
         (lambda (recipient-sessions)
           (when (jabber-omemo--send-operation-active-p operation)
             (if (null recipient-sessions)
                 (funcall failed
                          (format "OMEMO: no sessions for %s, cannot send"
                                  recipient))
               (condition-case own-error
                   (jabber-omemo--ensure-sessions
                    jc (jabber-connection-bare-jid jc)
                    (lambda (own-sessions)
                      (when (jabber-omemo--send-operation-active-p operation)
                        (condition-case send-error
                            (jabber-omemo--send-encrypted
                             jc body chat-with
                             (append recipient-sessions own-sessions)
                             buffer node id extra-elements succeeded failed
                             node-buffer)
                          (error
                           (funcall failed
                                    (error-message-string send-error)))))))
                 (error
                  (funcall failed
                           (error-message-string own-error))))))))
      (error
       (funcall failed (error-message-string err))))))

(defun jabber-omemo--send-encrypted (jc body chat-with all-sessions
                                        &optional buffer node id
                                        extra-elements success-callback
                                        failure-callback node-buffer)
  "Build and send an OMEMO-encrypted stanza.
JC is the connection.  BODY is the plaintext.  CHAT-WITH is the
recipient full/bare JID for addressing.  ALL-SESSIONS is a list
of (DEVICE-ID . SESSION-PTR) for recipient + own other devices.
Optional BUFFER, NODE, ID support immediate display: when NODE is
non-nil, update its status from :sending to :sent instead of
inserting a new ewoc entry.  EXTRA-ELEMENTS are spliced into the
stanza outside the encryption envelope.  SUCCESS-CALLBACK and
FAILURE-CALLBACK report transport completion.  NODE-BUFFER owns NODE."
  (let* ((chat-with (or chat-with jabber-chatting-with))
         (id (or id (format "emacs-msg-%.6f" (float-time))))
         (is-correction (assq 'replace extra-elements))
         (buffer (or buffer (unless is-correction (current-buffer))))
         (plaintext (encode-coding-string body 'utf-8))
         (enc-result (jabber-omemo-encrypt-message plaintext))
         (encrypted-xml (jabber-omemo--build-encrypted-xml
                         jc all-sessions enc-result))
         (stanza `(message ((to . ,chat-with)
                            (type . "chat")
                            (id . ,id))
                           (body () ,jabber-omemo-fallback-body)
                           ,encrypted-xml
                           ,(jabber-hints-store)
                           ,(jabber-eme-encryption jabber-omemo-xmlns "OMEMO")
                           ,@extra-elements)))
    (if (and buffer (not (buffer-live-p buffer)))
        (when failure-callback
          (funcall failure-callback
                   "OMEMO: chat buffer closed before send"))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          ;; This runs from an async IQ callback where current buffer
          ;; is not the chat buffer; the send hooks read buffer-local
          ;; state, so restore the chat buffer first.
          (jabber-chat--run-send-hooks stanza body id)
          (cond
           (node
            (when (buffer-live-p (or node-buffer buffer))
              (with-current-buffer (or node-buffer buffer)
                (plist-put (cadr (ewoc-data node)) :status :sent)
                (jabber-chat-ewoc-invalidate node))))
           ((not is-correction)
            (let ((msg-plist (jabber-chat--msg-plist-from-stanza stanza)))
              (plist-put msg-plist :body body)
              (plist-put msg-plist :status :sent)
              (jabber-chat--display-local-message jc msg-plist))))))
      (if (or success-callback failure-callback)
          (jabber-send-sexp
           jc stanza success-callback failure-callback)
        (jabber-send-sexp jc stanza)))))

(defun jabber-omemo--send-muc
    (jc body &optional extra-elements success-callback failure-callback)
  "Send BODY as OMEMO-encrypted groupchat message via JC.
Must be called from a MUC buffer with `jabber-group' set.
EXTRA-ELEMENTS are spliced into the stanza outside the encryption
envelope."
  (let* ((group jabber-group)
         (buffer (current-buffer))
         (id (format "emacs-msg-%.6f" (float-time)))
         (send-context
          (jabber-chat--capture-send-context body extra-elements))
         (extra-elements (plist-get send-context :extra-elements))
         (participants (cdr (assoc group jabber-muc-participants)))
         (bare-jids (jabber-omemo--muc-participant-jids group participants))
         (raw-failed
         (lambda (reason)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (jabber-chat--restore-send-context send-context)))
            (jabber-omemo--send-failed buffer nil body reason)
            (when failure-callback
              (funcall failure-callback reason))))
         (operation
          (jabber-omemo--send-operation-register
           jc success-callback raw-failed))
         (succeeded
          (and operation
               (lambda ()
                 (jabber-omemo--send-operation-finish
                  operation 'success))))
         (failed
          (if operation
              (lambda (reason)
                (jabber-omemo--send-operation-finish
                 operation 'failure reason))
            raw-failed)))
    (if (null bare-jids)
        (progn
          (funcall failed
                   "OMEMO: no participant JIDs available")
          (user-error "OMEMO: no participant JIDs available (room may be anonymous)"))
      (condition-case err
          (jabber-omemo--ensure-sessions-multi
           jc bare-jids
           (lambda (all-sessions)
             (when (jabber-omemo--send-operation-active-p operation)
               (if (null all-sessions)
                   (let ((reason
                          "OMEMO: no sessions for MUC participants, cannot send"))
                     (funcall failed reason))
                 (condition-case own-error
                     (jabber-omemo--ensure-sessions
                      jc (jabber-connection-bare-jid jc)
                      (lambda (own-sessions)
                        (when (jabber-omemo--send-operation-active-p operation)
                          (condition-case send-error
                              (jabber-omemo--send-encrypted-muc
                               jc body group
                               (append all-sessions own-sessions)
                               buffer id extra-elements succeeded failed)
                            (error
                             (funcall failed
                                      (error-message-string send-error)))))))
                   (error
                    (funcall failed
                             (error-message-string own-error))))))))
        (error
         (funcall failed (error-message-string err)))))))

(defun jabber-omemo--send-encrypted-muc (jc body group all-sessions
                                            &optional buffer id extra-elements
                                            success-callback failure-callback)
  "Build and send an OMEMO-encrypted MUC stanza.
JC is the connection.  BODY is the plaintext.  GROUP is the room JID.
ALL-SESSIONS is a list of (DEVICE-ID . SESSION-PTR) for all
participants plus own other devices.  BUFFER is the MUC buffer whose
buffer-local state the send hooks must see.  ID is the captured stanza ID.
EXTRA-ELEMENTS are
spliced into the stanza outside the encryption envelope.
SUCCESS-CALLBACK and FAILURE-CALLBACK report transport completion.
No local echo: the MUC server mirrors the message back."
  (let* ((plaintext (encode-coding-string body 'utf-8))
         (enc-result (jabber-omemo-encrypt-message plaintext))
         (encrypted-xml (jabber-omemo--build-encrypted-xml
                         jc all-sessions enc-result))
         (id (or id (format "emacs-msg-%.6f" (float-time))))
         (nick (jabber-muc-nickname group jc))
         (echo-key (and nick
                        (jabber-omemo--muc-echo-key
                         jc group (concat group "/" nick) id)))
         (_ (when echo-key
              (puthash echo-key body jabber-omemo--sent-muc-plaintexts)))
         (failed
          (lambda (reason)
            (when echo-key
              (remhash echo-key jabber-omemo--sent-muc-plaintexts))
            (when failure-callback
              (funcall failure-callback reason))))
         (stanza `(message ((to . ,group)
                            (type . "groupchat")
                            (id . ,id))
                           (body () ,jabber-omemo-fallback-body)
                           ,encrypted-xml
                           ,(jabber-hints-store)
                           ,(jabber-eme-encryption jabber-omemo-xmlns "OMEMO")
                           ,@extra-elements)))
    (condition-case err
        (progn
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (jabber-chat--run-send-hooks stanza body id)))
          (if (or success-callback failure-callback)
              (jabber-send-sexp jc stanza success-callback failed)
            (jabber-send-sexp jc stanza)))
      (error
       (funcall failed (error-message-string err))))))

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
    ('nil "new")
    (0 "undecided")
    (1 "TOFU")
    (2 "verified")
    (-1 "UNTRUSTED")
    (_ (format "unknown(%d)" level))))

;;; Connect/disconnect hooks

(defun jabber-omemo--maybe-rotate-signed-pre-key (jc)
  "Rotate JC's signed pre-key when the rotation period has passed.
On the first check for an account, record the current time as a
baseline without rotating.  After a rotation, the bundle
republish check on connect picks up the new signed pre-key id."
  (let* ((account (jabber-connection-bare-jid jc))
         (rotated-at (jabber-omemo-store-spk-rotated-at account))
         (now (time-convert nil 'integer)))
    (cond
     ((null rotated-at)
      (jabber-omemo-store-set-spk-rotated-at account now))
     ((>= (- now rotated-at) jabber-omemo-signed-pre-key-rotation-period)
      (jabber-omemo-rotate-signed-pre-key (jabber-omemo--get-store jc))
      (jabber-omemo--persist-store jc)
      (jabber-omemo-store-set-spk-rotated-at account now)
      (message "OMEMO: rotated signed pre-key for %s" account)))))

;;;###autoload
(defun jabber-omemo-on-connect (jc)
  "Post-connect hook on JC for OMEMO initialization.
Loads or creates the store, rotates the signed pre-key when due,
ensures our device is listed, republishes our bundle if it's out
of date, and pre-fetches sessions for open chat buffers."
  (jabber-omemo--get-store jc)
  (jabber-omemo--get-device-id jc)
  (jabber-omemo--maybe-rotate-signed-pre-key jc)
  (jabber-omemo--ensure-device-listed jc)
  (jabber-omemo--publish-bundle-if-needed jc)
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
  (jabber-omemo--fail-all-send-operations
   "OMEMO: connection closed before the message was sent")
  (clrhash jabber-omemo--device-ids)
  (clrhash jabber-omemo--stores)
  (clrhash jabber-omemo--device-lists)
  (clrhash jabber-omemo--sessions)
  (clrhash jabber-omemo--reconfigured-nodes)
  (clrhash jabber-omemo--bundle-publishes-in-flight)
  (clrhash jabber-omemo--sent-muc-plaintexts))

(defun jabber-omemo--session-reset (jc)
  "Discard pending OMEMO work belonging to logical session JC."
  (jabber-omemo--fail-send-operations
   jc "OMEMO: connection reset before the message was sent")
  (maphash
   (lambda (key _value)
     (when (eq jc (car key))
       (remhash key jabber-omemo--sent-muc-plaintexts)))
   jabber-omemo--sent-muc-plaintexts))

;;; XEP-0454: aesgcm file upload

(defun jabber-omemo--httpupload-transform (filepath callback)
  "Encrypt FILEPATH for aesgcm upload when OMEMO is active.
CALLBACK receives the URL of the uploaded ciphertext.
Returns (ENCRYPTED-PATH . WRAPPED-CALLBACK) or nil."
  (when (eq jabber-chat-encryption 'omemo)
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
          (cons tmp
                (lambda (get-url)
                  (ignore-errors (delete-file tmp))
                  (funcall callback
                           (jabber-omemo--build-aesgcm-url
                            get-url iv key)))))
      (error
       (message "aesgcm: file encryption failed: %s"
                (error-message-string err))
       nil))))

(defun jabber-omemo--httpupload-send-url (jc jid get-url)
  "Send GET-URL (aesgcm://) as an OMEMO-encrypted message from JC to JID.
Returns non-nil if handled, nil to fall through to plaintext."
  (when (string-prefix-p "aesgcm://" get-url)
    ;; This runs from the upload process sentinel, where the current
    ;; buffer is arbitrary; derive the chat buffer from JID rather
    ;; than trusting buffer-local state.
    (if (jabber-muc-joined-p jid)
        (with-current-buffer (jabber-muc-create-buffer jc jid)
          (jabber-omemo--send-muc jc get-url))
      (with-current-buffer (jabber-chat-create-buffer jc jid)
        (jabber-omemo--send-chat jc get-url)))
    t))

;;; Disco and PubSub registration

(when (eq jabber-omemo--available t)
  (jabber-disco-advertise-feature jabber-omemo-xmlns)
  (jabber-disco-advertise-feature (concat jabber-omemo-devicelist-node "+notify"))

  (setf (alist-get jabber-omemo-devicelist-node jabber-pubsub-node-handlers
                   nil nil #'equal)
        #'jabber-omemo--handle-device-list)

  (add-hook 'jabber-post-connect-hooks #'jabber-omemo-on-connect)
  (add-hook 'jabber-pre-disconnect-hook #'jabber-omemo--on-disconnect)
  (add-hook 'jabber-lifecycle-session-reset-functions
            #'jabber-omemo--session-reset)
  (add-hook 'jabber-mam-sync-complete-functions
            #'jabber-omemo--on-mam-sync-complete)

  (setq jabber-httpupload-pre-upload-transform
        #'jabber-omemo--httpupload-transform)
  (setq jabber-httpupload-send-url-function
        #'jabber-omemo--httpupload-send-url))

(when (eq jabber-omemo--available t)
  (jabber-chat-register-decrypt-handler
   'omemo
   :detect  #'jabber-omemo--detect-encrypted
   :decrypt #'jabber-omemo--decrypt-handler
   :priority 10
   :error-label "OMEMO"))

(provide 'jabber-omemo)
;;; jabber-omemo.el ends here
