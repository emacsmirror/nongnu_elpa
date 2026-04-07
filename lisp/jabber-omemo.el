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
(require 'jabber-eme)
(require 'jabber-omemo-trust)

(declare-function jabber-connection-bare-jid "jabber-util")
(declare-function jabber-jid-user "jabber-util")
(declare-function jabber-iq-error "jabber-util")
(declare-function jabber-parse-error "jabber-util")
(declare-function jabber-error-condition "jabber-util")
(declare-function jabber-disco-advertise-feature "jabber-disco")
(declare-function jabber-send-iq "jabber-iq")
(declare-function jabber-send-sexp "jabber-core")
(declare-function jabber-chat--run-send-hooks "jabber-chat"
                  (stanza body id))
(declare-function jabber-chat--msg-plist-from-stanza "jabber-chat")
(declare-function jabber-maybe-print-rare-time "jabber-chat")
(declare-function jabber-chat-ewoc-enter "jabber-chatbuffer")
(declare-function jabber-chat-ewoc-invalidate "jabber-chatbuffer" (node))
(declare-function jabber-httpupload--upload "jabber-httpupload")
(declare-function jabber-httpupload--send-url "jabber-httpupload")
(declare-function jabber-db--outgoing-handler "jabber-db" (body id))
(declare-function jabber-chat-register-decrypt-handler "jabber-chat"
  (id &rest props))
(declare-function jabber-chat--set-body "jabber-chat" (xml-data text))

(defcustom jabber-omemo-enable t
  "Whether to enable OMEMO encryption support.
When nil, the native module is not loaded and OMEMO features are
disabled.  Set to nil if you do not have the build toolchain to
compile jabber-omemo-core."
  :type 'boolean
  :group 'jabber)

(defcustom jabber-omemo-skipped-key-max-age (* 30 86400)
  "Maximum age in seconds for OMEMO skipped message keys.
Keys older than this are deleted on connect."
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
(defvar jabber-group)
(defvar jabber-muc-participants)
(defvar jabber-httpupload-pre-upload-transform)
(defvar jabber-httpupload-send-url-function)
(defvar jabber-message-reply--id)       ; jabber-message-reply.el
(defvar jabber-message-reply--jid)      ; jabber-message-reply.el

(unless module-file-suffix
  (error "jabber-omemo requires Emacs compiled with dynamic module support"))

(defvar jabber-omemo-build-command
  (if (eq system-type 'darwin)
      "make module CC=clang"
    "make module")
  "Shell command to build the jabber-omemo-core dynamic module.
Run from the emacs-jabber project root.")

(defun jabber-omemo--build-module (project-root)
  "Build the native module synchronously from PROJECT-ROOT.
Compiles picomemo via `jabber-omemo-build-command', then loads
the resulting module.  Signals an error on build failure."
  (let ((default-directory project-root)
        (buf (get-buffer-create "*jabber-omemo-build*")))
    (message "Building jabber-omemo-core module...")
    (unless (zerop (call-process-shell-command
                    jabber-omemo-build-command nil buf t))
      (pop-to-buffer buf)
      (error "Failed to build jabber-omemo-core module.  See *jabber-omemo-build*"))
    (require 'jabber-omemo-core)
    (message "jabber-omemo-core module built and loaded.")))

(defvar jabber-omemo--available nil
  "Non-nil when the jabber-omemo-core native module is loaded.")

(cl-eval-when (load eval)
  (if (not jabber-omemo-enable)
      (message "OMEMO: disabled by jabber-omemo-enable")
    (if (require 'jabber-omemo-core nil t)
        (setq jabber-omemo--available t)
      (let* ((this-dir (file-name-directory
                        (or load-file-name buffer-file-name)))
             (src-dir (if (file-directory-p
                           (expand-file-name "src" this-dir))
                          (expand-file-name "src" this-dir)
                        (expand-file-name "../src" this-dir))))
        (if (and (file-exists-p (expand-file-name "jabber-omemo-core.c" src-dir))
                 (or noninteractive
                     (yes-or-no-p
                      "jabber-omemo-core module not found.  Build it now? ")))
            (progn
              (jabber-omemo--build-module
               (file-name-directory (directory-file-name src-dir)))
              (setq jabber-omemo--available t))
          (message "OMEMO: native module not available, encryption disabled"))))))

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
  "Remove DEVICE-ID from our published device list and delete its bundle.
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

;;; Session establishment

(defun jabber-omemo--establish-session (jc jid device-id bundle)
  "Establish an OMEMO session with JID's DEVICE-ID using BUNDLE.
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

(defun jabber-omemo--decrypt-stanza (jc xml-data parsed)
  "Decrypt OMEMO message in XML-DATA using PARSED data.
Returns modified XML-DATA with decrypted body.

Signals structured errors that callers can dispatch on:
- `jabber-omemo-not-for-us' when the stanza has no key entry for
  our device (heartbeat or message addressed to a different device).
- `jabber-omemo-no-session' for a non-prekey message when we have
  no local session with the sender's device.
- `jabber-omemo-prekey-failed' when the C decrypt fails on a
  pre-key message (usually a stale local pre-key).
- `jabber-omemo-error' (the parent) for all other crypto failures."
  (let* ((our-did (jabber-omemo--get-device-id jc))
         (account (jabber-connection-bare-jid jc))
         (from (jabber-xml-get-attribute xml-data 'from))
         (sender-jid (and from (jabber-jid-user from))))
    (if (not sender-jid)
        (warn "OMEMO: ignoring encrypted message with no 'from' attribute")
    (let* ((sender-did (plist-get parsed :sid))
           (iv (plist-get parsed :iv))
           (payload (plist-get parsed :payload))
           (keys (plist-get parsed :keys))
           (our-key-entry (cl-find our-did keys :key #'car)))
      (unless our-key-entry
        (signal 'jabber-omemo-not-for-us (list our-did)))
      (let* ((key-data (plist-get (cdr our-key-entry) :data))
             (pre-key-p (plist-get (cdr our-key-entry) :pre-key-p))
             (store-ptr (jabber-omemo--get-store jc))
             (session-ptr (if pre-key-p
                              (jabber-omemo-make-session)
                            (or (jabber-omemo--get-session
                                 jc sender-jid sender-did)
                                (signal 'jabber-omemo-no-session
                                        (list sender-jid sender-did)))))
             (decrypted-key
              (condition-case err
                  (jabber-omemo-decrypt-key
                   session-ptr store-ptr pre-key-p key-data)
                (jabber-omemo-error
                 (if pre-key-p
                     (signal 'jabber-omemo-prekey-failed
                             (list sender-jid sender-did
                                   (error-message-string err)))
                   (signal (car err) (cdr err)))))))
        (jabber-omemo--save-session jc sender-jid sender-did session-ptr)
        (jabber-omemo--persist-store jc)
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
        (when pre-key-p
          (jabber-omemo-refill-pre-keys store-ptr)
          (jabber-omemo--persist-store jc)
          (jabber-omemo--publish-bundle jc))
        (if payload
            (let* ((plaintext (jabber-omemo-decrypt-message
                               decrypted-key iv payload))
                   (text (decode-coding-string plaintext 'utf-8)))
              (jabber-chat--set-body xml-data text))
          xml-data))))))

(defvar jabber-omemo--sent-muc-plaintexts (make-hash-table :test #'equal)
  "Cache of recently-sent OMEMO MUC message plaintexts.
Keyed by message ID string.  Entries are consumed when the MUC
server echo is received, so the cache is normally near-empty.")

(defun jabber-omemo--detect-encrypted (xml-data)
  "Detect OMEMO encryption in XML-DATA.
Returns a detection plist or nil.  Checks MUC echo cache first,
then looks for <encrypted> element."
  (let* ((msg-id (jabber-xml-get-attribute xml-data 'id))
         (cached (and msg-id
                      (gethash msg-id jabber-omemo--sent-muc-plaintexts))))
    (cond
     (cached
      (remhash msg-id jabber-omemo--sent-muc-plaintexts)
      (list :type 'muc-echo :cached cached))
     (t
      (when-let* ((parsed (jabber-omemo--parse-encrypted xml-data)))
        (list :type 'omemo :parsed parsed))))))

(defun jabber-omemo--decrypt-handler (jc xml-data detected)
  "Decrypt OMEMO message.  DETECTED is the plist from detect."
  (pcase (plist-get detected :type)
    ('muc-echo
     (jabber-chat--set-body xml-data (plist-get detected :cached)))
    ('omemo
     (jabber-omemo--decrypt-stanza jc xml-data (plist-get detected :parsed)))
    (_ xml-data)))

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

(defun jabber-omemo--display-pending (buffer body id)
  "Display BODY in BUFFER as a message with :sending status.
ID is the stanza id.  Persists to DB immediately.
Returns the ewoc node, or nil if BUFFER is dead."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((reply-id (bound-and-true-p jabber-message-reply--id))
             (reply-jid (bound-and-true-p jabber-message-reply--jid))
             (msg-plist (list :id id
                              :body body
                              :timestamp (current-time)
                              :status :sending
                              :encrypted t)))
        (when reply-id
          (plist-put msg-plist :reply-to-id reply-id)
          (plist-put msg-plist :reply-to-jid reply-jid))
        (jabber-db--outgoing-handler body id)
        (when (run-hook-with-args-until-success
               'jabber-chat-printers msg-plist :local :printp)
          (let ((node (jabber-chat-ewoc-enter (list :local msg-plist))))
            (jabber-maybe-print-rare-time node)
            node))))))

(defun jabber-omemo--send-failed (buffer node body reason)
  "Mark NODE as :undelivered and restore BODY to input area.
BUFFER is the chat buffer.  REASON is shown via `message'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when node
        (plist-put (cadr (ewoc-data node)) :status :undelivered)
        (jabber-chat-ewoc-invalidate node))
      (goto-char (point-max))
      (insert body)))
  (message "%s" reason))

(defun jabber-omemo--send-chat (jc body &optional extra-elements)
  "Send BODY as OMEMO-encrypted message via JC.
Must be called from a chat buffer with `jabber-chatting-with' set.
EXTRA-ELEMENTS are spliced into the stanza outside the encryption
envelope (e.g. XEP-0308 replace)."
  (let* ((recipient (jabber-jid-user jabber-chatting-with))
         (chat-with jabber-chatting-with)
         (is-correction (assq 'replace extra-elements))
         (buffer (if is-correction nil (current-buffer)))
         (id (format "emacs-msg-%.6f" (float-time)))
         (node (unless is-correction
                 (jabber-omemo--display-pending (current-buffer) body id))))
    (jabber-omemo--ensure-sessions
     jc recipient
     (lambda (recipient-sessions)
       (if (null recipient-sessions)
           (jabber-omemo--send-failed
            buffer node body
            (format "OMEMO: no sessions for %s, cannot send" recipient))
         (jabber-omemo--ensure-sessions
          jc (jabber-connection-bare-jid jc)
          (lambda (own-sessions)
            (jabber-omemo--send-encrypted
             jc body chat-with
             (append recipient-sessions own-sessions)
             buffer node id extra-elements))))))))

(defun jabber-omemo--send-encrypted (jc body chat-with all-sessions
                                        &optional buffer node id
                                        extra-elements)
  "Build and send an OMEMO-encrypted stanza.
JC is the connection.  BODY is the plaintext.  CHAT-WITH is the
recipient full/bare JID for addressing.  ALL-SESSIONS is a list
of (DEVICE-ID . SESSION-PTR) for recipient + own other devices.
Optional BUFFER, NODE, ID support immediate display: when NODE is
non-nil, update its status from :sending to :sent instead of
inserting a new ewoc entry.  EXTRA-ELEMENTS are spliced into the
stanza outside the encryption envelope."
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
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Inline hook loop instead of jabber-chat--run-send-hooks:
        ;; this runs from an async IQ callback where current buffer
        ;; is not the chat buffer, so we need with-current-buffer.
        (dolist (hook jabber-chat-send-hooks)
          (if (eq hook t)
              (when (local-variable-p 'jabber-chat-send-hooks)
                (dolist (global-hook (default-value 'jabber-chat-send-hooks))
                  (nconc stanza (funcall global-hook body id))))
            (nconc stanza (funcall hook body id))))
        (if node
            (progn
              (plist-put (cadr (ewoc-data node)) :status :sent)
              (jabber-chat-ewoc-invalidate node))
          (let ((msg-plist (jabber-chat--msg-plist-from-stanza stanza)))
            (plist-put msg-plist :body body)
            (plist-put msg-plist :status :sent)
            (when (run-hook-with-args-until-success
                   'jabber-chat-printers msg-plist :local :printp)
              (jabber-maybe-print-rare-time
               (jabber-chat-ewoc-enter (list :local msg-plist))))))))
    (jabber-send-sexp jc stanza)))

(defun jabber-omemo--send-muc (jc body &optional extra-elements)
  "Send BODY as OMEMO-encrypted groupchat message via JC.
Must be called from a MUC buffer with `jabber-group' set.
EXTRA-ELEMENTS are spliced into the stanza outside the encryption
envelope."
  (let* ((group jabber-group)
         (buffer (current-buffer))
         (participants (cdr (assoc group jabber-muc-participants)))
         (bare-jids (jabber-omemo--muc-participant-jids group participants)))
    (if (null bare-jids)
        (progn
          (jabber-omemo--send-failed
           buffer nil body
           "OMEMO: no participant JIDs available (room may be anonymous)")
          (user-error "OMEMO: no participant JIDs available (room may be anonymous)"))
      (jabber-omemo--ensure-sessions-multi
       jc bare-jids
       (lambda (all-sessions)
         (if (null all-sessions)
             (jabber-omemo--send-failed
              buffer nil body
              "OMEMO: no sessions for MUC participants, cannot send")
           (jabber-omemo--ensure-sessions
            jc (jabber-connection-bare-jid jc)
            (lambda (own-sessions)
              (jabber-omemo--send-encrypted-muc
               jc body group
               (append all-sessions own-sessions)
               extra-elements)))))))))

(defun jabber-omemo--send-encrypted-muc (jc body group all-sessions
                                             &optional extra-elements)
  "Build and send an OMEMO-encrypted MUC stanza.
JC is the connection.  BODY is the plaintext.  GROUP is the room JID.
ALL-SESSIONS is a list of (DEVICE-ID . SESSION-PTR) for all
participants plus own other devices.  EXTRA-ELEMENTS are spliced
into the stanza outside the encryption envelope.
No local echo: the MUC server mirrors the message back."
  (let* ((plaintext (encode-coding-string body 'utf-8))
         (enc-result (jabber-omemo-encrypt-message plaintext))
         (encrypted-xml (jabber-omemo--build-encrypted-xml
                         jc all-sessions enc-result))
         (id (format "emacs-msg-%.6f" (float-time)))
         (_ (puthash id body jabber-omemo--sent-muc-plaintexts))
         (stanza `(message ((to . ,group)
                            (type . "groupchat")
                            (id . ,id))
                           (body () ,jabber-omemo-fallback-body)
                           ,encrypted-xml
                           ,(jabber-hints-store)
                           ,(jabber-eme-encryption jabber-omemo-xmlns "OMEMO")
                           ,@extra-elements)))
    (jabber-chat--run-send-hooks stanza body id)
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
    ('nil "new")
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

;;;###autoload
(defun jabber-omemo-on-connect (jc)
  "Post-connect hook for OMEMO initialization.
Loads or creates the store, ensures our device is listed,
publishes our bundle, and pre-fetches sessions for open chat buffers."
  (jabber-omemo--get-store jc)
  (jabber-omemo--get-device-id jc)
  (jabber-omemo--ensure-device-listed jc)
  (jabber-omemo--publish-bundle jc)
  (jabber-omemo--prefetch-open-chats jc)
  (jabber-omemo-store-delete-old-skipped-keys
   (jabber-connection-bare-jid jc)
   jabber-omemo-skipped-key-max-age))

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

;;;###autoload
(defun jabber-omemo--on-disconnect ()
  "Pre-disconnect hook.  Clear OMEMO in-memory caches."
  (clrhash jabber-omemo--device-ids)
  (clrhash jabber-omemo--stores)
  (clrhash jabber-omemo--device-lists)
  (clrhash jabber-omemo--sessions)
  (clrhash jabber-omemo--reconfigured-nodes))

;;; XEP-0454: aesgcm file upload

(defun jabber-omemo--httpupload-transform (filepath callback)
  "Encrypt FILEPATH for aesgcm upload when OMEMO is active.
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
  "Send aesgcm:// URL as OMEMO-encrypted message.
Returns non-nil if handled, nil to fall through to plaintext."
  (when (string-prefix-p "aesgcm://" get-url)
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
    t))

;;; Disco and PubSub registration

(when jabber-omemo--available
  (jabber-disco-advertise-feature jabber-omemo-xmlns)
  (jabber-disco-advertise-feature (concat jabber-omemo-devicelist-node "+notify"))

  (with-eval-after-load "jabber-pubsub"
    (setf (alist-get jabber-omemo-devicelist-node jabber-pubsub-node-handlers
                     nil nil #'equal)
          #'jabber-omemo--handle-device-list))

  (with-eval-after-load "jabber-core"
    (add-hook 'jabber-post-connect-hooks #'jabber-omemo-on-connect)
    (add-hook 'jabber-pre-disconnect-hook #'jabber-omemo--on-disconnect))

  (with-eval-after-load "jabber-httpupload"
    (setq jabber-httpupload-pre-upload-transform
          #'jabber-omemo--httpupload-transform)
    (setq jabber-httpupload-send-url-function
          #'jabber-omemo--httpupload-send-url)))

(when jabber-omemo--available
  (jabber-chat-register-decrypt-handler
   'omemo
   :detect  #'jabber-omemo--detect-encrypted
   :decrypt #'jabber-omemo--decrypt-handler
   :priority 10
   :error-label "OMEMO"))

(provide 'jabber-omemo)
;;; jabber-omemo.el ends here
