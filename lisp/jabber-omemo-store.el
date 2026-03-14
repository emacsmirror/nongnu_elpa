;;; jabber-omemo-store.el --- OMEMO persistence  -*- lexical-binding: t; -*-

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

;; SQLite persistence for OMEMO state.  Uses the shared database
;; connection from `jabber-db'.  OMEMO tables are created by
;; `jabber-db--migrate' (version 2) when the database is opened.

;;; Code:

(require 'jabber-db)

(defsubst jabber-omemo-store--as-unibyte (value)
  "Return VALUE as a unibyte string if it is a string, else VALUE.
SQLite returns BLOBs as multibyte strings; this normalizes them
for the C module which expects unibyte."
  (if (and (stringp value) (multibyte-string-p value))
      (string-as-unibyte value)
    value))

;;; Store blob CRUD

(defun jabber-omemo-store-save (account blob)
  "Upsert serialized OMEMO store BLOB for ACCOUNT."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db
      "INSERT OR REPLACE INTO omemo_store (account, store_blob) VALUES (?, ?)"
      (list account blob))))

(defun jabber-omemo-store-load (account)
  "Load serialized OMEMO store blob for ACCOUNT, or nil."
  (when-let* ((db (jabber-db-ensure-open)))
    (jabber-omemo-store--as-unibyte
     (caar (sqlite-select db
             "SELECT store_blob FROM omemo_store WHERE account = ?"
             (list account))))))

(defun jabber-omemo-store-delete (account)
  "Delete OMEMO store for ACCOUNT."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db
      "DELETE FROM omemo_store WHERE account = ?"
      (list account))))

;;; Trust CRUD

(defun jabber-omemo-store-save-trust (account jid device-id
                                      identity-key trust)
  "Upsert trust record for ACCOUNT, JID, DEVICE-ID.
IDENTITY-KEY is a unibyte blob.
TRUST is 0=undecided, 1=tofu, 2=verified, -1=untrusted.
Sets first_seen to current time on initial insert."
  (when-let* ((db (jabber-db-ensure-open)))
    (let ((now (truncate (float-time))))
      (sqlite-execute db "\
INSERT INTO omemo_trust (account, jid, device_id, identity_key, trust, first_seen)
  VALUES (?, ?, ?, ?, ?, ?)
  ON CONFLICT (account, jid, device_id)
  DO UPDATE SET identity_key = excluded.identity_key,
                trust = excluded.trust"
        (list account jid device-id identity-key trust now)))))

(defun jabber-omemo-store-load-trust (account jid device-id)
  "Load trust record for ACCOUNT, JID, DEVICE-ID as a plist, or nil.
Returns (:identity-key BLOB :trust INT :first-seen INT)."
  (when-let* ((db (jabber-db-ensure-open)))
    (when-let* ((row (car (sqlite-select db "\
SELECT identity_key, trust, first_seen FROM omemo_trust
  WHERE account = ? AND jid = ? AND device_id = ?"
                           (list account jid device-id)))))
      (list :identity-key (jabber-omemo-store--as-unibyte (nth 0 row))
            :trust (nth 1 row)
            :first-seen (nth 2 row)))))

(defun jabber-omemo-store-set-trust (account jid device-id level)
  "Update trust LEVEL for a known device (ACCOUNT, JID, DEVICE-ID)."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db "\
UPDATE omemo_trust SET trust = ?
  WHERE account = ? AND jid = ? AND device_id = ?"
      (list level account jid device-id))))

(defun jabber-omemo-store-all-trust (account jid)
  "List all trust records for ACCOUNT + JID.
Returns a list of plists (:device-id INT :identity-key BLOB
:trust INT :first-seen INT)."
  (when-let* ((db (jabber-db-ensure-open)))
    (mapcar (lambda (row)
              (list :device-id (nth 0 row)
                    :identity-key (jabber-omemo-store--as-unibyte (nth 1 row))
                    :trust (nth 2 row)
                    :first-seen (nth 3 row)))
            (sqlite-select db "\
SELECT device_id, identity_key, trust, first_seen FROM omemo_trust
  WHERE account = ? AND jid = ?"
              (list account jid)))))

;;; Device list CRUD

(defun jabber-omemo-store-save-device (account jid device-id
                                       &optional active)
  "Upsert device record for ACCOUNT, JID, DEVICE-ID.
ACTIVE defaults to 1 (true).  Sets last_seen to current time."
  (when-let* ((db (jabber-db-ensure-open)))
    (let ((now (truncate (float-time)))
          (act (if (or (null active) (eq active t)) 1 active)))
      (sqlite-execute db "\
INSERT OR REPLACE INTO omemo_devices (account, jid, device_id, active, last_seen)
  VALUES (?, ?, ?, ?, ?)"
        (list account jid device-id act now)))))

(defun jabber-omemo-store-load-devices (account jid)
  "Load all device records for ACCOUNT + JID.
Returns a list of plists (:device-id INT :active BOOL :last-seen INT)."
  (when-let* ((db (jabber-db-ensure-open)))
    (mapcar (lambda (row)
              (list :device-id (nth 0 row)
                    :active (not (zerop (nth 1 row)))
                    :last-seen (nth 2 row)))
            (sqlite-select db "\
SELECT device_id, active, last_seen FROM omemo_devices
  WHERE account = ? AND jid = ?"
              (list account jid)))))

(defun jabber-omemo-store-set-device-active (account jid device-id active)
  "Mark device DEVICE-ID as ACTIVE (non-nil) or inactive (nil)."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db "\
UPDATE omemo_devices SET active = ?
  WHERE account = ? AND jid = ? AND device_id = ?"
      (list (if active 1 0) account jid device-id))))

(defun jabber-omemo-store-delete-device (account jid device-id)
  "Remove a device record for ACCOUNT, JID, DEVICE-ID."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db "\
DELETE FROM omemo_devices
  WHERE account = ? AND jid = ? AND device_id = ?"
      (list account jid device-id))))

;;; Session CRUD

(defun jabber-omemo-store-save-session (account jid device-id blob)
  "Upsert session BLOB for ACCOUNT, JID, DEVICE-ID."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db "\
INSERT OR REPLACE INTO omemo_sessions (account, jid, device_id, session_blob)
  VALUES (?, ?, ?, ?)"
      (list account jid device-id blob))))

(defun jabber-omemo-store-load-session (account jid device-id)
  "Load session blob for ACCOUNT, JID, DEVICE-ID, or nil."
  (when-let* ((db (jabber-db-ensure-open)))
    (jabber-omemo-store--as-unibyte
     (caar (sqlite-select db "\
SELECT session_blob FROM omemo_sessions
  WHERE account = ? AND jid = ? AND device_id = ?"
             (list account jid device-id))))))

(defun jabber-omemo-store-delete-session (account jid device-id)
  "Delete session for ACCOUNT, JID, DEVICE-ID."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db "\
DELETE FROM omemo_sessions
  WHERE account = ? AND jid = ? AND device_id = ?"
      (list account jid device-id))))

(defun jabber-omemo-store-all-sessions (account jid)
  "List all sessions for ACCOUNT + JID.
Returns a list of plists (:device-id INT :session-blob BLOB)."
  (when-let* ((db (jabber-db-ensure-open)))
    (mapcar (lambda (row)
              (list :device-id (nth 0 row)
                    :session-blob (jabber-omemo-store--as-unibyte (nth 1 row))))
            (sqlite-select db "\
SELECT device_id, session_blob FROM omemo_sessions
  WHERE account = ? AND jid = ?"
              (list account jid)))))

;;; Skipped key CRUD

(defun jabber-omemo-store-save-skipped-key (account jid device-id
                                            dh-key msg-number msg-key)
  "Store a skipped message key for ACCOUNT, JID, DEVICE-ID.
DH-KEY and MSG-KEY are unibyte blobs.  MSG-NUMBER is an integer."
  (when-let* ((db (jabber-db-ensure-open)))
    (let ((now (truncate (float-time))))
      (sqlite-execute db "\
INSERT OR REPLACE INTO omemo_skipped_keys
  (account, jid, device_id, dh_key, message_number, message_key, created_at)
  VALUES (?, ?, ?, ?, ?, ?, ?)"
        (list account jid device-id dh-key msg-number msg-key now)))))

(defun jabber-omemo-store-load-skipped-key (account jid device-id
                                            dh-key msg-number)
  "Load a skipped message key, or nil."
  (when-let* ((db (jabber-db-ensure-open)))
    (jabber-omemo-store--as-unibyte
     (caar (sqlite-select db "\
SELECT message_key FROM omemo_skipped_keys
  WHERE account = ? AND jid = ? AND device_id = ?
    AND dh_key = ? AND message_number = ?"
             (list account jid device-id dh-key msg-number))))))

(defun jabber-omemo-store-delete-skipped-key (account jid device-id
                                              dh-key msg-number)
  "Delete a skipped message key after use."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db "\
DELETE FROM omemo_skipped_keys
  WHERE account = ? AND jid = ? AND device_id = ?
    AND dh_key = ? AND message_number = ?"
      (list account jid device-id dh-key msg-number))))

(defun jabber-omemo-store-delete-old-skipped-keys (account max-age)
  "Delete skipped keys for ACCOUNT older than MAX-AGE seconds."
  (when-let* ((db (jabber-db-ensure-open)))
    (let ((cutoff (- (truncate (float-time)) max-age)))
      (sqlite-execute db "\
DELETE FROM omemo_skipped_keys
  WHERE account = ? AND created_at < ?"
        (list account cutoff)))))

(provide 'jabber-omemo-store)
;;; jabber-omemo-store.el ends here
