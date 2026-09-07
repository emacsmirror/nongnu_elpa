;;; hermes-chat-images.el --- Local image drafts and byte uploads -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Keep image bytes local until submission.  The released HTTP upload only
;; writes a remote file; image.attach then stages that returned path for the
;; next prompt.  These operations are not atomic.  Retain every send in an
;; owned recovery buffer, including acknowledged sends: interrupt can discard
;; a backend queue after acknowledgment.  Recovery is manual, never replay.
;; Recovery buffers survive chat teardown, but not Emacs exit.  Keep image
;; sessions exclusive to this Emacs instance: backend staging is shared with
;; all senders and is not an image-and-text transaction.  Streaming acceptance
;; keeps the local session fence until successful completion; image-bearing
;; queued acceptance has already copied staging.  Ordinary text admission also
;; excludes other buffers until completion.  Text-only queues consume staging
;; later without correlated turn IDs, so they retain image-only uncertainty.
;; Text uncertainty blocks images but leaves ordinary text recovery available.
;; Failure or interruption requires manual recovery
;; into a new session.  Uploaded remote files are not deleted or auto-detached:
;; a lost response cannot prove which paths were staged or already consumed.
;; Local limits are 2 MiB/image, eight images/message, and 32 MiB/64 recovery
;; records per chat.  Native PNG clipboard support is optional; use a file when
;; the editor cannot provide binary image data.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'select)
(require 'hermes-chat-buffer)
(require 'hermes-dashboard-rpc)

(defcustom hermes-chat-image-max-bytes (* 2 1024 1024)
  "Maximum bytes per local image, additionally capped at 2 MiB."
  :type 'natnum
  :group 'hermes)

(defconst hermes-chat--image-total-limit (* 32 1024 1024)
  "Maximum bytes retained by one chat's image recovery buffer.")

(defvar hermes-chat--image-session-blocks (make-hash-table :test #'equal)
  "Image staging owners keyed by endpoint and session, across chat buffers.
Uncertain owners survive chat teardown.  Other Emacs instances and external
senders cannot be fenced by this registry; do not share an image-send session.")

(defun hermes-chat--image-session-key ()
  "Return the current dashboard endpoint and live session key."
  (when hermes-chat--dashboard-client
    (cons (hermes-dashboard-transport--api-client-base-url
           hermes-chat--dashboard-client)
          hermes-chat--dashboard-active-session-id)))

(defvar hermes-chat--image-prior-submits (make-hash-table :test #'equal)
  "Ordinary submissions that may still consume session image staging.
Values are admission records, or the symbol `uncertain'.  Pending admissions
exclude other chat buffers; the owner can still submit text/steering.  Failed
sessions block images with one byte-free marker, without blocking ordinary text.")

(defun hermes-chat--image-admission-start (assistant-id)
  "Fence images before an ordinary submission for ASSISTANT-ID enters RPC."
  (let* ((key (hermes-chat--image-session-key))
         (owners (gethash key hermes-chat--image-prior-submits))
         (record (list :session-key key :owner (current-buffer)
                       :client hermes-chat--dashboard-client
                       :lifetime hermes-chat--lifecycle-generation
                       :generation hermes-chat--transport-generation
                       :assistant-id assistant-id)))
    (unless (memq 'uncertain owners)
      (puthash key (cons record owners) hermes-chat--image-prior-submits)
      record)))

(defun hermes-chat--image-admission-finish (record &optional uncertain)
  "Retire ordinary admission RECORD, retaining UNCERTAIN staging ownership."
  (when record
    (let* ((key (plist-get record :session-key))
           (owners (gethash key hermes-chat--image-prior-submits)))
      (when (memq record owners)
        (let ((remaining (delq record owners)))
          (if uncertain
              (puthash key '(uncertain) hermes-chat--image-prior-submits)
            (if remaining
                (puthash key remaining hermes-chat--image-prior-submits)
              (remhash key hermes-chat--image-prior-submits))))))))

(defun hermes-chat--image-admission-ack (record result)
  "Apply RESULT's staging evidence to ordinary admission RECORD."
  (pcase (hermes-transport--get result 'status)
    ;; Text-only queue envelopes omit image_paths and consume mutable staging
    ;; later.  Unqualified events cannot establish that queued turn's ownership.
    ("queued" (hermes-chat--image-admission-finish record t))
    ((or "steered" "redirected")
     (hermes-chat--image-admission-finish record))
    ("streaming" nil)
    (_ (hermes-chat--image-admission-finish record t))))

(defun hermes-chat--image-owner-current-p (record)
  "Return whether RECORD belongs to this buffer's current transport lifetime."
  (and (listp record) (eq (plist-get record :owner) (current-buffer))
       (eq (plist-get record :client) hermes-chat--dashboard-client)
       (eql (plist-get record :lifetime) hermes-chat--lifecycle-generation)
       (eql (plist-get record :generation) hermes-chat--transport-generation)))

(defun hermes-chat--images-rotate (assistant-id next-id)
  "Carry staging ownership from ASSISTANT-ID to its interim successor NEXT-ID."
  (let ((key (hermes-chat--image-session-key)))
    (dolist (record (cons (gethash key hermes-chat--image-session-blocks)
                          (gethash key hermes-chat--image-prior-submits)))
      (when (and (hermes-chat--image-owner-current-p record)
                 (equal assistant-id (plist-get record :assistant-id)))
        (setf (plist-get record :assistant-id) next-id)))))

(defvar-local hermes-chat--draft-images nil
  "Images selected for the next composer submission.")

(defun hermes-chat--images-inhibit ()
  "Return a reason to block sends while image staging has uncertain ownership."
  (cond
   ((gethash (hermes-chat--image-session-key) hermes-chat--image-session-blocks)
    "Image staging is pending or uncertain; use image recovery in a new session")
   ;; The wire has no stable turn ID.  Two buffers must not both correlate
   ;; the same terminal event to different ordinary admissions.
   ((seq-some (lambda (record)
                (and (listp record)
                     (not (eq (plist-get record :owner) (current-buffer)))))
              (gethash (hermes-chat--image-session-key) hermes-chat--image-prior-submits))
    "Session submission pending in another chat buffer")
   ((and hermes-chat--draft-images
         (gethash (hermes-chat--image-session-key) hermes-chat--image-prior-submits))
    "Prior session submission pending or uncertain; wait or use a new session")))

(defun hermes-chat--images-release (record)
  "Release only the staging lock still owned by RECORD."
  (let ((key (plist-get record :session-key)))
    (when (eq record (gethash key hermes-chat--image-session-blocks))
      (remhash key hermes-chat--image-session-blocks))))

(defun hermes-chat--images-settle (assistant-id status)
  "Release consumed staging for ASSISTANT-ID after successful STATUS.
Errors and local interruption do not prove that staging was consumed."
  (let ((record (gethash (hermes-chat--image-session-key)
                         hermes-chat--image-session-blocks)))
    (when (and (hermes-chat--image-owner-current-p record)
               (equal assistant-id (plist-get record :assistant-id)))
      (if (and (eq status 'done)
               (memq (plist-get record :state) '(submitted accepted)))
          (hermes-chat--images-release record)
        (setf (plist-get record :state) 'uncertain))))
  (dolist (record (copy-sequence
                  (gethash (hermes-chat--image-session-key)
                           hermes-chat--image-prior-submits)))
    (when (and assistant-id (hermes-chat--image-owner-current-p record)
               (equal assistant-id (plist-get record :assistant-id)))
      (hermes-chat--image-admission-finish record (not (eq status 'done))))))

(defvar-local hermes-chat--image-draft-record nil
  "Recovery record owning the current image draft.")
(defvar-local hermes-chat--image-recovery-buffer nil
  "Owned image recovery buffer, not killed on chat teardown.")
(defvar-local hermes-chat--image-records nil
  "Image records owned by a recovery buffer.")

(defun hermes-chat--image-kind (bytes)
  "Return the safe raster type of BYTES, or nil."
  (cond
   ((string-prefix-p (unibyte-string 137 80 78 71 13 10 26 10) bytes) 'png)
   ((string-prefix-p (unibyte-string 255 216 255) bytes) 'jpeg)
   ((or (string-prefix-p "GIF87a" bytes) (string-prefix-p "GIF89a" bytes)) 'gif)
   ((and (>= (length bytes) 12) (string-prefix-p "RIFF" bytes)
         (equal (substring bytes 8 12) "WEBP")) 'webp)
   ((string-prefix-p "BM" bytes) 'bmp)))

(defun hermes-chat--image-bytes (images)
  "Return the total byte size of IMAGES."
  (apply #'+ (mapcar (lambda (image) (length (plist-get image :bytes))) images)))

(defun hermes-chat--image-recovery ()
  "Return this chat's owned recovery buffer, creating it if necessary."
  (unless (buffer-live-p hermes-chat--image-recovery-buffer)
    (let ((buffer (generate-new-buffer "*Hermes Image Recovery*")))
      (with-current-buffer buffer (hermes-chat-image-recovery-mode))
      (setq hermes-chat--image-recovery-buffer buffer)))
  hermes-chat--image-recovery-buffer)

(defun hermes-chat--image-stage (bytes)
  "Validate and retain raster BYTES for the next composer submission."
  (unless (derived-mode-p 'hermes-chat-mode)
    (user-error "Not in a Hermes chat buffer"))
  (unless (and (stringp bytes) (not (multibyte-string-p bytes))
               (> (length bytes) 0)
               (<= (length bytes) (min hermes-chat-image-max-bytes (* 2 1024 1024))))
    (user-error "Image must contain at most 2 MiB of binary data"))
  (let ((kind (hermes-chat--image-kind bytes)))
    (unless kind (user-error "Use PNG, JPEG, GIF, WebP, or BMP image bytes"))
    (when (>= (length hermes-chat--draft-images) 8)
      (user-error "At most eight images per message"))
    (let* ((recovery (hermes-chat--image-recovery))
           (records (buffer-local-value 'hermes-chat--image-records recovery))
           (used (apply #'+ (mapcar (lambda (record)
                                      (hermes-chat--image-bytes (plist-get record :images)))
                                    records))))
      (when (> (+ used (length bytes)) hermes-chat--image-total-limit)
        (user-error "Image recovery is full; discard unneeded records first"))
      (when (and (null hermes-chat--image-draft-record) (>= (length records) 64))
        (user-error "Image recovery has 64 records; discard unneeded records first"))
      (unless hermes-chat--image-draft-record
        (setq hermes-chat--image-draft-record
              (list :state 'draft :images nil :content nil :session-id nil
                    :session-key nil :assistant-id nil :client nil :lifetime nil :generation nil
                    :owner (current-buffer)))
        (let ((record hermes-chat--image-draft-record))
          (with-current-buffer recovery
            (setq hermes-chat--image-records (append records (list record))))))
      (setq hermes-chat--draft-images
            (append hermes-chat--draft-images
                    (list (list :bytes (copy-sequence bytes) :type kind
                                :mime (concat "image/" (symbol-name kind))))))
      (setf (plist-get hermes-chat--image-draft-record :images) hermes-chat--draft-images)
      (message "%d image(s) staged locally; preview or remove before sending"
               (length hermes-chat--draft-images)))))

(defun hermes-chat-attach-image-file (file)
  "Read local image FILE bytes into the composer, without uploading."
  (interactive "fImage file: ")
  (when (file-remote-p file) (user-error "Choose a local image file"))
  (let ((origin (current-buffer)))
    (condition-case nil
	(with-temp-buffer
          (set-buffer-multibyte nil)
          ;; Bounded read also catches a file growing after selection.
          (insert-file-contents-literally file nil 0
					  (1+ (min hermes-chat-image-max-bytes
                                                   (* 2 1024 1024))))
          (let ((bytes (buffer-string)))
            (with-current-buffer origin
              (hermes-chat--image-stage bytes))))
      (file-error (user-error "Cannot read the selected image file")))))

(defun hermes-chat-paste-image ()
  "Stage PNG bytes from the editor's clipboard, not the remote clipboard.
Signal a user error when this graphical backend has no PNG selection."
  (interactive)
  (let ((bytes (condition-case nil
                   (gui-get-selection 'CLIPBOARD 'image/png)
                 (error nil))))
    (unless (stringp bytes)
      (user-error "No PNG clipboard data; attach a local image file instead"))
    (hermes-chat--image-stage bytes)))

(defun hermes-chat-remove-image (index)
  "Remove the one-based INDEX image from the local draft only."
  (interactive
   (list (string-to-number
          (completing-read "Remove image: "
                           (mapcar #'number-to-string
                                   (number-sequence 1 (length hermes-chat--draft-images)))
                           nil t))))
  (unless (and (integerp index) (> index 0) (<= index (length hermes-chat--draft-images)))
    (user-error "No such draft image"))
  (setq hermes-chat--draft-images
        (cl-loop for image in hermes-chat--draft-images for n from 1
                 unless (= n index) collect image))
  (setf (plist-get hermes-chat--image-draft-record :images) hermes-chat--draft-images)
  (unless hermes-chat--draft-images
    (let ((record hermes-chat--image-draft-record))
      (when (buffer-live-p hermes-chat--image-recovery-buffer)
        (with-current-buffer hermes-chat--image-recovery-buffer
          (setq hermes-chat--image-records (delq record hermes-chat--image-records))))
      (setq hermes-chat--image-draft-record nil)))
  (message "%d draft image(s) remain" (length hermes-chat--draft-images)))

(defun hermes-chat--image-insert-preview (image)
  "Insert a safe local preview of IMAGE, or a text fallback."
  (insert (format "%s · %d bytes\n" (plist-get image :mime)
                  (length (plist-get image :bytes))))
  (when (display-images-p)
    (condition-case nil
        (insert-image (create-image (plist-get image :bytes)
                                    (plist-get image :type) t
                                    :max-width 512 :max-height 512)
                      "[image preview]")
      (error (insert "[Preview unavailable]"))))
  (insert "\n"))

(defun hermes-chat-preview-images ()
  "Show local drafts and recoverable image sends without network access.
Recovery retains bytes after acknowledgment because backend interruption can
still discard queued input.  Use the view's restore or discard command;
restoring never sends automatically.  Bytes are not saved across Emacs exit."
  (interactive)
  (let ((buffer (hermes-chat--image-recovery)))
    (when hermes-chat--image-draft-record
      (setf (plist-get hermes-chat--image-draft-record :content)
            (hermes-chat-input-string)))
    (with-current-buffer buffer (hermes-chat-image-recovery-refresh))
    (pop-to-buffer buffer)))

(defun hermes-chat-image-recovery-refresh ()
  "Refresh this local image recovery view."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Local images — r: restore to chat, d: discard, g: refresh, q: quit\n"
            "Retained in memory only.  Restore never sends; retry may duplicate a turn.\n\n")
    (cl-loop for record in hermes-chat--image-records for index from 1 do
             (insert (format "%d. %s\n%s\n" index (plist-get record :state)
                             (or (plist-get record :content) "")))
             (mapc #'hermes-chat--image-insert-preview (plist-get record :images)))))

(defun hermes-chat--image-read-record ()
  "Read a record from this recovery view."
  (nth (1- (string-to-number
            (completing-read "Image record: "
                             (mapcar #'number-to-string
                                     (number-sequence 1 (length hermes-chat--image-records)))
                             nil t)))
       hermes-chat--image-records))

(defun hermes-chat--image-copy-images (images)
  "Return independent image records and binary strings for IMAGES."
  (mapcar (lambda (image)
            (let ((copy (copy-sequence image)))
              (setf (plist-get copy :bytes) (copy-sequence (plist-get image :bytes)))
              copy))
          images))

(defun hermes-chat-image-recovery-restore ()
  "Restore a selected record to a chosen chat, without sending it.
Refuse to overwrite newer text or images.  Ambiguous sends require a new
session before retrying; the original backend may already have accepted them."
  (interactive)
  (let* ((source (current-buffer))
         (record (hermes-chat--image-read-record))
         (snapshot (let ((copy (copy-tree record)))
                     (setf (plist-get copy :images)
                           (hermes-chat--image-copy-images (plist-get record :images))
                           (plist-get copy :content)
                           (copy-sequence (plist-get record :content)))
                     copy))
         (origin (get-buffer
                  (completing-read
                   "Restore into chat: "
                   (mapcar #'buffer-name
                           (seq-filter (lambda (buffer)
                                         (with-current-buffer buffer
                                           (derived-mode-p 'hermes-chat-mode)))
                                       (buffer-list)))
                   nil t))))
    (unless (and record (buffer-live-p origin))
      (user-error "Select a live chat buffer; retained bytes have not changed"))
    (with-current-buffer origin
      (let ((lifetime hermes-chat--lifecycle-generation)
            (generation hermes-chat--transport-generation)
            (client hermes-chat--dashboard-client)
            (session hermes-chat--dashboard-active-session-id)
            (marker hermes-chat--input-marker))
	(unless (and (derived-mode-p 'hermes-chat-mode)
                     (string-empty-p (hermes-chat-input-string))
                     (null hermes-chat--draft-images))
          (user-error "Clear the chat composer before restoring images"))
	(when (memq (plist-get record :state) '(local uploading attaching submitted))
          (user-error "Remove the queued entry before restoring its images"))
	(when (and (plist-get record :session-id)
                   (equal (plist-get record :session-id) hermes-chat--dashboard-active-session-id))
          (user-error "Use a new session before retrying an uncertain image send"))
	(unless (yes-or-no-p "Restore images despite possible prior delivery? ")
          (user-error "Restore canceled"))
	;; Minibuffer input can run timers and other commands.
	(unless (and (buffer-live-p source)
                     (memq record (buffer-local-value 'hermes-chat--image-records source))
                     (equal snapshot record)
                     (derived-mode-p 'hermes-chat-mode)
                     (eql lifetime hermes-chat--lifecycle-generation)
                     (eql generation hermes-chat--transport-generation)
                     (eq client hermes-chat--dashboard-client)
                     (equal session hermes-chat--dashboard-active-session-id)
                     (eq marker hermes-chat--input-marker)
                     (string-empty-p (hermes-chat-input-string))
                     (null hermes-chat--draft-images)
                     (not (memq (plist-get record :state)
				'(local uploading attaching submitted))))
          (user-error "Composer or image record changed during confirmation"))
	(let* ((recovery (hermes-chat--image-recovery))
               (records (buffer-local-value 'hermes-chat--image-records recovery))
               (used (apply #'+ (mapcar (lambda (item)
					  (hermes-chat--image-bytes (plist-get item :images)))
					records)))
               (copy (list :state 'draft :images (hermes-chat--image-copy-images (plist-get snapshot :images))
                           :content nil :session-id nil :session-key nil
                           :assistant-id nil :client nil :lifetime nil :generation nil
                           :owner (current-buffer))))
          (when (or (>= (length records) 64)
                    (> (+ used (hermes-chat--image-bytes (plist-get record :images)))
                       hermes-chat--image-total-limit))
            (user-error "Target image recovery is full"))
          (setq hermes-chat--draft-images (plist-get copy :images)
		hermes-chat--image-draft-record copy)
          (with-current-buffer recovery
            (setq hermes-chat--image-records (append records (list copy)))))
	(goto-char (point-max))
	(insert (or (plist-get snapshot :content) ""))))
    (pop-to-buffer origin)))

(defun hermes-chat--image-record-owned-p (record)
  "Return non-nil if RECORD is still owned by a live composer or queue."
  (let ((owner (plist-get record :owner)))
    (and (buffer-live-p owner)
         (with-current-buffer owner
           (or (eq record hermes-chat--image-draft-record)
               (seq-some (lambda (entry) (eq record (plist-get entry :image-record)))
                         hermes-chat--queued-messages))))))

(defun hermes-chat--image-forget-bytes (record)
  "Replace RECORD's remaining staging lock with a byte-free uncertainty marker."
  (let ((key (plist-get record :session-key)))
    (when (eq record (gethash key hermes-chat--image-session-blocks))
      (puthash key (list :state 'uncertain) hermes-chat--image-session-blocks))))

(defun hermes-chat--image-recovery-kill-p ()
  "Confirm loss of retained bytes, refusing to orphan live drafts or queues."
  (if (seq-some #'hermes-chat--image-record-owned-p hermes-chat--image-records)
      (progn (message "Remove images from their composer or queue first") nil)
    (or (null hermes-chat--image-records)
        (and (yes-or-no-p "Discard all retained images in this recovery buffer? ")
             (not (seq-some #'hermes-chat--image-record-owned-p
                            hermes-chat--image-records))))))

(defun hermes-chat--image-recovery-killed ()
  "Drop byte references from staging locks when recovery is deliberately killed."
  (mapc #'hermes-chat--image-forget-bytes hermes-chat--image-records))

(defun hermes-chat-image-recovery-discard ()
  "Discard a completed recovery record after confirmation.
Draft and pending records must be removed through their owning chat."
  (interactive)
  (let ((source (current-buffer))
        (record (hermes-chat--image-read-record)))
    (when (hermes-chat--image-record-owned-p record)
      (user-error "This record is still owned by the composer or queue"))
    (when (and record (yes-or-no-p "Discard retained image bytes? "))
      (unless (and (eq source (current-buffer))
                   (derived-mode-p 'hermes-chat-image-recovery-mode)
                   (memq record hermes-chat--image-records)
                   (not (hermes-chat--image-record-owned-p record)))
        (user-error "Image record changed during confirmation"))
      (hermes-chat--image-forget-bytes record)
      (setq hermes-chat--image-records (delq record hermes-chat--image-records))
      (hermes-chat-image-recovery-refresh))))

(defvar-keymap hermes-chat-image-recovery-mode-map
  :parent special-mode-map
  "g" #'hermes-chat-image-recovery-refresh
  "r" #'hermes-chat-image-recovery-restore
  "d" #'hermes-chat-image-recovery-discard)

(define-derived-mode hermes-chat-image-recovery-mode special-mode "Hermes Images"
  "Inspect retained local images and recover sends manually."
  (add-hook 'kill-buffer-query-functions #'hermes-chat--image-recovery-kill-p nil t)
  (add-hook 'kill-buffer-hook #'hermes-chat--image-recovery-killed nil t))

(defun hermes-chat--images-invalidate ()
  "Preserve image drafts and mark unfinished sends uncertain on teardown."
  (maphash
   (lambda (_key records)
     (dolist (record records)
       (when (and (listp record) (eq (plist-get record :owner) (current-buffer)))
         (hermes-chat--image-admission-finish record t))))
   hermes-chat--image-prior-submits)
  (when hermes-chat--image-draft-record
    (setf (plist-get hermes-chat--image-draft-record :content) (hermes-chat-input-string)))
  (when (buffer-live-p hermes-chat--image-recovery-buffer)
    (with-current-buffer hermes-chat--image-recovery-buffer
      (dolist (record hermes-chat--image-records)
        (when (or (not (memq (plist-get record :state) '(draft accepted)))
                  (eq record (gethash (plist-get record :session-key)
                                      hermes-chat--image-session-blocks)))
          (setf (plist-get record :state) 'uncertain))))))

(defun hermes-chat--images-prepare (client record context continue reject)
  "Upload RECORD bytes for CLIENT and CONTEXT, then call CONTINUE.
REJECT receives only a fixed safe message.  Every callback proves the original
buffer, lifetime, client, session and submit context.  Uncertain attachment
failures are not retried; retained records permit explicit manual recovery."
  (let ((buffer (current-buffer))
        (lifetime hermes-chat--lifecycle-generation)
        (generation hermes-chat--transport-generation)
        (session hermes-chat--dashboard-active-session-id)
        (profile hermes-chat--profile))
    (cl-labels
        ((current-p ()
           (and (buffer-live-p buffer)
                (with-current-buffer buffer
                  (and (derived-mode-p 'hermes-chat-mode)
                       (eql lifetime hermes-chat--lifecycle-generation)
                       (eql generation hermes-chat--transport-generation)
                       (eq client hermes-chat--dashboard-client)
                       (equal session hermes-chat--dashboard-active-session-id)
                       (equal profile hermes-chat--profile)
                       (eq context hermes-chat--unsettled-submit-context)))))
         (fail (&rest _)
           (when (current-p)
             (with-current-buffer buffer
               (if (eq (plist-get record :state) 'uploading)
                   (progn (setf (plist-get record :state) 'local)
                          (hermes-chat--images-release record)
                          (funcall reject "Image upload failed; bytes retained in queue"))
                 (setf (plist-get record :state) 'uncertain)
                 (funcall reject "Image send uncertain; bytes retained in image recovery")))))
         (attach (paths)
           (when (current-p)
             (with-current-buffer buffer
               (if (null paths)
                   (progn (setf (plist-get record :state) 'submitted)
                          (funcall continue))
                 (setf (plist-get record :state) 'attaching)
                 (condition-case nil
                     (hermes-dashboard-transport-image-attach
                      client (car paths) :session-id session
                      :resolve (lambda (result)
                                 (when (current-p)
                                   (if (and (eq t (hermes-transport--get result 'attached))
                                            (equal (car paths) (hermes-transport--get result 'path)))
                                       (attach (cdr paths))
                                     (fail))))
                      :reject #'fail)
                   (error (fail)))))))
         (upload (images paths)
           (when (current-p)
             (with-current-buffer buffer
               (if (null images)
                   (attach (reverse paths))
                 (let* ((image (car images))
                        (data (concat "data:" (plist-get image :mime) ";base64,"
                                      (base64-encode-string (plist-get image :bytes) t))))
                   (condition-case nil
                       (hermes--promise-then
                        (hermes-dashboard-transport-api-request-async
                         "POST" "/api/chat/image-upload" :client client
                         :query (and profile `((profile . ,profile)))
                         :body `((data_url . ,data) (filename . "image"))
                         :secrets (list data) :timeout 60)
                        (lambda (result)
                          (when (current-p)
                            (let ((path (hermes-transport--get result 'path)))
                              (if (and (eq t (hermes-transport--get result 'ok))
                                       (stringp path) (string-prefix-p "/" path)
                                       (not (string-match-p "[\0\n\r]" path)))
                                  (upload (cdr images) (cons path paths))
                                (fail)))))
                        #'fail)
                     (error (fail)))))))))
      (if (not (eq (plist-get record :state) 'local))
          (funcall reject "Image send cannot be retried automatically; use image recovery")
        (if (or (hermes-chat--images-inhibit)
                (gethash (hermes-chat--image-session-key) hermes-chat--image-prior-submits))
            (funcall reject "Prior session submission pending or uncertain; images retained")
          (setf (plist-get record :state) 'uploading
                (plist-get record :session-id) session
                (plist-get record :session-key) (hermes-chat--image-session-key)
                (plist-get record :owner) buffer
                (plist-get record :client) client
                (plist-get record :lifetime) lifetime
                (plist-get record :generation) generation
                (plist-get record :assistant-id) (plist-get context :assistant-id))
          (puthash (plist-get record :session-key) record hermes-chat--image-session-blocks)
          (upload (plist-get record :images) nil))))))

(provide 'hermes-chat-images)
;;; hermes-chat-images.el ends here
