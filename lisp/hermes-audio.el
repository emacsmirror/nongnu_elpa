;;; hermes-audio.el --- Optional client-local audio relays -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, multimedia
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Record on the Emacs machine, relay bytes to the owning dashboard, and
;; insert transcription only into its unchanged draft.  Speech is explicit,
;; selected settled assistant text, never continuous listening or auto-send.
;; Configure trusted foreground recorder/player argv; neither is auto-detected.
;; Cancellation retires local effects, not a provider request already uploaded.

;;; Code:

(require 'hermes-chat)

(defgroup hermes-audio nil
  "Optional client-local dictation and read-aloud."
  :group 'hermes)

(defcustom hermes-audio-recorder-command nil
  "Local foreground recorder argv, or nil when unavailable.
The program must emit audio bytes on stdout, diagnostics on stderr, and
finalize on SIGINT.  Do not use a shell, background children or remote paths.
For example, use arecord -q -t wav -f S16_LE -r 16000 as separate arguments.
Configure `hermes-audio-recording-mime-type' to match.  Recording always
requires explicit consent; loading this library never opens an audio device."
  :type '(repeat string) :group 'hermes-audio)

(defcustom hermes-audio-player-command nil
  "Local foreground player argv, or nil when unavailable.
One argument must be exactly %f, replaced with an owned private audio file.
For example, use ffplay -nodisp -autoexit -loglevel quiet %f as arguments.
The trusted program must support the allowlisted formats and not daemonize."
  :type '(repeat string) :group 'hermes-audio)

(defcustom hermes-audio-recording-mime-type "audio/wav"
  "MIME type emitted by the configured local recorder."
  :type '(choice (const "audio/wav") (const "audio/flac")
                 (const "audio/ogg") (const "audio/mpeg") (const "audio/webm"))
  :group 'hermes-audio)

(defcustom hermes-audio-max-bytes (* 8 1024 1024)
  "Maximum recording or decoded playback bytes, at most 25 MiB.
Playback envelopes are checked before decoding, but HTTP retrieval itself
uses the shared dashboard transport and is not streaming-size-limited."
  :type 'natnum :group 'hermes-audio)

(defcustom hermes-audio-max-seconds 120
  "Maximum local recording or playback duration in seconds.
Timers require Emacs event-loop progress.  Expiry cancels rather than uploads.
Relay requests have a separate 120-second local deadline."
  :type 'natnum :group 'hermes-audio)

(defconst hermes-audio--extensions
  '(("audio/wav" . ".wav") ("audio/flac" . ".flac")
    ("audio/ogg" . ".ogg") ("audio/mpeg" . ".mp3") ("audio/webm" . ".webm"))
  "Allowlisted local audio MIME types and private file suffixes.")

(defvar-local hermes-audio--operation nil "Exact active audio operation.")

(defun hermes-audio--destination ()
  "Return this chat's exact routing and lifetime values."
  (let ((client hermes-chat--dashboard-client))
    (list hermes-chat--lifecycle-generation
          (hermes-dashboard-transport-client-generation client)
          (hermes-dashboard-transport--api-client-base-url client)
          (hermes-instance-id hermes-instance) (hermes-instance-name hermes-instance)
          (hermes-instance-url hermes-instance) hermes-chat--profile hermes-chat--session-id
          hermes-chat--dashboard-active-session-id)))

(defun hermes-audio--ready-p ()
  "Return non-nil if this is an owned, attached dashboard chat."
  (let ((client hermes-chat--dashboard-client))
    (and (hermes-buffer--owned-p 'hermes-chat-mode)
         (hermes-dashboard-transport-client-p client)
         (hermes-dashboard-transport-client-ready-p client)
         (not (hermes-dashboard-transport-client-stopping-p client))
         (not (hermes-dashboard-transport-client-reconnecting-p client))
         hermes-chat--dashboard-session-ready-p
         (stringp hermes-chat--dashboard-active-session-id))))

(defun hermes-audio--current-p (op)
  "Return non-nil while OP owns the exact attached chat."
  (and (not (plist-get op :retired))
       (<= (float-time) (plist-get op :deadline))
       (buffer-live-p (plist-get op :buffer))
       (with-current-buffer (plist-get op :buffer)
         (and (eq op hermes-audio--operation) (hermes-audio--ready-p)
              (eq hermes-buffer--owner (plist-get op :claim))
              (eq hermes-chat--dashboard-client (plist-get op :client))
              (equal (hermes-audio--destination) (plist-get op :destination))))))

(defun hermes-audio--retire (op)
  "Retire OP before stopping processes, timers and private files."
  (unless (plist-get op :retired)
    (setf (plist-get op :retired) t (plist-get op :chunks) nil)
    (when (buffer-live-p (plist-get op :buffer))
      (with-current-buffer (plist-get op :buffer)
        (when (eq op hermes-audio--operation)
          (setq hermes-audio--operation nil)
          (remove-hook 'after-change-functions #'hermes-audio--draft-changed t)
          (dolist (hook '(kill-buffer-hook change-major-mode-hook
                         after-set-visited-file-name-hook
                         hermes-chat-lifecycle-invalidation-hook))
            (remove-hook hook #'hermes-audio-cancel t))
          (remove-hook 'hermes-chat-state-change-hook #'hermes-audio--changed t))))
    (when (plist-get op :subscription)
      (hermes-dashboard-transport-unsubscribe
       (plist-get op :client) (plist-get op :subscription)))
    (when (timerp (plist-get op :watch))
      (cancel-timer (plist-get op :watch)))
    (dolist (key '(:process :error-process))
      (when (process-live-p (plist-get op key))
        (delete-process (plist-get op key))))
    (when (plist-get op :directory)
      ;; This directory was created locally.  A newly installed handler must
      ;; not redirect cleanup to another host or receive the private path.
      (let ((file-name-handler-alist nil))
        (delete-directory (plist-get op :directory) t)))))

;;;###autoload
(defun hermes-audio-cancel ()
  "Cancel this chat's local audio work and discard late replies.
This cannot revoke bytes already uploaded or stop an in-flight provider."
  (interactive nil hermes-chat-mode)
  (when hermes-audio--operation
    (hermes-audio--retire hermes-audio--operation)
    (message "Local audio cancelled")))

(defun hermes-audio--changed ()
  "Retire local audio after chat owner replacement."
  (when (and hermes-audio--operation
             (not (hermes-audio--current-p hermes-audio--operation)))
    (hermes-audio-cancel)))

(defun hermes-audio--watch (op)
  "Enforce OP's local ownership and deadline without remote polling."
  (when (or (not (hermes-audio--current-p op))
            (> (float-time) (plist-get op :deadline)))
    (hermes-audio--retire op)))

(defun hermes-audio--draft-changed (_begin end _old-length)
  "Mark edits ending at END as conflicting with pending dictation."
  (when (and hermes-audio--operation (marker-position hermes-chat--input-marker)
             (>= end hermes-chat--input-marker))
    (setf (plist-get hermes-audio--operation :edited) t)))

(defun hermes-audio--command (command player)
  "Validate trusted local COMMAND argv, requiring %f for PLAYER."
  (unless (and (consp command) (seq-every-p #'stringp command)
               (not (file-remote-p (car command)))
               (executable-find (car command))
               (or (not player) (= 1 (cl-count "%f" command :test #'equal))))
    (user-error "Audio unavailable: configure hermes-audio-%s-command with a local executable"
                (if player "player" "recorder")))
  (mapcar #'copy-sequence command))

(defun hermes-audio--local-path-p (path)
  "Return non-nil if PATH is absolute, local and not handler-managed."
  (and (stringp path) (file-name-absolute-p path)
       (not (string-prefix-p "~" path))
       ;; A nil operation in `find-file-name-handler' misses handlers whose
       ;; `operations' property restricts dispatch, such as write-only ones.
       (not (seq-some (lambda (entry) (string-match-p (car entry) path))
                      file-name-handler-alist))
       (not (file-remote-p path))))

(defun hermes-audio--check-local-owner (op &optional file)
  "Require OP's exact owner and captured local scratch before using FILE."
  (unless (and (hermes-audio--current-p op)
               (hermes-audio--local-path-p (plist-get op :scratch-root))
               (or (not file) (hermes-audio--local-path-p file)))
    (hermes-audio--retire op)
    (user-error "Audio owner or local scratch is no longer available")))

(defun hermes-audio--begin ()
  "Capture and publish a new audio owner before any recursive input."
  (unless (hermes-audio--ready-p)
    (user-error "Connect and attach this Hermes chat before using audio"))
  (when hermes-audio--operation (user-error "Stop or cancel current audio first"))
  (unless (hermes-audio--local-path-p temporary-file-directory)
    (user-error "Audio requires an absolute local temporary-file-directory without handlers"))
  (unless (and (integerp hermes-audio-max-bytes)
               (< 0 hermes-audio-max-bytes) (<= hermes-audio-max-bytes (* 25 1024 1024))
               (integerp hermes-audio-max-seconds) (< 0 hermes-audio-max-seconds)
               (<= hermes-audio-max-seconds 600))
    (user-error "Audio limits require positive bytes up to 25 MiB and 1–600 seconds"))
  (let ((op (list :buffer (current-buffer) :claim hermes-buffer--owner
                  :scratch-root (copy-sequence (file-name-as-directory temporary-file-directory))
                  :client hermes-chat--dashboard-client
                  :destination (hermes-audio--destination)
                  :profile (and hermes-chat--profile (copy-sequence hermes-chat--profile))
                  :marker hermes-chat--input-marker :edited nil
                  :draft (save-restriction (widen) (hermes-chat-input-string))
                  :max-bytes hermes-audio-max-bytes :seconds hermes-audio-max-seconds
                  :deadline (+ (float-time) 120) :state 'consent :retired nil
                  :process nil :error-process nil :chunks nil :size 0 :mime nil :player nil
                  :watch nil :subscription nil :directory nil)))
    ;; Capture routing strings by value, not by mutable string identity.
    (setf (plist-get op :destination)
          (mapcar (lambda (value) (if (stringp value) (copy-sequence value) value))
                  (plist-get op :destination)))
    (setq hermes-audio--operation op)
    (dolist (hook '(kill-buffer-hook change-major-mode-hook
                   after-set-visited-file-name-hook hermes-chat-lifecycle-invalidation-hook))
      (add-hook hook #'hermes-audio-cancel nil t))
    (add-hook 'hermes-chat-state-change-hook #'hermes-audio--changed nil t)
    (add-hook 'after-change-functions #'hermes-audio--draft-changed nil t)
    (setf (plist-get op :subscription)
          (hermes-dashboard-transport-subscribe
           (plist-get op :client) nil (lambda () (hermes-audio--retire op)))
          (plist-get op :watch) (run-at-time 0.2 0.2 #'hermes-audio--watch op))
    op))

(defun hermes-audio--consent (op action)
  "Ask consent for ACTION under OP's exact destination and local device."
  (and (yes-or-no-p
        (format "%s on this Emacs machine; relay to %s (profile %s)? "
                action (nth 2 (plist-get op :destination))
                (or (plist-get op :profile) "dashboard launch profile")))
       (hermes-audio--current-p op)))

(defun hermes-audio--filter (op chunk)
  "Retain binary recorder CHUNK within OP's byte limit."
  (when (hermes-audio--current-p op)
    (let ((size (+ (plist-get op :size) (string-bytes chunk))))
      (if (> size (plist-get op :max-bytes))
          (progn (hermes-audio--retire op) (message "Recording exceeded byte limit"))
        (setf (plist-get op :size) size)
        (push chunk (plist-get op :chunks))))))

(defun hermes-audio--spawn (op command &optional record)
  "Start OP's local foreground COMMAND, collecting stdout when RECORD."
  (hermes-audio--check-local-owner op)
  (let ((default-directory (plist-get op :scratch-root)))
    (setf (plist-get op :error-process)
          (make-pipe-process :name "hermes-audio-stderr" :noquery t
                             :buffer nil :filter #'ignore))
    (hermes-audio--check-local-owner op)
    (setf (plist-get op :process)
          (make-process
           :name "hermes-local-audio" :command command :connection-type 'pipe
           :coding 'binary :noquery t :buffer nil :stderr (plist-get op :error-process)
           :filter (if record (lambda (_ chunk) (hermes-audio--filter op chunk)) #'ignore)
           :sentinel (lambda (process _event)
                       (when (memq (process-status process) '(exit signal))
                         (hermes-audio--exited op process record)))))))

;;;###autoload
(defun hermes-audio-record ()
  "Ask consent and start the configured Emacs-side recorder.
Use `hermes-audio-stop' to finalize and upload, or `hermes-audio-cancel'
to discard.  Dictation never sends a chat message."
  (interactive nil hermes-chat-mode)
  (let ((command (hermes-audio--command hermes-audio-recorder-command nil))
        (mime hermes-audio-recording-mime-type))
    (unless (assoc mime hermes-audio--extensions) (user-error "Unsupported recording MIME"))
    (let ((op (hermes-audio--begin)) started)
      (unwind-protect
          (when (hermes-audio--consent op "Record local audio")
            (setf (plist-get op :mime) (copy-sequence mime)
                  (plist-get op :state) 'recording
                  (plist-get op :deadline) (+ (float-time) (plist-get op :seconds)))
            (hermes-audio--spawn op command t)
            (setq started t)
            (message "Recording locally; Stop uploads to the labelled backend, Cancel discards"))
        (unless started (hermes-audio--retire op))))))

;;;###autoload
(defun hermes-audio-stop ()
  "Finalize the local recorder and upload bounded audio for draft transcription.
The recorder gets SIGINT and two seconds to exit; timeout discards bytes.
Stopping playback simply cancels it."
  (interactive nil hermes-chat-mode)
  (let ((op hermes-audio--operation))
    (unless (and op (hermes-audio--current-p op)) (user-error "No current local audio"))
    (if (not (eq (plist-get op :state) 'recording))
        (hermes-audio-cancel)
      (setf (plist-get op :state) 'stopping
            (plist-get op :deadline) (+ (float-time) 2))
      (condition-case err
          (interrupt-process (plist-get op :process))
        ((error quit) (hermes-audio--retire op) (signal (car err) (cdr err)))))))

(defun hermes-audio--exited (op process record)
  "Settle OP after PROCESS exits; upload only explicitly stopped RECORD."
  (when (and (eq process (plist-get op :process)) (not (plist-get op :retired)))
    (if (and record (hermes-audio--current-p op)
             (eq (plist-get op :state) 'stopping)
             (or (= (process-exit-status process) 0)
                 (and (eq (process-status process) 'signal)
                      (= (process-exit-status process) 2))))
        (if (= 0 (plist-get op :size))
            (progn (hermes-audio--retire op) (message "Recording was empty; nothing uploaded"))
          (let* ((mime (plist-get op :mime))
                 (bytes (apply #'concat (nreverse (plist-get op :chunks))))
                 (body `((data_url . ,(concat "data:" mime ";base64,"
                                              (base64-encode-string bytes t)))
                         (mime_type . ,mime))))
            (setf (plist-get op :chunks) nil)
            (hermes-audio--relay op "/api/audio/transcribe" body #'hermes-audio--transcribed)))
      (let ((current (hermes-audio--current-p op)))
        (hermes-audio--retire op)
        (when current
          (message (if record "Recorder ended without Stop; nothing uploaded"
                     (if (= 0 (process-exit-status process)) "Local playback completed"
                       "Local playback failed"))))))))

(defun hermes-audio--relay (op path body accept)
  "POST BODY to PATH on OP's retained client and call ACCEPT if still owned."
  (setf (plist-get op :state) 'relay (plist-get op :deadline) (+ (float-time) 120))
  (condition-case nil
      (hermes--promise-catch
       (hermes--promise-then
        (hermes-dashboard-transport-api-request-async
         "POST" path :body body :client (plist-get op :client) :timeout 120
         :query (and (plist-get op :profile) `((profile . ,(plist-get op :profile))))
         :current-p (lambda () (hermes-audio--current-p op)))
        (lambda (result)
          (when (hermes-audio--current-p op) (funcall accept op result))))
       (lambda (_reason)
         (let ((current (hermes-audio--current-p op)))
           (hermes-audio--retire op)
           (when current (message "Audio relay failed; check backend audio setup and retry explicitly")))))
    ((error quit) (hermes-audio--retire op) (message "Audio relay unavailable"))))

(defun hermes-audio--draft-current-p (op)
  "Return non-nil if OP still owns an untouched draft."
  (and (hermes-audio--current-p op) (not (plist-get op :edited))
       (eq hermes-chat--input-marker (plist-get op :marker))
       (equal (hermes-chat-input-string) (plist-get op :draft))))

(defun hermes-audio--insert (op text)
  "Insert TEXT if OP retains its draft across native modification hooks."
  (when (hermes-audio--draft-current-p op)
    (save-excursion
      (goto-char (point-max))
      (combine-change-calls (point) (point)
        (when (hermes-audio--draft-current-p op)
          (goto-char (point-max))
          (insert (if (string-empty-p (plist-get op :draft)) "" "\n") text)
          t)))))

(defun hermes-audio--recover (text)
  "Display literal TEXT for manual copying without selecting its buffer."
  (let ((buffer (generate-new-buffer "*Hermes dictation recovery*")))
    (with-current-buffer buffer
      (insert text) (special-mode)
      (setq-local header-line-format "Dictation — copy manually; not sent"))
    (display-buffer buffer)))

(defun hermes-audio--transcribed (op result)
  "Insert RESULT's transcript only into OP's unchanged draft, never send."
  (let ((text (hermes-transport--get result 'transcript)))
    (unless (and (eq t (hermes-transport--get result 'ok)) (stringp text)
                 (<= (string-bytes text) 65536))
      (error "Invalid transcription response"))
    (unwind-protect
        (with-current-buffer (plist-get op :buffer)
          (save-restriction
            (widen)
            (cond
             ((string-empty-p text) (message "No speech detected"))
             ((hermes-audio--insert op text)
              (message "Dictation inserted as draft; review before Send"))
             ((hermes-audio--current-p op) (hermes-audio--recover text)))))
      (hermes-audio--retire op))))

(defun hermes-audio--decode (op result)
  "Validate RESULT's bounded audio envelope for OP and return MIME and bytes."
  (let* ((mime (hermes-transport--get result 'mime_type))
         (data (hermes-transport--get result 'data_url))
         (prefix (and (stringp mime) (concat "data:" mime ";base64,"))))
    (unless (and (eq t (hermes-transport--get result 'ok))
                 (assoc mime hermes-audio--extensions) (stringp data)
                 (string-prefix-p prefix data)
                 (<= (length data) (+ (length prefix)
                                      (* 4 (/ (+ (plist-get op :max-bytes) 2) 3)))))
      (error "Invalid audio envelope"))
    (let* ((encoded (substring data (length prefix)))
           (bytes (base64-decode-string encoded)))
      (unless (and (> (length bytes) 0) (<= (length bytes) (plist-get op :max-bytes))
                   (equal encoded (base64-encode-string bytes t)))
        (error "Invalid audio bytes"))
      (cons mime bytes))))

(defun hermes-audio--play (op result)
  "Play validated RESULT bytes locally under OP's retained owner."
  (hermes-audio--check-local-owner op)
  (let* ((audio (hermes-audio--decode op result))
         (prefix (concat (plist-get op :scratch-root) "hermes-audio-"))
         (directory (progn (hermes-audio--check-local-owner op prefix)
                           (make-temp-file prefix t))))
    (setf (plist-get op :directory) directory)
    (hermes-audio--check-local-owner op directory)
    (set-file-modes directory #o700)
    (let ((file (expand-file-name (concat "speech" (cdr (assoc (car audio) hermes-audio--extensions)))
                                  directory))
          (coding-system-for-write 'binary))
      (hermes-audio--check-local-owner op file)
      (write-region (cdr audio) nil file nil 'silent)
      ;; Native post-annotation hooks may retire this owner or its scratch.
      (hermes-audio--check-local-owner op file)
      (set-file-modes file #o600)
      (setf (plist-get op :state) 'playing
            (plist-get op :deadline) (+ (float-time) (plist-get op :seconds)))
      (hermes-audio--spawn op (mapcar (lambda (arg) (if (equal arg "%f") file arg))
                                      (plist-get op :player)))
      (message "Playing locally; HTTP synthesis is complete, playback is not"))))

(defun hermes-audio--choices ()
  "Return numbered settled assistant reply choices for this chat."
  (cl-loop for entry in (hermes-chat--entries) for index from 1
           for text = (plist-get entry :content)
           when (and (eq (plist-get entry :role) 'assistant)
                     (eq (plist-get entry :status) 'done)
                     (stringp text) (not (string-empty-p (string-trim text)))
                     (<= (string-bytes text) 65536))
           collect (cons (format "%d: %s" index (truncate-string-to-width
                                                 (replace-regexp-in-string "\n" " " text) 72 nil nil t))
                         (copy-sequence text))))

;;;###autoload
(defun hermes-audio-read-aloud ()
  "Choose a settled assistant reply, consent to relay, then play locally.
Only the chosen reply is uploaded, never a draft or surrounding transcript."
  (interactive nil hermes-chat-mode)
  (let* ((player (hermes-audio--command hermes-audio-player-command t))
         (op (hermes-audio--begin)) started)
    (unwind-protect
        (let* ((choices (hermes-audio--choices))
               (choice (and choices (completing-read "Read assistant reply: " choices nil t)))
               (text (cdr (assoc choice choices))))
          (unless choices (user-error "No bounded settled assistant reply to read"))
          (when (and text (hermes-audio--current-p op)
                     (hermes-audio--consent op "Play selected speech locally"))
            (setf (plist-get op :player) player)
            (hermes-audio--relay op "/api/audio/speak" `((text . ,text)) #'hermes-audio--play)
            (setq started t)))
      (unless started (hermes-audio--retire op)))))

(provide 'hermes-audio)
;;; hermes-audio.el ends here
