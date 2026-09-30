;;; hermes-audio-tests.el --- Client-local audio tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic bytes and dummy subprocesses only; no audio device or provider.

;;; Code:

(require 'hermes-test-helpers)

(ert-deftest hermes-audio-public-actions-are-discoverable ()
  (should (commandp 'hermes-audio-record))
  (should (commandp 'hermes-audio-stop))
  (should (commandp 'hermes-audio-cancel))
  (should (commandp 'hermes-audio-read-aloud)))

(require 'hermes-audio)

(defconst hermes-audio-test--wave
  (base64-decode-string
   (concat "UklGRmQBAABXQVZFZm10IBAAAAABAAEAgD4AAAB9AAACABAAZGF0YUAB"
           (make-string 428 ?A) "AA=="))
  "Synthetic PCM silence, never captured from a device.")

(defun hermes-audio-test--recorder ()
  "Return a dummy recorder emitting a prerecorded synthetic WAV."
  (list (or (executable-find "python3") (ert-skip "Python unavailable")) "-c"
        (concat "import sys,signal,base64; "
                "signal.signal(signal.SIGINT,lambda *_:sys.exit(0)); "
                "sys.stderr.write('diagnostic-not-audio\\n');sys.stderr.flush(); "
                "sys.stdout.buffer.write(base64.b64decode('"
                (base64-encode-string hermes-audio-test--wave t)
                "'));sys.stdout.flush();signal.pause()")))

(defun hermes-audio-test--player ()
  "Return a dummy player that verifies bytes and awaits stdin, not a speaker."
  (list (or (executable-find "python3") (ert-skip "Python unavailable")) "-c"
        (concat "import sys,base64;assert open(sys.argv[1],'rb').read()==base64.b64decode('"
                (base64-encode-string hermes-audio-test--wave t)
                "');sys.stdin.readline()") "%f"))

(defmacro hermes-audio-test--with-chat (&rest body)
  "Run BODY with an owned attached chat, dummy adapters and deferred HTTP."
  (declare (indent 0) (debug t))
  `(let ((hermes-audio-recorder-command (hermes-audio-test--recorder))
         (hermes-audio-player-command (hermes-audio-test--player))
         (pending (hermes--promise-make)) requests consent)
     (cl-letf (((symbol-function 'yes-or-no-p)
                (lambda (prompt) (setq consent prompt) t))
               ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                (lambda (request) (push request requests) pending))
               ((symbol-function 'websocket-close) #'ignore))
       (hermes-test-with-chat-buffer
         (let ((client (hermes-test--dashboard-client)))
           (setf (hermes-dashboard-transport-client-ready-p client) t
                 (hermes-dashboard-transport-client-base-url client) "http://audio.invalid"
                 (hermes-dashboard-transport-client-token client) "synthetic-token")
           (setq hermes-chat--dashboard-client client
                 hermes-chat--dashboard-session-ready-p t
                 hermes-chat--dashboard-active-session-id "runtime"
                 hermes-chat--session-id "stored"
                 hermes-chat--profile "audio-fixture"
                 hermes-chat--resolved-start-mode 'remote)
           ,@body)))))

(defun hermes-audio-test--record-and-stop ()
  "Record synthetic bytes using the public commands, then stop and await relay."
  (call-interactively #'hermes-audio-record)
  (hermes-test--wait-until
   (lambda () (and hermes-audio--operation
                   (= (plist-get hermes-audio--operation :size)
                      (length hermes-audio-test--wave)))) 3 "dummy recorder bytes")
  (call-interactively #'hermes-audio-stop)
  (hermes-test--wait-until
   (lambda () (eq (plist-get hermes-audio--operation :state) 'relay)) 3 "recording relay"))

(defun hermes-audio-test--reply (pending body)
  "Resolve PENDING with successful HTTP BODY."
  (hermes--promise-resolve pending (list :status 200 :body body)))

(defun hermes-audio-test--envelope ()
  "Return a bounded valid synthetic WAV response."
  `((ok . t) (mime_type . "audio/wav")
    (data_url . ,(concat "data:audio/wav;base64,"
                         (base64-encode-string hermes-audio-test--wave t)))))

(defun hermes-audio-test--choose-reply ()
  "Insert synthetic settled assistant text and invoke the public speech command."
  (hermes-chat--insert-entry
   (list :id "settled" :role 'assistant :status 'done :content "Synthetic reply α"))
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt choices &rest _) (caar choices))))
    (call-interactively #'hermes-audio-read-aloud)))

(ert-deftest hermes-audio-record-stop-exact-bytes-owner-profile-draft-no-send ()
  (hermes-audio-test--with-chat
    (insert "original draft")
    (let ((point-before (point)))
      (hermes-audio-test--record-and-stop)
      (should (string-match-p "this Emacs machine" consent))
      (should (string-match-p "audio.invalid.*audio-fixture" consent))
      (should (= 1 (length requests)))
      (let* ((request (car requests)) (body (plist-get request :body)))
        (should (equal (plist-get request :url)
                       "http://audio.invalid/api/audio/transcribe?profile=audio-fixture"))
        (should (equal (plist-get request :method) "POST"))
        (should (equal (alist-get 'mime_type body) "audio/wav"))
        (should (equal (base64-decode-string (cadr (split-string (alist-get 'data_url body) ",")))
                       hermes-audio-test--wave)))
      (hermes-audio-test--reply pending '((ok . t) (transcript . "dictated α")))
      (should (equal (hermes-chat-input-string) "original draft\ndictated α"))
      (should (= point-before (point)))
      (should-not hermes-audio--operation)
      (should-not hermes-chat--queued-messages)
      (should-not (hermes-chat--entries)))))

(ert-deftest hermes-audio-draft-edit-and-erase-requires-literal-recovery ()
  (hermes-audio-test--with-chat
    (hermes-audio-test--record-and-stop)
    (insert "x") (delete-char -1)
    (let (recovery)
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buffer &rest _) (setq recovery buffer))))
        (hermes-audio-test--reply pending '((ok . t) (transcript . "keep *literal* α"))))
      (unwind-protect
          (progn
            (should (equal "" (hermes-chat-input-string)))
            (should (buffer-live-p recovery))
            (with-current-buffer recovery
              (should (eq major-mode 'special-mode))
              (should (equal "keep *literal* α" (buffer-string)))))
        (when recovery (kill-buffer recovery))))))

(ert-deftest hermes-audio-silence-is-success-without-message-or-draft-change ()
  (hermes-audio-test--with-chat
    (insert "keep")
    (hermes-audio-test--record-and-stop)
    (hermes-audio-test--reply pending '((ok . t) (transcript . "")))
    (should (equal "keep" (hermes-chat-input-string)))
    (should-not hermes-audio--operation)
    (should-not (hermes-chat--entries))))

(ert-deftest hermes-audio-cancel-retirement-and-deadline-stop-real-process ()
  (dolist (retire '(cancel kill mode file transport profile timeout))
    (hermes-audio-test--with-chat
      (hermes-audio-record)
      (let* ((op hermes-audio--operation) (process (plist-get op :process)))
        (should (process-live-p process))
        (pcase retire
          ('cancel (hermes-audio-cancel))
          ('kill (kill-buffer (current-buffer)))
          ('mode (fundamental-mode))
          ('file (set-visited-file-name (expand-file-name "synthetic-notes" temporary-file-directory)))
          ('transport (hermes-dashboard-transport-stop client))
          ('profile (setq hermes-chat--profile "other"))
          ('timeout (setf (plist-get op :deadline) 0)))
        (hermes-test--wait-until (lambda () (not (process-live-p process))) 2 "audio teardown")
        (should (plist-get op :retired))
        (should-not requests)))))

(ert-deftest hermes-audio-cancel-relay-discards-late-transcript ()
  (hermes-audio-test--with-chat
    (hermes-audio-test--record-and-stop)
    (hermes-audio-cancel)
    (insert "successor")
    (hermes-audio-test--reply pending '((ok . t) (transcript . "late")))
    (should (equal "successor" (hermes-chat-input-string)))))

(ert-deftest hermes-audio-player-private-file-completes-and-cleans-up ()
  (hermes-audio-test--with-chat
    (insert "private draft")
    (hermes-audio-test--choose-reply)
    (should (equal (alist-get 'text (plist-get (car requests) :body)) "Synthetic reply α"))
    (should (string-search "/api/audio/speak?profile=" (plist-get (car requests) :url)))
    (hermes-audio-test--reply pending (hermes-audio-test--envelope))
    (let* ((op hermes-audio--operation) (directory (plist-get op :directory))
           (process (plist-get op :process)) (file (expand-file-name "speech.wav" directory)))
      (should (eq (plist-get op :state) 'playing))
      (should (process-live-p process))
      (should (= #o700 (file-modes directory)))
      (should (= #o600 (file-modes file)))
      (should (equal "private draft" (hermes-chat-input-string)))
      (process-send-string process "done\n")
      (hermes-test--wait-until (lambda () (plist-get op :retired)) 3 "dummy playback exit")
      (should (= 0 (process-exit-status process)))
      (should-not (file-exists-p directory))
      (should-not hermes-audio--operation))))

(ert-deftest hermes-audio-cancel-player-and-late-audio-cleanup ()
  (dolist (late '(nil t))
    (hermes-audio-test--with-chat
      (hermes-audio-test--choose-reply)
      (let ((op hermes-audio--operation))
        (unless late (hermes-audio-test--reply pending (hermes-audio-test--envelope)))
        (hermes-audio-cancel)
        (when late (hermes-audio-test--reply pending (hermes-audio-test--envelope)))
        (should-not (process-live-p (plist-get op :process)))
        (should-not (and (plist-get op :directory) (file-exists-p (plist-get op :directory))))
        (should-not hermes-audio--operation)))))

(ert-deftest hermes-audio-invalid-mime-base64-and-size-never-play ()
  (dolist (body '(((ok . t) (mime_type . "text/plain") (data_url . "data:text/plain;base64,YQ=="))
                  ((ok . t) (mime_type . "audio/wav") (data_url . "data:audio/ogg;base64,YQ=="))
                  ((ok . t) (mime_type . "audio/wav") (data_url . "data:audio/wav;base64,!"))
                  ((ok . t) (mime_type . "audio/wav") (data_url . "data:audio/wav;base64,"))))
    (hermes-audio-test--with-chat
      (hermes-audio-test--choose-reply)
      (let ((op hermes-audio--operation))
        (hermes-audio-test--reply pending body)
        (should (plist-get op :retired))
        (should-not (plist-get op :process))
        (should-not (plist-get op :directory)))))
  (hermes-audio-test--with-chat
    (let ((hermes-audio-max-bytes 8))
      (hermes-audio-test--choose-reply)
      (hermes-audio-test--reply pending (hermes-audio-test--envelope))
      (should-not hermes-audio--operation))))

(ert-deftest hermes-audio-recorder-overflow-and-nonzero-exit-never-upload ()
  (hermes-audio-test--with-chat
    (let ((hermes-audio-max-bytes 8))
      (hermes-audio-record)
      (hermes-test--wait-until (lambda () (null hermes-audio--operation)) 3 "recording overflow")
      (should-not requests)))
  (hermes-audio-test--with-chat
    (let ((hermes-audio-recorder-command (list (executable-find "python3") "-c" "raise SystemExit(7)")))
      (hermes-audio-record)
      (hermes-test--wait-until (lambda () (null hermes-audio--operation)) 3 "recorder failure")
      (should-not requests))))

(ert-deftest hermes-audio-absent-and-refused-adapters-preserve-chat ()
  (hermes-audio-test--with-chat
    (insert "keep")
    (let ((hermes-audio-recorder-command nil))
      (should-error (hermes-audio-record) :type 'user-error))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (hermes-audio-record))
    (should-not hermes-audio--operation)
    (should-not requests)
    (should (equal "keep" (hermes-chat-input-string)))))

(ert-deftest hermes-audio-recursive-consent-retirement-prevents-start ()
  (hermes-audio-test--with-chat
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_) (setq hermes-chat--profile "other") t)))
      (hermes-audio-record))
    (should-not hermes-audio--operation)
    (should-not requests))
  (hermes-audio-test--with-chat
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (signal 'quit nil))))
      (should (eq 'cancelled (condition-case nil (hermes-audio-record) (quit 'cancelled)))))
    (should-not hermes-audio--operation)))

(ert-deftest hermes-audio-auth-wait-cancellation-cannot-dispatch ()
  (hermes-audio-test--with-chat
    (setf (hermes-dashboard-transport-client-token client) nil)
    (let ((auth (hermes--promise-make)))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-api-auth-async) (lambda () auth)))
        (hermes-audio-test--choose-reply)
        (should-not requests)
        (hermes-audio-cancel)
        (hermes--promise-resolve auth '(:base-url "http://audio.invalid")))
      (should-not requests)
      (should-not hermes-audio--operation))))

(ert-deftest hermes-audio-native-before-change-retirement-refuses-insertion ()
  (hermes-audio-test--with-chat
    (hermes-audio-test--record-and-stop)
    (add-hook 'before-change-functions
              (lambda (&rest _) (setq hermes-chat--profile "successor")) nil t)
    (hermes-audio-test--reply pending '((ok . t) (transcript . "must not insert")))
    (should (equal "" (hermes-chat-input-string)))
    (should-not hermes-audio--operation)))

(ert-deftest hermes-audio-native-popup-record-and-stop-bindings ()
  (hermes-audio-test--with-chat
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "C-c C-o A r"))
      (should (eq (plist-get hermes-audio--operation :state) 'recording))
      (hermes-test--wait-until
       (lambda () (> (plist-get hermes-audio--operation :size) 0)) 3 "native recorder")
      (execute-kbd-macro (kbd "C-c C-o A s"))
      (hermes-test--wait-until (lambda () requests) 3 "native stop")
      (hermes-audio-test--reply pending '((ok . t) (transcript . "native draft")))
      (should (equal "native draft" (hermes-chat-input-string))))))

(ert-deftest hermes-audio-transcript-update-and-narrowing-preserve-draft-owner ()
  (hermes-audio-test--with-chat
    (insert "draft")
    (hermes-audio-test--record-and-stop)
    (hermes-chat--insert-entry
     (list :id "status" :role 'status :status 'done :content "background update"))
    (narrow-to-region hermes-chat--input-marker (point-max))
    (hermes-audio-test--reply pending '((ok . t) (transcript . "dictation")))
    (save-restriction (widen)
      (should (equal "draft\ndictation" (hermes-chat-input-string))))))

(ert-deftest hermes-audio-relay-errors-and-default-profile-remain-explicit ()
  (hermes-audio-test--with-chat
    (setq hermes-chat--profile nil)
    (hermes-audio-test--choose-reply)
    (should (equal (plist-get (car requests) :url) "http://audio.invalid/api/audio/speak"))
    (hermes--promise-reject pending "synthetic failure")
    (should-not hermes-audio--operation))
  (hermes-audio-test--with-chat
    (hermes-audio-test--record-and-stop)
    (hermes-audio-test--reply pending '((ok . t) (transcript . 42)))
    (should (equal "" (hermes-chat-input-string)))
    (should-not hermes-audio--operation)))

(ert-deftest hermes-audio-speech-reader-and-playback-retirement ()
  (hermes-audio-test--with-chat
    (hermes-chat--insert-entry
     (list :id "settled" :role 'assistant :status 'done :content "Synthetic reply α"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_ choices &rest _args)
                 (setq hermes-chat--profile "replacement") (caar choices))))
      (hermes-audio-read-aloud))
    (should-not requests)
    (should-not consent)
    (should-not hermes-audio--operation))
  (dolist (late '(nil t))
    (hermes-audio-test--with-chat
      (hermes-audio-test--choose-reply)
      (let ((op hermes-audio--operation))
        (unless late (hermes-audio-test--reply pending (hermes-audio-test--envelope)))
        (hermes-dashboard-transport-stop client)
        (when late (hermes-audio-test--reply pending (hermes-audio-test--envelope)))
        (should (plist-get op :retired))
        (should-not (process-live-p (plist-get op :process)))
        (should-not (and (plist-get op :directory) (file-exists-p (plist-get op :directory))))))))

(ert-deftest hermes-audio-stop-uncooperative-recorder-is-bounded ()
  (hermes-audio-test--with-chat
    (let ((hermes-audio-recorder-command
           (list (executable-find "python3") "-c"
                 "import signal,sys;signal.signal(signal.SIGINT,signal.SIG_IGN);sys.stdout.buffer.write(b'fixture');sys.stdout.flush();signal.pause()")))
      (hermes-audio-record)
      (let ((op hermes-audio--operation))
        (hermes-test--wait-until (lambda () (> (plist-get op :size) 0)) 3 "uncooperative recorder")
        (hermes-audio-stop)
        (hermes-test--wait-until (lambda () (plist-get op :retired)) 4 "stop deadline")
        (should-not (process-live-p (plist-get op :process)))
        (should-not requests)))))

(defmacro hermes-audio-test--with-admissions (&rest body)
  "Run BODY, recording real audio process admission directories in STARTS."
  (declare (indent 0) (debug t))
  `(let* (starts
          (observer (lambda (&rest args)
                      (when (equal (plist-get args :name) "hermes-local-audio")
                        (push default-directory starts)))))
     (unwind-protect
         (progn (advice-add 'make-process :before observer) ,@body)
       (advice-remove 'make-process observer))))

(ert-deftest hermes-audio-native-write-retirement-admits-zero-players ()
  (dolist (retirement '(profile generation transport cancel successor))
    (hermes-audio-test--with-chat
      (hermes-audio-test--choose-reply)
      (let* ((op hermes-audio--operation) (owner (current-buffer)) successor
             (write-region-post-annotation-function
              (lambda ()
                (with-current-buffer owner
                  (pcase retirement
                    ('profile (setq hermes-chat--profile "replacement"))
                    ('generation
                     (cl-incf (hermes-dashboard-transport-client-generation client)))
                    ('transport (hermes-dashboard-transport-stop client))
                    ('cancel (hermes-audio-cancel))
                    ('successor
                     (hermes-audio-cancel)
                     (setq successor (hermes-audio--begin))))))))
        (hermes-audio-test--with-admissions
          (unwind-protect
              (progn
                (hermes-audio-test--reply pending (hermes-audio-test--envelope))
                (should-not starts)
                (should (plist-get op :retired))
                (should-not (plist-get op :process))
                (should-not (process-live-p (plist-get op :error-process)))
                (should-not (file-exists-p (plist-get op :directory)))
                (when successor
                  (should (eq successor hermes-audio--operation))
                  (should (hermes-audio--current-p successor))))
            (hermes-audio--retire op)
            (when successor (hermes-audio--retire successor))))))))

(defmacro hermes-audio-test--with-scratch (&rest body)
  "Run BODY with local ROOT and handler-managed OTHER; record handler WRITES."
  (declare (indent 0) (debug t))
  `(let* ((fixture (make-temp-file "hermes-audio-locality-" t))
          (root (file-name-as-directory (expand-file-name "local" fixture)))
          (other (file-name-as-directory (expand-file-name "handled" fixture)))
          (temporary-file-directory root)
          writes
          (handler (lambda (operation &rest args)
                     (if (eq operation 'file-remote-p) "synthetic-remote:"
                       (when (eq operation 'write-region) (push (car args) writes))
                       (let ((file-name-handler-alist nil)) (apply operation args))))))
     (make-directory root)
     (make-directory other)
     (unwind-protect (progn ,@body)
       (let ((file-name-handler-alist nil)) (delete-directory fixture t)))))

(ert-deftest hermes-audio-initial-handler-scratch-refused-before-consent ()
  (hermes-audio-test--with-scratch
    (hermes-audio-test--with-chat
      (dolist (remote '(nil t))
        (let* ((temporary-file-directory other)
               (file-name-handler-alist
                (list (cons (regexp-quote other)
                            (if remote handler
                              (lambda (operation &rest args)
                                (unless (eq operation 'file-remote-p)
                                  (let ((file-name-handler-alist nil))
                                    (apply operation args)))))))))
          (hermes-audio-test--with-admissions
            (unwind-protect
                (progn
                  (should-error (hermes-audio-record) :type 'user-error)
                  (should-not consent)
                  (should-not starts)
                  (should-not requests)
                  (should-not writes)
                  (should-not hermes-audio--operation))
              (hermes-audio-cancel))))))))

(ert-deftest hermes-audio-consent-and-http-ambient-scratch-never-redirect ()
  (dolist (stage '(consent http))
    (hermes-audio-test--with-scratch
      (hermes-audio-test--with-chat
        (let ((file-name-handler-alist (list (cons (regexp-quote other) handler))))
          (hermes-audio-test--with-admissions
            (unwind-protect
                (progn
                  (cl-letf (((symbol-function 'yes-or-no-p)
                             (lambda (_)
                               (when (eq stage 'consent)
                                 (setq temporary-file-directory other))
                               t)))
                    (hermes-audio-test--choose-reply))
                  (when (eq stage 'http) (setq temporary-file-directory other))
                  (hermes-audio-test--reply pending (hermes-audio-test--envelope))
                  (let* ((op hermes-audio--operation)
                         (directory (plist-get op :directory))
                         (process (plist-get op :process)))
                    (should (equal starts (list root)))
                    (should (string-prefix-p root directory))
                    (should (= #o700 (file-modes directory)))
                    (should (= #o600 (file-modes (expand-file-name "speech.wav" directory))))
                    (should-not writes)
                    (process-send-string process "done\n")
                    (hermes-test--wait-until (lambda () (plist-get op :retired)) 3 "local player")
                    (should (= 0 (process-exit-status process)))
                    (should-not (file-exists-p directory))))
              (hermes-audio-cancel))))))))

(ert-deftest hermes-audio-native-deferred-http-keeps-local-scratch ()
  (dolist (handled '(ambient captured))
    (hermes-audio-test--with-scratch
      (let ((native-request (symbol-function 'hermes-dashboard-transport--http-json-request-async))
            peer)
        (hermes-test--with-http-server
         (lambda (connection _request) (setq peer connection))
         (lambda (url)
           (hermes-audio-test--with-chat
             (cl-letf (((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                        native-request))
               (setf (hermes-dashboard-transport-client-base-url client) url)
               (hermes-audio-test--choose-reply)
               (hermes-test--wait-until (lambda () peer) 3 "deferred HTTP request")
               (let* ((op hermes-audio--operation)
                      (temporary-file-directory other)
                      (file-name-handler-alist
                       (list (cons (regexp-quote (if (eq handled 'ambient) other root)) handler))))
                 (hermes-audio-test--with-admissions
                   (unwind-protect
                       (progn
                         (hermes-test--http-reply peer 200 (json-serialize (hermes-audio-test--envelope)))
                         (hermes-test--wait-until
                          (lambda () (or (plist-get op :retired)
                                         (eq (plist-get op :state) 'playing))) 3 "deferred HTTP audio")
                         (should-not writes)
                         (if (eq handled 'captured)
                             (progn (should-not starts) (should (plist-get op :retired)))
                           (should (equal starts (list root)))
                           (should (string-prefix-p root (plist-get op :directory)))
                           (process-send-string (plist-get op :process) "done\n")
                           (hermes-test--wait-until (lambda () (plist-get op :retired)) 3 "HTTP player exit")
                           (should (= 0 (process-exit-status (plist-get op :process))))))
                     (hermes-audio--retire op))))))))))))

(ert-deftest hermes-audio-recorder-retains-pre-consent-process-directory ()
  (hermes-audio-test--with-scratch
    (hermes-audio-test--with-chat
      (let ((file-name-handler-alist (list (cons (regexp-quote other) handler))))
        (hermes-audio-test--with-admissions
          (unwind-protect
              (progn
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (_) (setq temporary-file-directory other) t)))
                  (hermes-audio-record))
                (should (equal starts (list root)))
                (should (process-live-p (plist-get hermes-audio--operation :process)))
                (should-not writes))
            (hermes-audio-cancel)))))))

(ert-deftest hermes-audio-captured-root-new-handler-fails-closed ()
  (dolist (stage '(consent http))
    (hermes-audio-test--with-scratch
      (hermes-audio-test--with-chat
        (let ((file-name-handler-alist file-name-handler-alist) op)
          (hermes-audio-test--with-admissions
            (unwind-protect
                (progn
                  (cl-letf (((symbol-function 'yes-or-no-p)
                             (lambda (_)
                               (when (eq stage 'consent)
                                 (push (cons (regexp-quote root) handler) file-name-handler-alist))
                               t)))
                    (hermes-audio-test--choose-reply))
                  (setq op hermes-audio--operation)
                  (when (eq stage 'http)
                    (push (cons (regexp-quote root) handler) file-name-handler-alist))
                  (hermes-audio-test--reply pending (hermes-audio-test--envelope))
                  (should-not writes)
                  (should-not starts)
                  (should (plist-get op :retired))
                  (should-not hermes-audio--operation))
              (hermes-audio-cancel))))))))

(ert-deftest hermes-audio-native-write-new-handler-refuses-player-and-cleans-local ()
  (hermes-audio-test--with-scratch
    (hermes-audio-test--with-chat
      (hermes-audio-test--choose-reply)
      (let* ((op hermes-audio--operation)
             (file-name-handler-alist file-name-handler-alist)
             (write-region-post-annotation-function
              (lambda () (push (cons (regexp-quote root) handler) file-name-handler-alist))))
        (hermes-audio-test--with-admissions
          (unwind-protect
              (progn
                (hermes-audio-test--reply pending (hermes-audio-test--envelope))
                (should-not starts)
                (should-not writes)
                (should (plist-get op :retired))
                (let ((file-name-handler-alist nil))
                  (should-not (file-exists-p (plist-get op :directory)))))
            (hermes-audio--retire op)))))))

(ert-deftest hermes-audio-private-cleanup-preserves-sibling-symlink ()
  (hermes-audio-test--with-scratch
    (hermes-audio-test--with-chat
      (let ((sibling (expand-file-name "unrelated" root))
            (link (expand-file-name "speech.wav" root)))
        (write-region "UNRELATED" nil sibling nil 'silent)
        (make-symbolic-link sibling link)
        (unwind-protect
            (progn
              (hermes-audio-test--choose-reply)
              (hermes-audio-test--reply pending (hermes-audio-test--envelope))
              (let ((directory (plist-get hermes-audio--operation :directory)))
                (should (process-live-p (plist-get hermes-audio--operation :process)))
                (should-not (file-symlink-p directory))
                (hermes-audio-cancel)
                (should-not (file-exists-p directory)))
              (should (file-symlink-p link))
              (should (equal "UNRELATED" (with-temp-buffer
                                          (insert-file-contents-literally link)
                                          (buffer-string)))))
          (hermes-audio-cancel))))))

(ert-deftest hermes-audio-operation-specific-handler-cannot-receive-bytes ()
  (dolist (stage '(initial deferred))
    (hermes-audio-test--with-scratch
      (hermes-audio-test--with-chat
        (let ((writer (make-symbol "synthetic-audio-write-handler"))
              (file-name-handler-alist file-name-handler-alist) op)
          (fset writer handler)
          (put writer 'operations '(write-region))
          (when (eq stage 'deferred)
            (hermes-audio-test--choose-reply)
            (setq op hermes-audio--operation))
          (push (cons (regexp-quote root) writer) file-name-handler-alist)
          (hermes-audio-test--with-admissions
            (unwind-protect
                (progn
                  (if (eq stage 'initial)
                      (should-error (hermes-audio-record) :type 'user-error)
                    (hermes-audio-test--reply pending (hermes-audio-test--envelope))
                    (should-not writes)
                    (should (plist-get op :retired)))
                  (should-not starts)
                  (should-not hermes-audio--operation))
              (hermes-audio-cancel))))))))

(provide 'hermes-audio-tests)
;;; hermes-audio-tests.el ends here
