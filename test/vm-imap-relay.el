;;; vm-imap-relay.el --- Fault-injecting IMAP relay for tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Tier 3 of dev/docs/design/imap-live-tests.org: a TCP relay that sits
;; between VM and a real IMAP server and misbehaves on request.
;;
;; A healthy dovecot cannot produce the states several of the stuck tickets
;; are about.  #335 is a connection dropped after SELECT being read as a
;; UIDVALIDITY change; #286 is duplicate deletion acting on data gathered
;; across a broken connection; #270 and the remaining gap in #38 are VM
;; trusting a STORE that the server did not accept.  None of those can be
;; reached by asking a working server nicely.
;;
;; The relay speaks no IMAP.  It forwards bytes and applies rules to the
;; stream, which keeps it small and keeps it honest: it cannot accidentally
;; paper over a protocol mistake VM makes, because it does not know what
;; correct looks like.
;;
;; Rules, all optional:
;;
;;   :drop-on REGEXP        kill both connections when a line from the client
;;                          matches, simulating the peer vanishing mid-command
;;   :drop-after REGEXP     forward the matching client line upstream first,
;;                          then kill, so the server does act on it
;;   :reject REGEXP         answer a matching client command with a tagged NO
;;                          instead of passing it upstream
;;   :bad REGEXP            the same, with BAD
;;
;; Everything the relay sees is recorded, so a test can assert on what VM
;; actually sent rather than on what it was supposed to send.

;;; Code:

(require 'cl-lib)

(cl-defstruct (vm-imap-relay (:constructor vm-imap-relay--make))
  server port upstream-host upstream-port
  drop-on drop-after reject bad
  (log nil) (dropped nil))

(defun vm-imap-relay--log (relay direction text)
  "Record TEXT flowing in DIRECTION through RELAY."
  (setf (vm-imap-relay-log relay)
        (append (vm-imap-relay-log relay) (list (cons direction text)))))

(defun vm-imap-relay-transcript (relay &optional direction)
  "Return RELAY's transcript, optionally only lines going DIRECTION."
  (mapconcat #'cdr
             (if direction
                 (seq-filter (lambda (e) (eq (car e) direction))
                             (vm-imap-relay-log relay))
               (vm-imap-relay-log relay))
             ""))

(defun vm-imap-relay--tag-of (line)
  "Return the IMAP tag at the start of LINE, or nil."
  (when (string-match "\\`\\([^ \r\n]+\\) " line)
    (match-string 1 line)))

(defun vm-imap-relay--kill (relay client)
  "Tear down CLIENT and its upstream, marking RELAY as having dropped."
  (setf (vm-imap-relay-dropped relay) t)
  (let ((upstream (process-get client 'vm-imap-relay-upstream)))
    (when (process-live-p upstream) (delete-process upstream)))
  (when (process-live-p client) (delete-process client)))

(defun vm-imap-relay--client-filter (client text)
  "Handle TEXT sent by CLIENT, applying the relay's rules."
  (let* ((relay (process-get client 'vm-imap-relay))
         (upstream (process-get client 'vm-imap-relay-upstream)))
    (vm-imap-relay--log relay 'client text)
    (cond
     ((and (vm-imap-relay-drop-on relay)
           (string-match-p (vm-imap-relay-drop-on relay) text))
      (vm-imap-relay--kill relay client))
     ((and (vm-imap-relay-drop-after relay)
           (string-match-p (vm-imap-relay-drop-after relay) text))
      (when (process-live-p upstream) (process-send-string upstream text))
      ;; Let the server act on it, then vanish before it can reply.
      (accept-process-output upstream 0 50)
      (vm-imap-relay--kill relay client))
     ((and (vm-imap-relay-reject relay)
           (string-match-p (vm-imap-relay-reject relay) text))
      (let ((tag (or (vm-imap-relay--tag-of text) "*")))
        (process-send-string client (format "%s NO relay refused\r\n" tag))
        (vm-imap-relay--log relay 'server (format "%s NO relay refused\r\n" tag))))
     ((and (vm-imap-relay-bad relay)
           (string-match-p (vm-imap-relay-bad relay) text))
      (let ((tag (or (vm-imap-relay--tag-of text) "*")))
        (process-send-string client (format "%s BAD relay refused\r\n" tag))
        (vm-imap-relay--log relay 'server (format "%s BAD relay refused\r\n" tag))))
     (t
      (when (process-live-p upstream) (process-send-string upstream text))))))

(defun vm-imap-relay--upstream-filter (upstream text)
  "Forward TEXT from UPSTREAM back to its client."
  (let ((relay (process-get upstream 'vm-imap-relay))
        (client (process-get upstream 'vm-imap-relay-client)))
    (vm-imap-relay--log relay 'server text)
    (when (process-live-p client) (process-send-string client text))))

(defun vm-imap-relay--on-connect (server client _message)
  "Open an upstream connection for CLIENT accepted by SERVER."
  (let* ((relay (process-get server 'vm-imap-relay))
         (upstream (make-network-process
                    :name "vm-imap-relay-upstream"
                    :host (vm-imap-relay-upstream-host relay)
                    :service (vm-imap-relay-upstream-port relay)
                    :coding 'binary :noquery t
                    :filter #'vm-imap-relay--upstream-filter)))
    (process-put upstream 'vm-imap-relay relay)
    (process-put upstream 'vm-imap-relay-client client)
    (process-put client 'vm-imap-relay relay)
    (process-put client 'vm-imap-relay-upstream upstream)
    (set-process-filter client #'vm-imap-relay--client-filter)
    (set-process-coding-system client 'binary 'binary)
    ;; An accepted connection gets a buffer named after its process, and this
    ;; relay never reads it: everything the client sends goes to
    ;; `vm-imap-relay--client-filter'.  Left alone it outlives the test, one per
    ;; connection.  Detached before it is killed, so killing it does not ask
    ;; about the live process.
    (let ((buffer (process-buffer client)))
      (set-process-buffer client nil)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(cl-defun vm-imap-relay-start (&key host port drop-on drop-after reject bad)
  "Start a relay in front of the IMAP server at HOST and PORT.
Returns a `vm-imap-relay'; its `vm-imap-relay-port' is the local port to
point VM at.

DROP-ON, DROP-AFTER, REJECT and BAD are the fault-injection rules, each a
regexp matched against lines from the client; see the commentary above for
what each one does."
  (let* ((relay (vm-imap-relay--make
                 :upstream-host host :upstream-port port
                 :drop-on drop-on :drop-after drop-after
                 :reject reject :bad bad))
         (server (make-network-process
                  :name "vm-imap-relay" :server t :service t
                  :host 'local :family 'ipv4 :coding 'binary :noquery t
                  :log #'vm-imap-relay--on-connect)))
    (process-put server 'vm-imap-relay relay)
    (setf (vm-imap-relay-server relay) server
          (vm-imap-relay-port relay) (process-contact server :service))
    relay))

(defun vm-imap-relay-stop (relay)
  "Shut RELAY down, ignoring errors."
  (let ((server (vm-imap-relay-server relay)))
    (when (process-live-p server)
      (ignore-errors (delete-process server))))
  ;; Any client and upstream processes it spawned.
  (dolist (p (process-list))
    (when (string-prefix-p "vm-imap-relay" (process-name p))
      (ignore-errors (delete-process p)))))

(defmacro vm-imap-relay-with (spec &rest body)
  "Run BODY with a relay bound to RELAY-VAR, then stop it.
SPEC is (RELAY-VAR &rest ARGS), where ARGS go to `vm-imap-relay-start'."
  (declare (indent 1) (debug t))
  `(let ((,(car spec) (vm-imap-relay-start ,@(cdr spec))))
     (unwind-protect (progn ,@body)
       (vm-imap-relay-stop ,(car spec)))))

(provide 'vm-imap-relay)

;;; vm-imap-relay.el ends here
