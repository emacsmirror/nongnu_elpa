;;; jabber-archive-test-helpers.el --- Archive fixtures -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared fixtures contain no ERT definitions, so ordinary suites compose.

;;; Code:

(require 'ert)
(require 'jabber-db)
(require 'jabber-disco)
(require 'jabber-chat)
(require 'jabber-muc)
(require 'jabber-mam)
(require 'jabber-core)
(require 'jabber-message-correct)
(require 'jabber-moderation)
(require 'jabber-omemo-store)
(require 'jabber-message-reply)
(require 'jabber-reactions)

(defun jabber-test-archive--contain-errors (dispatch &rest args)
  "Call DISPATCH with ARGS without entering the debugger on contained errors."
  (let ((debug-on-error nil))
    (apply dispatch args)))

;; Stanza dispatch contains handler errors with a `debug' condition.
;; ERT before Emacs 30 runs tests with `debug-on-error' and fails on any
;; debugger entry, so injected faults that dispatch contains would fail.
(when (< emacs-major-version 30)
  (advice-add 'jabber-process-input :around #'jabber-test-archive--contain-errors))

(defun jabber-test-mam--native-connection (&optional username)
  "Return a disposable established native FSM for USERNAME, defaulting to me."
  (let ((jc (make-symbol "mam-native")))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data
         (jabber-sm--reset
          (list :username (or username "me") :server "example.com"
                :connection (list 'transport) :session-id "stream"
                :send-function #'ignore)))
    jc))

(defmacro jabber-test-archive--with-file (&rest body)
  "Run BODY with a disposable native file database and MAM state."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "jabber-archive-" t))
          (jabber-db-path (expand-file-name "history.sqlite" directory))
          (jabber-db--connection nil)
          (jabber-mam--syncing nil)
          (jabber-mam--dirty-peers nil)
          (jabber-mam--redraw-peers nil)
          (jabber-mam--redraw-timer nil)
          (jabber-open-info-queries nil)
          (jabber-db-message-thread-stored-functions nil)
          (jabber-history-inhibit-received-message-functions nil)
          (jabber-mam-sync-complete-functions nil)
          (jc (jabber-test-mam--native-connection)))
     (unwind-protect
         (cl-letf (((symbol-function 'jabber-send-sexp) #'ignore))
           (jabber-db-ensure-open)
           ,@body)
       (jabber-mam--cleanup-all)
       (when jabber-mam--redraw-timer
         (cancel-timer jabber-mam--redraw-timer))
       (jabber-db-close)
       (delete-directory directory t))))

(defun jabber-test-archive--result (query uid inner)
  "Wrap INNER in QUERY's result with UID."
  `(message ((from . ,(or (plist-get query :to) "me@example.com")))
            (result ((xmlns . "urn:xmpp:mam:2")
                     (queryid . ,(plist-get query :id)) (id . ,uid))
                    (forwarded ((xmlns . "urn:xmpp:forward:0"))
                               (delay ((xmlns . "urn:xmpp:delay")
                                       (stamp . "2025-01-01T00:00:00Z")))
                               ,inner))))

(defun jabber-test-archive--fin (jc query uid)
  "Complete QUERY on JC with UID."
  (jabber-mam--handle-fin
   jc `(iq ((from . ,(or (plist-get query :to) "me@example.com")))
           (fin ((xmlns . "urn:xmpp:mam:2") (complete . "true"))
                (set ((xmlns . "http://jabber.org/protocol/rsm"))
                     ,@(when uid `((last () ,uid))))))
   (cons query (plist-get query :page))))

(defun jabber-test-archive--idle-p ()
  "Return non-nil when no SQLite transaction is open on the database.
MAM must never hold a transaction while it waits for the network."
  (condition-case nil
      (progn (sqlite-execute jabber-db--connection "BEGIN")
             (sqlite-execute jabber-db--connection "ROLLBACK")
             t)
    (sqlite-error nil)))

(defun jabber-test-archive--progress (&optional account archive with)
  "Read native progress for ACCOUNT, ARCHIVE and WITH."
  (jabber-db-mam-progress (or account "me@example.com")
                          (or archive "me@example.com") with))

(provide 'jabber-archive-test-helpers)
;;; jabber-archive-test-helpers.el ends here
