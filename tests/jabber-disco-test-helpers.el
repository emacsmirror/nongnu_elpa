;;; jabber-disco-test-helpers.el --- Discovery fixtures -*- lexical-binding: t; -*-

;;; Commentary:
;; Share native connection and wire fixtures without importing another suite.

;;; Code:

(require 'ert)
(require 'jabber-disco)
(require 'jabber-moderation)

(defun jabber-test-disco-owner--connection (name)
  "Return a disposable connected FSM named NAME."
  (let ((jc (make-symbol name)))
    (put jc :state 'session-established)
    (put jc :state-data
         (list :connection (make-symbol "transport") :session-id "stream"
               :username name :server "example.org"))
    jc))

(defmacro jabber-test-disco-owner--with-state (&rest body)
  "Run BODY with isolated native discovery state and captured wire writes."
  (declare (indent 0) (debug t))
  `(let* ((a (jabber-test-disco-owner--connection "a"))
          (b (jabber-test-disco-owner--connection "b"))
          (c (jabber-test-disco-owner--connection "c"))
          (jabber-connections (list a b c))
          (jabber-db-path nil)
          (jabber-jid-obarray (make-vector 127 0))
          (jabber-caps-cache (make-hash-table :test #'equal))
          (jabber-caps--pending (make-hash-table :test #'equal))
          (jabber-disco-info-cache (make-hash-table :test #'equal))
          (jabber-disco-items-cache (make-hash-table :test #'equal))
          (jabber-open-info-queries nil)
          (wire nil))
     (unwind-protect
         (cl-letf (((symbol-function 'jabber-send-sexp)
                    (lambda (jc xml &rest _ignored)
                      (push (cons jc xml) wire))))
           ,@body)
       (maphash (lambda (key pending) (jabber-caps--settle key pending))
                jabber-caps--pending))))

(defun jabber-test-disco-owner--reply (sent children &optional error-p)
  "Deliver a reply to SENT with query CHILDREN, or an error if ERROR-P."
  (let* ((jc (car sent))
         (iq (cdr sent))
         (query (jabber-iq-query iq)))
    (jabber-process-iq
     jc `(iq ((from . ,(jabber-xml-get-attribute iq 'to))
              (id . ,(jabber-xml-get-attribute iq 'id))
              (type . ,(if error-p "error" "result")))
             ,(if error-p
                  '(error ((type . "cancel")) (item-not-found ()))
                `(query ,(cadr query) ,@children))))))

(defun jabber-test-disco-owner--expire (pending)
  "Run PENDING's real timeout function without waiting ten seconds."
  (let ((timer (plist-get pending :timer)))
    (apply (timer--function timer) (timer--args timer))))

(provide 'jabber-disco-test-helpers)
;;; jabber-disco-test-helpers.el ends here
