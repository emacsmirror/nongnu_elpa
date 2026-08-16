;;; jabber-test-rtt.el --- Tests for jabber-rtt  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0301 In-Band Real Time Text.

;;; Code:

(require 'ert)
(require 'jabber-rtt)

(defun jabber-test-rtt--process-actions (message actions)
  "Return MESSAGE after replaying RTT ACTIONS."
  (with-temp-buffer
    (setq-local jabber-rtt-message message)
    (setq-local jabber-rtt-ewoc-node 'test-node)
    (setq-local jabber-rtt-pending-events actions)
    (cl-letf (((symbol-function 'ewoc-set-data) #'ignore)
              ((symbol-function 'jabber-chat-ewoc-invalidate) #'ignore))
      (jabber-rtt--process-actions (current-buffer)))
    jabber-rtt-message))

(defun jabber-test-rtt--message (sequence event &rest actions)
  "Return an RTT message with SEQUENCE, EVENT, and ACTIONS."
  `(message ((from . "them@example.org") (type . "chat"))
            (rtt ((xmlns . ,jabber-rtt-xmlns)
                  (seq . ,(number-to-string sequence))
                  ,@(when event `((event . ,event))))
                 ,@actions)))

(ert-deftest jabber-test-rtt-ignores-edits-until-resynchronized ()
  "A sequence gap blocks later edits until a new message or reset."
  (with-temp-buffer
    (setq-local jabber-rtt-ewoc-node 'test-node)
    (setq-local jabber-rtt-last-seq 10)
    (setq-local jabber-rtt-message "hello")
    (cl-letf (((symbol-function 'jabber-chat-get-buffer)
               (lambda (&rest _args) (buffer-name)))
              ((symbol-function 'ewoc-set-data) #'ignore)
              ((symbol-function 'jabber-chat-ewoc-invalidate) #'ignore))
      (let ((inhibit-message t))
        (jabber-rtt-handle-message
         'test-connection
         (jabber-test-rtt--message 12 "edit" '(t nil "!")))
        (jabber-rtt-handle-message
         'test-connection
         (jabber-test-rtt--message 11 "edit" '(t nil "!")))))
    (should (equal "hello" jabber-rtt-message))))

(ert-deftest jabber-test-rtt-reset-recovers-synchronization ()
  "A reset replaces stale text and permits subsequent edits."
  (with-temp-buffer
    (setq-local jabber-rtt-ewoc-node 'old-node)
    (setq-local jabber-rtt-last-seq 10)
    (setq-local jabber-rtt-out-of-sync-p t)
    (setq-local jabber-rtt-message "stale")
    (cl-letf (((symbol-function 'jabber-chat-get-buffer)
               (lambda (&rest _args) (buffer-name)))
              ((symbol-function 'jabber-chat-ewoc-delete) #'ignore)
              ((symbol-function 'jabber-chat-ewoc-enter)
               (lambda (&rest _args) 'new-node))
              ((symbol-function 'ewoc-set-data) #'ignore)
              ((symbol-function 'jabber-chat-ewoc-invalidate) #'ignore))
      (jabber-rtt-handle-message
       'test-connection
       (jabber-test-rtt--message 20 "reset" '(t nil "fresh")))
      (jabber-rtt-handle-message
       'test-connection
       (jabber-test-rtt--message 21 "edit" '(t nil "!"))))
    (should (equal "fresh!" jabber-rtt-message))
    (should (= 21 jabber-rtt-last-seq))
    (should-not jabber-rtt-out-of-sync-p)))

(ert-deftest jabber-test-rtt-replays-insert-and-erase-actions ()
  "Insert and erase actions follow the XEP-0301 editing example."
  (should
   (equal "Hello there, World"
          (jabber-test-rtt--process-actions
           ""
           '((t nil "Helo")
             (e nil)
             (t nil "lo...planet")
             (e ((n . "6")) nil)
             (t nil " World")
             (e ((n . "3") (p . "8")) nil)
             (t ((p . "5")) " there,"))))))

(ert-deftest jabber-test-rtt-ignores-blank-insert ()
  "A blank insert action leaves the real-time message unchanged."
  (should
   (equal "hello"
          (jabber-test-rtt--process-actions
           "hello" '((t nil "   "))))))

(ert-deftest jabber-test-rtt-replay-ignores-dead-buffer ()
  "A delayed replay callback ignores a buffer that has been killed."
  (let ((buffer (generate-new-buffer " *jabber-rtt-dead*")))
    (kill-buffer buffer)
    (should-not (jabber-rtt--process-actions buffer))))

(ert-deftest jabber-test-rtt-send-ignores-dead-buffer ()
  "A delayed send callback ignores a buffer that has been killed."
  (let ((buffer (generate-new-buffer " *jabber-rtt-send-dead*")))
    (kill-buffer buffer)
    (should-not (jabber-rtt--send-queued-events buffer))))

(ert-deftest jabber-test-rtt-clips-negative-wait ()
  "Negative wait intervals are clipped to zero."
  (let* ((actions '((w ((n . "-10")) nil)))
         (result (jabber-rtt--fix-waits actions))
         (wait (car result)))
    (should (equal "0" (jabber-xml-get-attribute wait 'n)))))

(ert-deftest jabber-test-rtt-scales-waits-to-unsigned-integers ()
  "Scaled wait intervals remain valid unsigned integers."
  (let* ((actions '((w ((n . "500")) nil)
                    (t nil "x")
                    (w ((n . "500")) nil)))
         (result (jabber-rtt--fix-waits actions))
         (waits (seq-filter
                 (lambda (action)
                   (eq (jabber-xml-node-name action) 'w))
                 result)))
    (should (equal '("350" "350")
                   (mapcar
                    (lambda (wait)
                      (jabber-xml-get-attribute wait 'n))
                    waits)))))

(ert-deftest jabber-test-rtt-sends-queued-edit ()
  "Queued actions are sent in an incremented edit event."
  (with-temp-buffer
    (setq-local jabber-buffer-connection 'test-connection)
    (setq-local jabber-chatting-with "them@example.org")
    (setq-local jabber-rtt-send-seq 7)
    (setq-local jabber-rtt-outgoing-events '((t nil "x")))
    (put 'test-connection :state :session-established)
    (unwind-protect
        (let ((jabber-connections '(test-connection))
              sent)
          (cl-letf (((symbol-function 'jabber-send-sexp)
                     (lambda (_connection stanza)
                       (setq sent stanza))))
            (jabber-rtt--send-queued-events (current-buffer)))
          (should
           (equal
            `(message ((to . "them@example.org") (type . "chat"))
                      (rtt ((xmlns . ,jabber-rtt-xmlns)
                            (seq . "8")
                            (event . "edit"))
                           (t nil "x")))
            sent))
          (should-not jabber-rtt-outgoing-events))
      (put 'test-connection :state nil))))

(ert-deftest jabber-test-rtt-send-ignores-dead-connection ()
  "A delayed send callback does not send after the session is gone."
  (with-temp-buffer
    (setq-local jabber-buffer-connection 'dead-jc)
    (setq-local jabber-chatting-with "them@example.org")
    (setq-local jabber-rtt-send-seq 7)
    (setq-local jabber-rtt-outgoing-events '((t nil "x")))
    (let ((jabber-connections nil)
          sent)
      (cl-letf (((symbol-function 'jabber-send-sexp)
                 (lambda (_jc _stanza)
                   (setq sent t)
                   (error "dead-jc has no connection"))))
        (jabber-rtt--send-queued-events (current-buffer)))
      (should-not sent)
      (should (equal '((t nil "x")) jabber-rtt-outgoing-events)))))

(provide 'jabber-test-rtt)
;;; jabber-test-rtt.el ends here
