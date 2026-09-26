;;; jabber-test-alert.el --- Alert routing tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise real alert and send modules with isolated buffers and wire capture.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'jabber-chat)
(require 'jabber-muc)
(require 'jabber-alert)
(require 'jabber-notifications)
(require 'jabber-activity)

(defun jabber-test-alert--connection (name)
  "Return a disposable connection named NAME."
  (let ((jc (make-symbol name)))
    (put jc :state-data (list :username name :server "example.test"))
    jc))

(ert-deftest jabber-test-alert-autoanswer-origin ()
  "A closed-thread alert replies through its parent, never the current chat."
  (let* ((a (jabber-test-alert--connection "a"))
         (b (jabber-test-alert--connection "b"))
         (jabber-connections (list a b))
         (origin (generate-new-buffer " *jabber-alert-origin*"))
         (jabber-autoanswer-alist '(("ping" . "reply")))
         (jabber-message-hooks nil)
         (jabber-alert-message-hooks '(jabber-autoanswer-answer))
         (jabber-alert-message-function (lambda (&rest _) "Peer"))
         sent)
    (unwind-protect
        (progn
          (with-current-buffer origin
            (setq-local jabber-buffer-connection a
                        jabber-chatting-with "peer@example.test"
                        jabber-send-function #'jabber-chat-send
                        jabber-chat-encryption 'plaintext))
          (with-temp-buffer
            (setq-local jabber-buffer-connection b
                        jabber-chatting-with "other@example.test")
            (cl-letf (((symbol-function 'jabber-chat--find-buffer-on-connection)
                       (lambda (&rest _) origin))
                      ((symbol-function 'jabber-chat--run-send-hooks) #'ignore)
                      ((symbol-function 'jabber-chat--display-local-message) #'ignore)
                      ((symbol-function 'jabber-send-sexp)
                       (lambda (jc stanza) (push (list jc stanza) sent))))
              (jabber-chat--display-message
               a nil nil nil "peer@example.test/resource"
               '(:body "ping" :thread-id "closed")))
            (should (= (length sent) 1))
            (should (eq (caar sent) a))
            (should (equal (jabber-xml-get-attribute (cadar sent) 'to)
                           "peer@example.test"))))
      (kill-buffer origin))))

(ert-deftest jabber-test-alert-autoanswer-stale-origin ()
  "Killed, disconnected, and retargeted origins never send a reply."
  (let* ((a (jabber-test-alert--connection "a"))
         (jabber-connections (list a))
         (jabber-autoanswer-alist '(("ping" . "reply")))
         (jabber-alert-chat-send-function (lambda (&rest _) (ert-fail "Sent"))))
    (with-temp-buffer
      (setq-local jabber-buffer-connection a
                  jabber-chatting-with "other@example.test")
      (jabber-autoanswer-answer "peer@example.test" (current-buffer) "ping" "Peer")
      (setq-local jabber-chatting-with "peer@example.test")
      (let ((jabber-connections nil))
        (jabber-autoanswer-answer "peer@example.test" (current-buffer) "ping" "Peer")))
    (let ((dead (generate-new-buffer " *jabber-alert-dead*")))
      (kill-buffer dead)
      (jabber-autoanswer-answer "peer@example.test" dead "ping" "Peer"))))

(ert-deftest jabber-test-alert-autoanswer-muc-origin ()
  "MUC autoanswers use the room's groupchat sender and owning account."
  (let* ((a (jabber-test-alert--connection "a"))
         (b (jabber-test-alert--connection "b"))
         (jabber-connections (list a b))
         (jabber-muc--rooms (make-hash-table :test #'equal))
         (jabber-autoanswer-alist '(("ping" . "reply")))
         (origin (generate-new-buffer " *jabber-alert-room*"))
         sent)
    (unwind-protect
        (progn
          (jabber-muc-join-set "room@example.test" a "alice")
          (with-current-buffer origin
            (setq-local jabber-buffer-connection a
                        jabber-group "room@example.test"
                        jabber-send-function #'jabber-muc-send
                        jabber-chat-encryption 'plaintext))
          (with-temp-buffer
            (setq-local jabber-buffer-connection b
                        jabber-chatting-with "unrelated@example.test")
            (cl-letf (((symbol-function 'jabber-chat--run-send-hooks) #'ignore)
                      ((symbol-function 'jabber-chat--display-local-message) #'ignore)
                      ((symbol-function 'jabber-send-sexp)
                       (lambda (jc stanza) (push (list jc stanza) sent))))
              (jabber-autoanswer-answer-muc
               "bob" "room@example.test" origin "ping" "Room"))
            (should (= (length sent) 1))
            (should (eq (caar sent) a))
            (should (equal (jabber-xml-get-attribute (cadar sent) 'to)
                           "room@example.test"))
            (should (equal (jabber-xml-get-attribute (cadar sent) 'type)
                           "groupchat"))))
      (kill-buffer origin))))

(ert-deftest jabber-test-alert-muc-background-mentions ()
  "Closed rooms retain the receiving account through notification hooks."
  (let* ((a (jabber-test-alert--connection "a"))
         (b (jabber-test-alert--connection "b"))
         (jabber-connections (list a b))
         (jabber-muc--rooms (make-hash-table :test #'equal))
         (jabber-muc-hooks nil)
         (jabber-alert-muc-hooks '(jabber-muc-notifications))
         (jabber-alert-muc-function (lambda (&rest _) "Room"))
         (jabber-notifications-muc 'mentions)
         sent)
    (jabber-muc-join-set "room@example.test" a "alice")
    (jabber-muc-join-set "room@example.test" b "bob")
    (with-temp-buffer
      (setq-local jabber-buffer-connection b)
      (cl-letf (((symbol-function 'jabber-muc-find-buffer) (lambda (&rest _) nil))
                ((symbol-function 'notifications-notify)
                 (lambda (&rest args) (push args sent))))
        (jabber-muc--display-message
         a '(message nil) "room@example.test" "sender" :muc-foreign
         '(:body "alice: hello"))
        (should (= (length sent) 1))
        (jabber-muc--display-message
         a '(message nil) "room@example.test" "sender" :muc-foreign
         '(:body "bob: hello"))
        (should (= (length sent) 1))))))

(ert-deftest jabber-test-alert-muc-nick-retains-owner ()
  "Interactive nickname changes use the room buffer's account."
  (let* ((a (jabber-test-alert--connection "a"))
         (b (jabber-test-alert--connection "b"))
         (jabber-connections (list a b))
         (jabber-muc--rooms (make-hash-table :test #'equal))
         sent)
    (jabber-muc-join-set "room@example.test" a "alice")
    (jabber-muc-join-set "room@example.test" b "bob")
    (with-temp-buffer
      (setq major-mode 'jabber-chat-mode)
      (setq-local jabber-buffer-connection a jabber-group "room@example.test")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "new-alice"))
                ((symbol-function 'jabber-send-sexp)
                 (lambda (jc stanza) (push (list jc stanza) sent))))
        (call-interactively #'jabber-muc-nick))
      (should (eq (caar sent) a))
      (should (equal (jabber-xml-get-attribute (cadar sent) 'to)
                     "room@example.test/new-alice"))
      (should (equal (jabber-muc-nickname "room@example.test" b) "bob"))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _)
                   (setq jabber-connections (list b))
                   "stale"))
                ((symbol-function 'jabber-send-sexp)
                 (lambda (&rest args) (push args sent))))
        (should-error (call-interactively #'jabber-muc-nick) :type 'user-error))
      (should (= (length sent) 1)))))

(ert-deftest jabber-test-alert-muc-hooks-retain-receiver ()
  "Buffer-changing hooks cannot steal subsequent mention detection."
  (let* ((a (jabber-test-alert--connection "a"))
         (b (jabber-test-alert--connection "b"))
         (jabber-connections (list a b))
         (jabber-muc--rooms (make-hash-table :test #'equal))
         (foreign (generate-new-buffer " *jabber-foreign*"))
         (jabber-alert-muc-function (lambda (&rest _) "Room"))
         (jabber-notifications-muc 'mentions)
         (jabber-activity-jids nil)
         (jabber-activity-personal-jids nil)
         (jabber-activity-show-p (lambda (&rest _) t))
         (jabber-muc-hooks
          (list (lambda (&rest _) (set-buffer foreign))
                #'jabber-activity-add-muc))
         (jabber-alert-muc-hooks '(jabber-muc-notifications))
         calls)
    (unwind-protect
        (progn
          (jabber-muc-join-set "room@example.test" a "alice")
          (jabber-muc-join-set "room@example.test" b "bob")
          (with-current-buffer foreign (setq-local jabber-buffer-connection b))
          (cl-letf (((symbol-function 'jabber-muc-find-buffer) (lambda (&rest _) nil))
                    ((symbol-function 'jabber-activity-mode-line-update) #'ignore)
                    ((symbol-function 'notifications-notify)
                     (lambda (&rest args) (push args calls))))
            (jabber-muc--display-message
             a '(message nil) "room@example.test" "sender" :muc-foreign
             '(:body "alice: hello")))
          (should (= (length calls) 1))
          (should (equal jabber-activity-personal-jids '("room@example.test"))))
      (kill-buffer foreign))))

(provide 'jabber-test-alert)
;;; jabber-test-alert.el ends here
