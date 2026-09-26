;;; jabber-test-notifications.el --- Tests for jabber-notifications  -*- lexical-binding: t; -*-

;;; Commentary:

;; Desktop notification actions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'jabber-notifications)

(defun jabber-test-notifications--arguments (buffer)
  "Return notification arguments produced for BUFFER."
  (let (arguments)
    (cl-letf (((symbol-function 'notifications-notify)
               (lambda (&rest args)
                 (setq arguments args))))
      (jabber-message-notifications
       "romeo@example.net" buffer "Hello" "Romeo"))
    arguments))

(ert-deftest jabber-test-notifications-default-action ()
  (with-temp-buffer
    (should (equal (plist-get (jabber-test-notifications--arguments
                               (current-buffer))
                              :actions)
                   '("default" "Switch to buffer")))))

(ert-deftest jabber-test-notifications-action-opens-chat-buffer ()
  (with-temp-buffer
    (let* ((buffer (current-buffer))
           (callback (plist-get
                      (jabber-test-notifications--arguments buffer)
                      :on-action))
           opened)
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (target &rest _)
                   (setq opened target))))
        (funcall callback "notification-id" "default"))
      (should (eq opened buffer)))))

(ert-deftest jabber-test-notifications-action-ignores-killed-buffer ()
  (let* ((buffer (generate-new-buffer " jabber-notification-test"))
         (callback (plist-get
                    (jabber-test-notifications--arguments buffer)
                    :on-action)))
    (kill-buffer buffer)
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (&rest _)
                 (ert-fail "pop-to-buffer called for a killed buffer"))))
      (should-not (funcall callback "notification-id" "default")))))

(ert-deftest jabber-test-notifications-close-removes-only-own-action ()
  (with-temp-buffer
    (let* ((arguments (jabber-test-notifications--arguments
                       (current-buffer)))
           (action (plist-get arguments :on-action))
           (on-close (plist-get arguments :on-close))
           (other-action (lambda (&rest _)))
           (notifications-on-action-map
            `(((bus service 1) ,action)
              ((bus service 2) ,other-action))))
      (funcall on-close "notification-id" 'expired)
      (should (equal notifications-on-action-map
                     `(((bus service 2) ,other-action)))))))

(ert-deftest jabber-test-notifications-close-unregisters-action-signal ()
  (with-temp-buffer
    (let* ((arguments (jabber-test-notifications--arguments
                       (current-buffer)))
           (action (plist-get arguments :on-action))
           (on-close (plist-get arguments :on-close))
           (notifications-on-action-map `(((bus service 1) ,action)))
           (notifications-on-action-object 'action-signal)
           unregistered)
      (cl-letf (((symbol-function 'dbus-unregister-object)
                 (lambda (object)
                   (setq unregistered object))))
        (funcall on-close "notification-id" 'expired))
      (should (null notifications-on-action-map))
      (should (eq unregistered 'action-signal))
      (should (null notifications-on-action-object)))))

(ert-deftest jabber-test-notifications-nil-title-is-silent ()
  "Neither direct nor MUC notification policy overrides title suppression."
  (let ((jabber-notifications-muc 'all)
        calls)
    (cl-letf (((symbol-function 'notifications-notify)
               (lambda (&rest args) (push args calls))))
      (save-window-excursion
        (with-temp-buffer
          (set-window-buffer (selected-window) (current-buffer))
          (let ((jabber-message-alert-same-buffer nil))
            (jabber-message-notifications
             "peer@example.test" (current-buffer) "hello"
             (jabber-message-default-message
              "peer@example.test" (current-buffer) "hello")))
          (jabber-message-notifications "peer@example.test" nil "hello" nil)
          (jabber-muc-notifications "nick" "room@example.test" nil "hello" nil)))
      (should-not calls))))

(provide 'jabber-test-notifications)

;;; jabber-test-notifications.el ends here
