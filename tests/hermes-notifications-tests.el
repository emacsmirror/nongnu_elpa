;;; hermes-notifications-tests.el --- desktop notification tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared desktop notification policy, focus suppression, and click actions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'hermes-notifications)

(defvar notifications-on-action-map)
(defvar notifications-on-action-object)

(ert-deftest hermes-notifications-load-keeps-notifications-optional ()
  "Loading the Hermes boundary does not load `notifications'."
  (should-not (featurep 'notifications)))

(ert-deftest hermes-notifications-default-events-are-high-signal ()
  "Default notifications cover unattended work without routine Kanban success."
  (should (equal hermes-notifications-events
                 '(chat-reply chat-error prompt background
                   kanban-attention cron-failure)))
  (should (hermes-notifications-enabled-p 'chat-reply))
  (should-not (hermes-notifications-enabled-p 'kanban-done)))

(ert-deftest hermes-notifications-disabled-event-does-nothing ()
  "An event absent from the configured set emits no desktop notification."
  (let ((hermes-notifications-events nil)
        called)
    (cl-letf (((symbol-function 'notifications-notify)
               (lambda (&rest _) (setq called t))))
      (should-not (hermes-notifications-notify 'chat-reply "Title" "Body"))
      (should-not called))))

(ert-deftest hermes-notifications-suppress-visible-buffer-on-focused-frame ()
  "A visible target on the focused frame needs no desktop interruption."
  (with-temp-buffer
    (let (called)
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t))
                ((symbol-function 'get-buffer-window)
                 (lambda (&rest _) (selected-window)))
                ((symbol-function 'notifications-notify)
                 (lambda (&rest _) (setq called t))))
        (should-not
         (hermes-notifications-notify
          'chat-reply "Title" "Body" :buffer (current-buffer)))
        (should-not called)))))

(ert-deftest hermes-notifications-action-opens-live-buffer ()
  "The default action opens the target buffer when it remains live."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          arguments opened)
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil))
                ((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'notifications-notify)
                 (lambda (&rest args) (setq arguments args) 7))
                ((symbol-function 'pop-to-buffer)
                 (lambda (target &rest _) (setq opened target))))
        (should (= 7 (hermes-notifications-notify
                      'chat-reply "Title" "Body" :buffer buffer
                      :category "hermes.chat" :urgency 'normal)))
        (should (equal (plist-get arguments :actions)
                       '("default" "Open in Emacs")))
        (should (equal (plist-get arguments :category) "hermes.chat"))
        (should (eq (plist-get arguments :urgency) 'normal))
        (funcall (plist-get arguments :on-action) 7 "default")
        (should (eq opened buffer))))))

(ert-deftest hermes-notifications-action-ignores-killed-buffer ()
  "Clicking a stale notification does not recreate or display a dead buffer."
  (let ((buffer (generate-new-buffer " hermes-notification-dead"))
        arguments)
    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil))
              ((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'notifications-notify)
               (lambda (&rest args) (setq arguments args) 8)))
      (hermes-notifications-notify
       'chat-reply "Title" "Body" :buffer buffer)
      (kill-buffer buffer)
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (&rest _)
                   (ert-fail "Opened a killed notification buffer"))))
        (should-not (funcall (plist-get arguments :on-action) 8 "default"))))))

(ert-deftest hermes-notifications-close-removes-own-action ()
  "Closing a notification removes its pending click callback."
  (with-temp-buffer
    (let (arguments)
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil))
                ((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'notifications-notify)
                 (lambda (&rest args) (setq arguments args) 9)))
        (hermes-notifications-notify
         'chat-reply "Title" "Body" :buffer (current-buffer)))
      (let* ((action (plist-get arguments :on-action))
             (close (plist-get arguments :on-close))
             (other (lambda (&rest _)))
             (notifications-on-action-map
              `(((bus service 9) ,action)
                ((bus service 10) ,other)))
             notifications-on-action-object)
        (funcall close 9 'expired)
        (should (equal notifications-on-action-map
                       `(((bus service 10) ,other))))))))

(ert-deftest hermes-notifications-fall-back-to-echo-area ()
  "Unavailable desktop notifications degrade to one concise echo message."
  (let (text)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _)
                 (not (eq feature 'notifications))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq text (apply #'format format-string args)))))
      (should-not
       (hermes-notifications-notify 'chat-error "Hermes error" "Failed"))
      (should (equal text "Hermes error: Failed")))))

(provide 'hermes-notifications-tests)
;;; hermes-notifications-tests.el ends here
