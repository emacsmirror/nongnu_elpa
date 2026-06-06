;;; jabber-test-conn.el --- Tests for jabber-conn  -*- lexical-binding: t; -*-

;;; Commentary:

;; Network transport helpers.

;;; Code:

(require 'ert)
(require 'jabber-conn)

(defvar jabber-process-buffer)
(defvar jabber-debug-keep-process-buffers)

;;; Failed async connection cleanup

(ert-deftest jabber-conn-test-failed-target-kills-process-buffer ()
  "A failed async connection target cleans up its process buffer."
  (let ((jabber-process-buffer " *jabber-test-process*")
        (jabber-debug-keep-process-buffers nil)
        (jabber-connection-timeout nil)
        proc
        sentinel
        fsm-event)
    (cl-letf (((symbol-function 'jabber-srv-targets)
               (lambda (&rest _) '(("example.com" 5222 nil))))
              ((symbol-function 'jabber-conn--make-process)
               (lambda (_host _port buffer _directtls-p _server)
                 (setq proc (make-pipe-process
                             :name "jabber-test-process"
                             :buffer buffer))
                 proc))
              ((symbol-function 'set-process-sentinel)
               (lambda (_proc fn) (setq sentinel fn)))
              ((symbol-function 'fsm-send)
               (lambda (_fsm event) (setq fsm-event event))))
      (jabber-network-connect-async 'fake-fsm "example.com" nil nil)
      (funcall sentinel proc "failed with code 1\n")
      (should-not (process-live-p proc))
      (should-not (buffer-live-p (process-buffer proc)))
      (should (equal '(:connection-failed
                       ("Couldn't connect to example.com:5222: failed with code 1"))
                     fsm-event)))))

(ert-deftest jabber-conn-test-setup-error-kills-generated-buffer ()
  "A setup error after buffer creation kills the generated buffer."
  (let ((jabber-process-buffer " *jabber-test-process*")
        (jabber-debug-keep-process-buffers nil)
        (jabber-connection-timeout nil)
        generated-buffer
        fsm-event)
    (cl-letf (((symbol-function 'jabber-srv-targets)
               (lambda (&rest _) '(("example.com" 5222 nil))))
              ((symbol-function 'generate-new-buffer)
               (lambda (name)
                 (setq generated-buffer (get-buffer-create name))
                 generated-buffer))
              ((symbol-function 'jabber-conn--make-process)
               (lambda (&rest _) (error "setup failed")))
              ((symbol-function 'fsm-send)
               (lambda (_fsm event) (setq fsm-event event))))
      (jabber-network-connect-async 'fake-fsm "example.com" nil nil)
      (should-not (buffer-live-p generated-buffer))
      (should (equal '(:connection-failed
                       ("Couldn't connect to example.com:5222: setup failed"))
                     fsm-event)))))

(ert-deftest jabber-conn-test-keeps-failed-buffer-when-debugging ()
  "Debug buffer retention preserves failed process buffers."
  (let ((jabber-debug-keep-process-buffers t)
        (buffer (generate-new-buffer " *jabber-test-process*"))
        proc)
    (unwind-protect
        (progn
          (setq proc (make-pipe-process
                      :name "jabber-test-process"
                      :buffer buffer))
          (jabber-conn--delete-failed-process proc buffer)
          (should-not (process-live-p proc))
          (should (buffer-live-p buffer)))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'jabber-test-conn)

;;; jabber-test-conn.el ends here
