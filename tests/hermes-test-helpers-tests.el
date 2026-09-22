;;; hermes-test-helpers-tests.el --- Shared fixture contracts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)
(require 'url-parse)

(ert-deftest hermes-test-wait-until-immediate-and-real-timer ()
  (should (eq (hermes-test--wait-until (lambda () 'ready) 0) 'ready))
  (let* (ready
         (timer (run-at-time 0 nil (lambda () (setq ready 'delivered)))))
    (unwind-protect
        (should (eq (hermes-test--wait-until (lambda () ready)) 'delivered))
      (cancel-timer timer))))

(ert-deftest hermes-test-wait-until-deadline-is-diagnostic ()
  (let ((failure (should-error
                  (hermes-test--wait-until (lambda () nil) 0.01 "missing reply")
                  :type 'ert-test-failed)))
    (should (string-match-p "missing reply" (cadr failure)))))

(ert-deftest hermes-test-event-loop-barrier-delivers-prior-notification ()
  (let* (delivered
         (timer (run-at-time 0 nil (lambda () (setq delivered t)))))
    (unwind-protect
        (progn
          (should-not delivered)
          (hermes-test--event-loop-barrier)
          (should delivered))
      (cancel-timer timer))))

(ert-deftest hermes-test-event-loop-barrier-unwinds-only-owned-timer ()
  (let* ((unrelated (run-at-time 60 nil #'ignore))
         (before (copy-sequence timer-list))
         owned)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'hermes-test--wait-until)
                     (lambda (&rest _)
                       (setq owned (cl-set-difference timer-list before))
                       (error "fixture wait failed"))))
            (should-error (hermes-test--event-loop-barrier)))
          (should (memq unrelated timer-list))
          (should-not (cl-intersection owned timer-list)))
      (cancel-timer unrelated))))

(ert-deftest hermes-test-http-server-frames-fragments-once ()
  (let ((calls 0) peer listener client response)
    (unwind-protect
        (hermes-test--with-http-server
         (lambda (process request)
           (setq peer process)
           (cl-incf calls)
           (should (string-suffix-p "\r\n\r\nbody" request))
           (hermes-test--http-reply process 200 "{}"))
         (lambda (url)
           (setq listener (get-process "hermes-test-http"))
           (let ((filter (process-filter listener)))
             (set-process-filter listener
                                 (lambda (process text)
                                   (setq peer process)
                                   (funcall filter process text))))
           (setq client (make-network-process
                         :name "hermes-test-http-client" :host "127.0.0.1"
                         :service (url-port (url-generic-parse-url url))
                         :coding 'binary :noquery t
                         :filter (lambda (_process text)
                                   (setq response (concat response text)))))
           (process-send-string client "POST / HTTP/1.1\r\nContent-Len")
           (hermes-test--wait-until
            (lambda () (and peer (process-get peer 'request))))
           (should (= calls 0))
           (process-send-string client "gth: 4\r\n\r\nbo")
           (hermes-test--wait-until
            (lambda () (string-suffix-p "bo" (process-get peer 'request))))
           (should (= calls 0))
           (process-send-string client "dy")
           (hermes-test--wait-until (lambda () response))
           (should (= calls 1))
           (should (string-match-p "Content-Length: 2\r\n" response))
           (process-send-string client "ignored")
           (hermes-test--wait-until
            (lambda () (string-suffix-p "ignored" (process-get peer 'request))))
           (should (= calls 1))))
      (when (processp client) (delete-process client)))
    (should-not (process-live-p peer))
    (should-not (process-live-p listener))))

(ert-deftest hermes-test-http-server-unwinds-after-client-error ()
  (let (listener peer client
        (unrelated (make-pipe-process :name "hermes-test-unrelated" :noquery t)))
    (unwind-protect
        (progn
          (should-error
           (hermes-test--with-http-server
            (lambda (process _request) (setq peer process))
            (lambda (url)
              (setq listener (get-process "hermes-test-http")
                    client (make-network-process
                            :name "hermes-test-http-client" :host "127.0.0.1"
                            :service (url-port (url-generic-parse-url url))
                            :coding 'binary :noquery t))
              (process-send-string client "GET / HTTP/1.1\r\n\r\n")
              (hermes-test--wait-until (lambda () peer))
              (error "fixture client failed"))))
          (should-not (process-live-p peer))
          (should-not (process-live-p listener))
          (should (process-live-p unrelated)))
      (when (processp client) (delete-process client))
      (delete-process unrelated))))

(ert-deftest hermes-test-suites-compose-without-running-or-exiting ()
  "Every ordinary suite is a require-able library, not a test runner."
  (let ((directory (file-name-directory
                    (locate-library "hermes-test-helpers-tests"))))
    (cl-letf (((symbol-function 'ert-run-tests-batch-and-exit)
               (lambda (&rest _) (ert-fail "Suite tried to run and exit")))
              ((symbol-function 'ert-run-tests-batch)
               (lambda (&rest _) (ert-fail "Suite tried to run tests")))
              ((symbol-function 'kill-emacs)
               (lambda (&rest _) (ert-fail "Suite tried to exit"))))
      (dolist (file (directory-files directory nil "-tests\\.el\\'"))
        (should (require (intern (file-name-base file))))))))

(provide 'hermes-test-helpers-tests)
;;; hermes-test-helpers-tests.el ends here
