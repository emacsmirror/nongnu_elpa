;;; hermes-system-tests.el --- Gateway status and log tests  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-system-api-uses-status-and-log-routes ()
  "System requests preserve status path and log tail query."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest args)
                 (push (list method path (plist-get args :query)) calls)
                 (hermes--promise-resolved '((ok . t))))))
      (hermes-system--api 'client "/api/status")
      (hermes-system--api 'client "/api/logs" '((lines . 25))))
    (should (member '("GET" "/api/status" nil) calls))
    (should (member '("GET" "/api/logs" ((lines . 25))) calls))))

(ert-deftest hermes-system-redacts-secret-shaped-log-values ()
  "Management log rendering removes token, API-key, and bearer values."
  (let ((safe (hermes-system--redact-text
               "token=abc api_key: xyz Authorization: Bearer credential-value")))
    (should-not (string-match-p "abc\\|xyz\\|credential-value" safe))
    (should (string-match-p "<redacted>" safe))))

(ert-deftest hermes-system-logs-bounds-and-preserves-tail-on-refresh ()
  "Log requests clamp their tail and refresh with the same query."
  (let (queries)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-then (funcall make-promise 'client)
                                       on-success)))
              ((symbol-function 'hermes-system--api)
               (lambda (_client _path &optional query)
                 (push query queries)
                 (hermes--promise-resolved '((lines . ("one")))))))
      (unwind-protect
          (progn
            (hermes-system-logs 900)
            (with-current-buffer "*Hermes Logs*"
              (funcall revert-buffer-function nil t)))
        (when-let* ((buffer (get-buffer "*Hermes Logs*")))
          (kill-buffer buffer))))
    (should (equal queries
                   '(((file . "agent") (lines . 500))
                     ((file . "agent") (lines . 500)))))))

(ert-deftest hermes-system-renders-request-errors-in-owning-buffer ()
  "A failed system request replaces the view with a visible error."
  (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
            ((symbol-function 'hermes-browser--run-on-client)
             (lambda (make-promise &optional on-success)
               (hermes--promise-catch
                (hermes--promise-then (funcall make-promise 'client) on-success)
                #'ignore)))
            ((symbol-function 'hermes-system--api)
             (lambda (&rest _)
               (hermes--promise-rejected "HTTP 503 unavailable"))))
    (unwind-protect
        (progn
          (hermes-system-status)
          (with-current-buffer "*Hermes Status*"
            (should (derived-mode-p 'hermes-system-mode))
            (should (string-match-p "Error: HTTP 503 unavailable"
                                    (buffer-string)))))
      (when-let* ((buffer (get-buffer "*Hermes Status*")))
        (kill-buffer buffer)))))

(ert-deftest hermes-system-ignores-stale-refresh-results ()
  "An older status response cannot replace a reopened status view."
  (let (callbacks)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'hermes-browser--run-on-client)
               (lambda (_make-promise &optional on-success)
                 (push on-success callbacks))))
      (unwind-protect
          (progn
            (hermes-system-status)
            (hermes-system-status)
            (funcall (car callbacks) '((gateway_state . "new")))
            (funcall (cadr callbacks) '((gateway_state . "old")))
            (with-current-buffer "*Hermes Status*"
              (should (string-match-p "new" (buffer-string)))
              (should-not (string-match-p "old" (buffer-string)))))
        (when-let* ((buffer (get-buffer "*Hermes Status*")))
          (kill-buffer buffer))))))

(provide 'hermes-system-tests)
;;; hermes-system-tests.el ends here
