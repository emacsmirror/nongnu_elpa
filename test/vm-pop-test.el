;;; vm-pop-test.el --- Tests for vm-pop.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM POP functions in vm-pop.el
;; Tests pure functions directly.

;;; Code:

(require 'vm-test-init)
(require 'vm-pop)

;;; vm-pop-parse-spec-to-list tests

(ert-deftest vm-pop-test-parse-spec-basic ()
  "Test parsing a basic POP spec (without protocol prefix)."
  (let ((result (vm-pop-parse-spec-to-list
                 "mail.example.com:110:pass:user:secret")))
    (should (equal (nth 0 result) "mail.example.com"))
    (should (equal (nth 1 result) "110"))
    (should (equal (nth 2 result) "pass"))
    (should (equal (nth 3 result) "user"))
    (should (equal (nth 4 result) "secret"))))

(ert-deftest vm-pop-test-parse-spec-with-protocol ()
  "Test parsing a POP spec with protocol prefix."
  (let ((result (vm-pop-parse-spec-to-list
                 "pop:mail.example.com:110:pass:user:secret")))
    (should (equal (nth 0 result) "pop"))
    (should (equal (nth 1 result) "mail.example.com"))
    (should (equal (nth 2 result) "110"))
    (should (equal (nth 3 result) "pass"))
    (should (equal (nth 4 result) "user"))
    (should (equal (nth 5 result) "secret"))))

(ert-deftest vm-pop-test-parse-spec-ssl ()
  "Test parsing a POP-SSL spec."
  (let ((result (vm-pop-parse-spec-to-list
                 "pop-ssl:mail.example.com:995:pass:user:secret")))
    (should (equal (nth 0 result) "pop-ssl"))
    (should (equal (nth 2 result) "995"))))

(ert-deftest vm-pop-test-parse-spec-ssh ()
  "Test parsing a POP-SSH spec."
  (let ((result (vm-pop-parse-spec-to-list
                 "pop-ssh:mail.example.com:110:pass:user:secret")))
    (should (equal (nth 0 result) "pop-ssh"))))

;;; vm-pop-make-filename-for-spec tests

(ert-deftest vm-pop-test-make-filename-returns-string ()
  "Test that make-filename-for-spec returns a string."
  (let ((vm-pop-folder-cache-directory nil)
        (vm-folder-directory nil))
    (should (stringp (vm-pop-make-filename-for-spec
                      "pop:mail.example.com:110:pass:user:secret")))))

(ert-deftest vm-pop-test-make-filename-includes-prefix ()
  "Test that make-filename includes 'pop-cache-' prefix."
  (let ((vm-pop-folder-cache-directory nil)
        (vm-folder-directory nil)
        (result (vm-pop-make-filename-for-spec
                 "pop:mail.example.com:110:pass:user:secret")))
    (should (string-match "pop-cache-" result))))

(ert-deftest vm-pop-test-make-filename-scrub-password ()
  "Test that different passwords give same filename when scrubbed."
  (let ((vm-pop-folder-cache-directory nil)
        (vm-folder-directory nil)
        (result1 (vm-pop-make-filename-for-spec
                  "pop:mail.example.com:110:pass:user:secret1" t))
        (result2 (vm-pop-make-filename-for-spec
                  "pop:mail.example.com:110:pass:user:secret2" t)))
    (should (equal result1 result2))))

(ert-deftest vm-pop-test-make-filename-scrub-spec ()
  "Test that scrub-spec normalizes protocol and auth."
  (let ((vm-pop-folder-cache-directory nil)
        (vm-folder-directory nil)
        (result1 (vm-pop-make-filename-for-spec
                  "pop:mail.example.com:110:pass:user:secret" t t))
        (result2 (vm-pop-make-filename-for-spec
                  "pop-ssl:mail.example.com:995:apop:user:secret" t t)))
    ;; Both should normalize to same filename
    (should (equal result1 result2))))

;;; vm-pop-find-spec-for-name / vm-pop-find-name-for-spec tests

(ert-deftest vm-pop-test-find-spec-for-name ()
  "Test finding POP spec from name."
  (let ((vm-pop-folder-alist
         '(("pop:mail.example.com:110:pass:user:*" "work")
           ("pop:personal.com:995:pass:me:*" "home"))))
    (should (equal (vm-pop-find-spec-for-name "work")
                   "pop:mail.example.com:110:pass:user:*"))
    (should (equal (vm-pop-find-spec-for-name "home")
                   "pop:personal.com:995:pass:me:*"))))

(ert-deftest vm-pop-test-find-spec-for-name-not-found ()
  "Test that nil is returned when name not found."
  (let ((vm-pop-folder-alist
         '(("pop:mail.example.com:110:pass:user:*" "work"))))
    (should (null (vm-pop-find-spec-for-name "unknown")))))

(ert-deftest vm-pop-test-find-name-for-spec ()
  "Test finding name from POP spec."
  (let ((vm-pop-folder-alist
         '(("pop:mail.example.com:110:pass:user:*" "work")
           ("pop:personal.com:995:pass:me:*" "home"))))
    (should (equal (vm-pop-find-name-for-spec
                    "pop:mail.example.com:110:pass:user:*")
                   "work"))))

(ert-deftest vm-pop-test-find-name-for-spec-not-found ()
  "Test that nil is returned when spec not found."
  (let ((vm-pop-folder-alist
         '(("pop:mail.example.com:110:pass:user:*" "work"))))
    (should (null (vm-pop-find-name-for-spec
                   "pop:unknown.com:110:pass:other:*")))))

;;; vm-safe-popdrop-string tests

(ert-deftest vm-pop-test-safe-popdrop-string-basic ()
  "Test safe-popdrop-string extracts user@host."
  (should (equal (vm-safe-popdrop-string
                  "pop:mail.example.com:110:pass:myuser:secret")
                 "myuser@mail.example.com")))

(ert-deftest vm-pop-test-safe-popdrop-string-no-protocol ()
  "Test safe-popdrop-string without protocol prefix."
  (should (equal (vm-safe-popdrop-string
                  "mail.example.com:110:pass:myuser:secret")
                 "myuser@mail.example.com")))

(ert-deftest vm-pop-test-safe-popdrop-string-ssl ()
  "Test safe-popdrop-string with pop-ssl."
  (should (equal (vm-safe-popdrop-string
                  "pop-ssl:mail.example.com:995:pass:myuser:secret")
                 "myuser@mail.example.com")))

(ert-deftest vm-pop-test-safe-popdrop-string-invalid ()
  "Test safe-popdrop-string with invalid string."
  (should (equal (vm-safe-popdrop-string "invalid") "???")))

;;; vm-popdrop-sans-password tests

(ert-deftest vm-pop-test-popdrop-sans-password ()
  "Test removing password from popdrop."
  (let ((result (vm-popdrop-sans-password
                 "pop:mail.example.com:110:pass:user:secret")))
    (should (string-match ":user:\\*$" result))
    (should-not (string-match "secret" result))))

(ert-deftest vm-pop-test-popdrop-sans-password-no-protocol ()
  "Test removing password from popdrop without protocol."
  (let ((result (vm-popdrop-sans-password
                 "mail.example.com:110:pass:user:secret")))
    (should (string-match ":user:\\*$" result))))

;;; vm-popdrop-sans-personal-info tests

(ert-deftest vm-pop-test-popdrop-sans-personal-info ()
  "Test removing user and password from popdrop."
  (let ((result (vm-popdrop-sans-personal-info
                 "pop:mail.example.com:110:pass:user:secret")))
    (should (string-match ":\\*:\\*$" result))
    (should-not (string-match "user" result))
    (should-not (string-match "secret" result))))

;;; Error condition tests

(ert-deftest vm-pop-test-error-conditions-defined ()
  "Test that POP error conditions are defined."
  (should (get 'vm-cant-uidl 'error-conditions))
  (should (get 'vm-dele-failed 'error-conditions))
  (should (get 'vm-uidl-failed 'error-conditions)))

(ert-deftest vm-pop-test-error-hierarchy ()
  "Test that POP errors inherit from 'error."
  (should (memq 'error (get 'vm-cant-uidl 'error-conditions)))
  (should (memq 'error (get 'vm-dele-failed 'error-conditions)))
  (should (memq 'error (get 'vm-uidl-failed 'error-conditions))))

(ert-deftest vm-pop-test-signal-cant-uidl ()
  "Test signaling vm-cant-uidl error."
  (should-error (signal 'vm-cant-uidl '("test"))
                :type 'vm-cant-uidl))

(ert-deftest vm-pop-test-signal-dele-failed ()
  "Test signaling vm-dele-failed error."
  (should-error (signal 'vm-dele-failed '("test"))
                :type 'vm-dele-failed))

(ert-deftest vm-pop-test-signal-uidl-failed ()
  "Test signaling vm-uidl-failed error."
  (should-error (signal 'vm-uidl-failed '("test"))
                :type 'vm-uidl-failed))

;;; vm-pop-stat-* accessor tests

(ert-deftest vm-pop-test-stat-accessors ()
  "Test POP status blob accessors."
  (let ((blob (make-vector 12 nil)))
    ;; Set some values
    (aset blob 0 'timer)
    (aset blob 1 t)
    (aset blob 2 "inbox")
    (aset blob 3 5)
    (aset blob 4 10)
    ;; Test accessors
    (should (eq (vm-pop-stat-timer blob) 'timer))
    (should (eq (vm-pop-stat-did-report blob) t))
    (should (equal (vm-pop-stat-x-box blob) "inbox"))
    (should (= (vm-pop-stat-x-currmsg blob) 5))
    (should (= (vm-pop-stat-x-maxmsg blob) 10))))

;;; vm-maildrop-sans-password tests (handles both POP and IMAP)

(ert-deftest vm-pop-test-maildrop-sans-password-pop ()
  "Test vm-maildrop-sans-password with POP spec."
  (let ((result (vm-maildrop-sans-password
                 "pop:mail.example.com:110:pass:user:secret")))
    (should (string-match ":\\*$" result))))

(ert-deftest vm-pop-test-maildrop-sans-password-non-maildrop ()
  "Test vm-maildrop-sans-password with local path."
  (let ((result (vm-maildrop-sans-password "/home/user/mail")))
    ;; Should return unchanged
    (should (equal result "/home/user/mail"))))

;;; POP cleanup region tests

(ert-deftest vm-pop-test-cleanup-region-crlf ()
  "Test vm-pop-cleanup-region converts CRLF to LF."
  (with-temp-buffer
    (insert "Line1\r\nLine2\r\nLine3\r\n")
    (vm-pop-cleanup-region (point-min) (point-max))
    (should (equal (buffer-string) "Line1\nLine2\nLine3\n"))))

(ert-deftest vm-pop-test-cleanup-region-leading-dot ()
  "Test vm-pop-cleanup-region removes leading dots."
  (with-temp-buffer
    (insert "Line1\n..escaped\n.removed\nLine4\n")
    (vm-pop-cleanup-region (point-min) (point-max))
    (should (string-match "escaped" (buffer-string)))
    (should-not (string-match "^\\.removed" (buffer-string)))))

;;; POP protocol tests with mock network layer

(ert-deftest vm-pop-test-send-command-records ()
  "Test vm-pop-send-command records command sent."
  (vm-test-with-pop-session '("+OK POP3 ready\r\n" "+OK\r\n")
    (let ((process vm-test-mock-process))
      (vm-pop-send-command process "USER testuser")
      (should (member "USER testuser\r\n" vm-test-mock-commands)))))

(ert-deftest vm-pop-test-send-command-hides-password ()
  "Test vm-pop-send-command obscures PASS command in buffer."
  (vm-test-with-pop-session '("+OK POP3 ready\r\n" "+OK\r\n")
    (let ((process vm-test-mock-process))
      (vm-pop-send-command process "PASS secret123")
      ;; Command should be sent
      (should (member "PASS secret123\r\n" vm-test-mock-commands))
      ;; But buffer should show <omitted>
      (should (string-match "PASS <omitted>" (buffer-string))))))

(ert-deftest vm-pop-test-read-response-ok ()
  "Test vm-pop-read-response returns t for +OK."
  (vm-test-with-pop-session '("+OK POP3 ready\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      (should (eq (vm-pop-read-response process) t)))))

(ert-deftest vm-pop-test-read-response-ok-with-string ()
  "Test vm-pop-read-response returns response string when requested."
  (vm-test-with-pop-session '("+OK Welcome to POP3\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      (let ((response (vm-pop-read-response process t)))
        (should (stringp response))
        (should (string-match "Welcome" response))))))

(ert-deftest vm-pop-test-read-response-err ()
  "Test vm-pop-read-response returns nil for -ERR."
  (vm-test-with-pop-session '("-ERR Authentication failed\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      (should (null (vm-pop-read-response process))))))

(ert-deftest vm-pop-test-read-stat-response ()
  "Test vm-pop-read-stat-response parses message count and size."
  (vm-test-with-pop-session '("+OK 5 12345\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      (let ((result (vm-pop-read-stat-response process)))
        (should (listp result))
        (should (= (car result) 5))      ; message count
        (should (= (cadr result) 12345)))))) ; total size

(ert-deftest vm-pop-test-read-stat-response-empty ()
  "Test vm-pop-read-stat-response with empty mailbox."
  (vm-test-with-pop-session '("+OK 0 0\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      (let ((result (vm-pop-read-stat-response process)))
        (should (= (car result) 0))
        (should (= (cadr result) 0))))))

(ert-deftest vm-pop-test-read-list-response ()
  "Test vm-pop-read-list-response parses message size."
  (vm-test-with-pop-session '("+OK 1 2048\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      (let ((result (vm-pop-read-list-response process)))
        (should (= result 2048))))))

(ert-deftest vm-pop-test-read-uidl-long-response ()
  "Test vm-pop-read-uidl-long-response parses multi-line UIDL."
  (vm-test-with-pop-session '("+OK\r\n1 UID001\r\n2 UID002\r\n3 UID003\r\n.\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      (let ((result (vm-pop-read-uidl-long-response process)))
        (should (listp result))
        (should (= (length result) 3))
        ;; Each entry is (msgnum-string . uidl), list is in reverse order
        (should (equal (car (nth 0 result)) "3"))
        (should (equal (cdr (nth 0 result)) "UID003"))
        (should (equal (car (nth 2 result)) "1"))
        (should (equal (cdr (nth 2 result)) "UID001"))))))

(ert-deftest vm-pop-test-read-uidl-no-support ()
  "Test vm-pop-read-uidl-long-response returns nil when UIDL not supported."
  (vm-test-with-pop-session '("-ERR UIDL not supported\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      (should (null (vm-pop-read-uidl-long-response process))))))

(ert-deftest vm-pop-test-read-past-dot-sentinel ()
  "Test vm-pop-read-past-dot-sentinel-line finds end of multi-line response."
  (vm-test-with-pop-session '("Line 1\r\nLine 2\r\nLine 3\r\n.\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      (vm-pop-read-past-dot-sentinel-line process)
      ;; vm-pop-read-point should now be past the dot line
      (should (>= vm-pop-read-point (point-max))))))

;;; POP session flow tests

(ert-deftest vm-pop-test-login-sequence ()
  "Test a typical POP login command sequence."
  (vm-test-with-pop-session
      '("+OK POP3 server ready\r\n"
        "+OK User accepted\r\n"
        "+OK Password accepted\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      ;; Read greeting
      (should (vm-pop-read-response process))
      ;; Send USER
      (vm-pop-send-command process "USER testuser")
      (should (vm-pop-read-response process))
      ;; Send PASS
      (vm-pop-send-command process "PASS secret")
      (should (vm-pop-read-response process))
      ;; Verify commands were sent
      (should (member "USER testuser\r\n" vm-test-mock-commands))
      (should (member "PASS secret\r\n" vm-test-mock-commands)))))

(ert-deftest vm-pop-test-stat-list-sequence ()
  "Test STAT and LIST command sequence."
  (vm-test-with-pop-session
      '("+OK 3 5000\r\n"
        "+OK 1 1500\r\n"
        "+OK 2 2000\r\n"
        "+OK 3 1500\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      ;; STAT
      (let ((stat (vm-pop-read-stat-response process)))
        (should (= (car stat) 3))
        (should (= (cadr stat) 5000)))
      ;; LIST for each message
      (vm-pop-send-command process "LIST 1")
      (should (= (vm-pop-read-list-response process) 1500))
      (vm-pop-send-command process "LIST 2")
      (should (= (vm-pop-read-list-response process) 2000))
      (vm-pop-send-command process "LIST 3")
      (should (= (vm-pop-read-list-response process) 1500)))))

(ert-deftest vm-pop-test-error-handling ()
  "Test handling of POP error responses."
  (vm-test-with-pop-session
      '("+OK Ready\r\n"
        "-ERR Invalid command\r\n")
    (let ((process vm-test-mock-process))
      (setq vm-pop-read-point (point-min-marker))
      ;; First response OK
      (should (vm-pop-read-response process))
      ;; Second response is error
      (vm-pop-send-command process "BADCMD")
      (should (null (vm-pop-read-response process))))))

;;; Spec parsing edge cases

(ert-deftest vm-pop-test-parse-spec-with-colons-in-password ()
  "Test parsing spec with colons in password field."
  ;; Note: the last field can contain colons
  (let ((result (vm-pop-parse-spec-to-list
                 "pop:mail.example.com:110:pass:user:secret:with:colons")))
    (should (equal (nth 0 result) "pop"))
    (should (equal (nth 5 result) "secret:with:colons"))))

(ert-deftest vm-pop-test-parse-spec-apop ()
  "Test parsing a POP spec with APOP auth."
  (let ((result (vm-pop-parse-spec-to-list
                 "pop:mail.example.com:110:apop:user:secret")))
    (should (equal (nth 3 result) "apop"))))

(ert-deftest vm-pop-test-parse-spec-asterisk-password ()
  "Test parsing spec with asterisk password (prompt for it)."
  (let ((result (vm-pop-parse-spec-to-list
                 "pop:mail.example.com:110:pass:user:*")))
    (should (equal (nth 5 result) "*"))))

(provide 'vm-pop-test)

;;; vm-pop-test.el ends here
