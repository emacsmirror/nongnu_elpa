;;; jabber-srv-tests.el --- Tests for jabber-srv  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-srv)
(require 'jabber-conn)

;;; Test data helpers

(defun jabber-srv-test--make-answer (priority weight port target)
  "Build an SRV answer alist."
  (list (list 'priority priority)
        (list 'weight weight)
        (list 'port port)
        (list 'target target)))

;;; Group by priority

(ert-deftest jabber-srv-test-group-by-priority-single ()
  "Single priority group."
  (let* ((a1 (jabber-srv-test--make-answer 10 50 5222 "a.example.com"))
         (a2 (jabber-srv-test--make-answer 10 50 5222 "b.example.com"))
         (groups (jabber-srv--group-by-priority (list a1 a2))))
    (should (= (length groups) 1))
    (should (= (caar groups) 10))
    (should (= (length (cdar groups)) 2))))

(ert-deftest jabber-srv-test-group-by-priority-multiple ()
  "Multiple priority groups sorted lowest first."
  (let* ((a1 (jabber-srv-test--make-answer 20 50 5222 "low.example.com"))
         (a2 (jabber-srv-test--make-answer 5 50 5222 "high.example.com"))
         (a3 (jabber-srv-test--make-answer 20 50 5223 "low2.example.com"))
         (groups (jabber-srv--group-by-priority (list a1 a2 a3))))
    (should (= (length groups) 2))
    (should (= (caar groups) 5))
    (should (= (caadr groups) 20))
    (should (= (length (cdadr groups)) 2))))

;;; Sort answers

(ert-deftest jabber-srv-test-sort-answers-preserves-all ()
  "Sort preserves all elements."
  (let* ((a1 (jabber-srv-test--make-answer 10 50 5222 "a.example.com"))
         (a2 (jabber-srv-test--make-answer 20 50 5222 "b.example.com"))
         (a3 (jabber-srv-test--make-answer 10 50 5223 "c.example.com"))
         (sorted (jabber-srv--sort-answers (list a1 a2 a3))))
    (should (= (length sorted) 3))
    ;; Priority 10 entries come before priority 20
    (let ((targets (mapcar (lambda (a) (cadr (assq 'target a))) sorted)))
      (should (member "a.example.com" targets))
      (should (member "b.example.com" targets))
      (should (member "c.example.com" targets))
      ;; b.example.com (priority 20) must be last
      (should (string= (nth 2 targets) "b.example.com")))))

;;; Fetch answers

(ert-deftest jabber-srv-test-fetch-answers-nil ()
  "No DNS results returns nil."
  (cl-letf (((symbol-function 'jabber-srv--dns-query)
             (lambda (_target)
               '((answers nil)))))
    (should (null (jabber-srv--fetch-answers "_xmpp-client._tcp.example.com")))))

(ert-deftest jabber-srv-test-fetch-answers-dot ()
  "Single dot target returns :dot."
  (cl-letf (((symbol-function 'jabber-srv--dns-query)
             (lambda (_target)
               `((answers
                  (((data ((priority 0) (weight 0) (port 0) (target "."))))))))))
    (should (eq :dot (jabber-srv--fetch-answers
                      "_xmpp-client._tcp.example.com")))))

(ert-deftest jabber-srv-test-fetch-answers-records ()
  "Normal records returned as list."
  (cl-letf (((symbol-function 'jabber-srv--dns-query)
             (lambda (_target)
               `((answers
                  (((data ((priority 10) (weight 50) (port 5222)
                           (target "xmpp.example.com"))))))))))
    (let ((result (jabber-srv--fetch-answers "_xmpp-client._tcp.example.com")))
      (should (listp result))
      (should (= (length result) 1))
      (should (string= (cadr (assq 'target (car result)))
                        "xmpp.example.com")))))

;;; Lookup mixed

(defun jabber-srv-test--mock-fetch (xmpps-answers xmpp-answers)
  "Return a mock for `jabber-srv--fetch-answers'.
XMPPS-ANSWERS is returned for _xmpps queries, XMPP-ANSWERS for _xmpp."
  (lambda (target)
    (cond
     ((string-match "^_xmpps-client" target) xmpps-answers)
     ((string-match "^_xmpp-client" target) xmpp-answers))))

(ert-deftest jabber-srv-test-lookup-mixed-both ()
  "Both services return records, merged by priority, with fallback appended."
  (cl-letf (((symbol-function 'jabber-srv--fetch-answers)
             (jabber-srv-test--mock-fetch
              (list (jabber-srv-test--make-answer 5 50 443 "tls.example.com"))
              (list (jabber-srv-test--make-answer 10 50 5222 "plain.example.com")))))
    (let ((result (jabber-srv-lookup-mixed "example.com")))
      ;; 2 SRV records + 1 fallback
      (should (= (length result) 3))
      ;; Priority 5 (direct TLS) comes first
      (should (string= (nth 0 (car result)) "tls.example.com"))
      (should (= (nth 1 (car result)) 443))
      (should (nth 2 (car result)))
      ;; Priority 10 (STARTTLS) comes second
      (should (string= (nth 0 (cadr result)) "plain.example.com"))
      (should (= (nth 1 (cadr result)) 5222))
      (should-not (nth 2 (cadr result)))
      ;; Fallback: example.com:5222 STARTTLS at the end
      (let ((last (car (last result))))
        (should (string= (nth 0 last) "example.com"))
        (should (= (nth 1 last) 5222))
        (should-not (nth 2 last))))))

(ert-deftest jabber-srv-test-lookup-mixed-only-xmpp ()
  "Only _xmpp-client returns records, fallback appended."
  (cl-letf (((symbol-function 'jabber-srv--fetch-answers)
             (jabber-srv-test--mock-fetch
              nil
              (list (jabber-srv-test--make-answer 10 50 5222 "plain.example.com")))))
    (let ((result (jabber-srv-lookup-mixed "example.com")))
      (should (= (length result) 2))
      (should (string= (nth 0 (car result)) "plain.example.com"))
      (should-not (nth 2 (car result)))
      ;; Fallback appended
      (should (equal (cadr result) '("example.com" 5222 nil))))))

(ert-deftest jabber-srv-test-lookup-mixed-only-xmpps ()
  "Only _xmpps-client returns records, with STARTTLS fallback."
  (cl-letf (((symbol-function 'jabber-srv--fetch-answers)
             (jabber-srv-test--mock-fetch
              (list (jabber-srv-test--make-answer 5 50 443 "tls.example.com"))
              nil)))
    (let ((result (jabber-srv-lookup-mixed "example.com")))
      (should (= (length result) 2))
      (should (string= (nth 0 (car result)) "tls.example.com"))
      (should (nth 2 (car result)))
      ;; Fallback
      (should (equal (cadr result) '("example.com" 5222 nil))))))

(ert-deftest jabber-srv-test-lookup-mixed-xmpps-dot ()
  "xmpps-client returns dot, only STARTTLS targets with fallback."
  (cl-letf (((symbol-function 'jabber-srv--fetch-answers)
             (jabber-srv-test--mock-fetch
              :dot
              (list (jabber-srv-test--make-answer 10 50 5222 "plain.example.com")))))
    (let ((result (jabber-srv-lookup-mixed "example.com")))
      (should (= (length result) 2))
      (should-not (nth 2 (car result)))
      (should (equal (cadr result) '("example.com" 5222 nil))))))

(ert-deftest jabber-srv-test-lookup-mixed-neither ()
  "Neither service returns records."
  (cl-letf (((symbol-function 'jabber-srv--fetch-answers)
             (jabber-srv-test--mock-fetch nil nil)))
    (should (null (jabber-srv-lookup-mixed "example.com")))))

(ert-deftest jabber-srv-test-lookup-mixed-xmpps-error ()
  "DNS error for xmpps is caught, xmpp results still used with fallback."
  (cl-letf (((symbol-function 'jabber-srv--fetch-answers)
             (lambda (target)
               (cond
                ((string-match "^_xmpps-client" target)
                 (error "DNS query failed"))
                ((string-match "^_xmpp-client" target)
                 (list (jabber-srv-test--make-answer 10 50 5222
                                                     "plain.example.com")))))))
    (let ((result (jabber-srv-lookup-mixed "example.com")))
      (should (= (length result) 2))
      (should (string= (nth 0 (car result)) "plain.example.com"))
      (should (equal (cadr result) '("example.com" 5222 nil))))))

;;; Fallback dedup

(ert-deftest jabber-srv-test-has-fallback-p ()
  "Detect existing domain:5222 STARTTLS in target list."
  (should (jabber-srv--has-fallback-p
           '(("example.com" 5222 nil)) "example.com"))
  (should-not (jabber-srv--has-fallback-p
               '(("other.example.com" 5222 nil)) "example.com"))
  (should-not (jabber-srv--has-fallback-p
               '(("example.com" 443 t)) "example.com")))

(ert-deftest jabber-srv-test-lookup-mixed-no-dup-fallback ()
  "No fallback appended when SRV already includes domain:5222 STARTTLS."
  (cl-letf (((symbol-function 'jabber-srv--fetch-answers)
             (jabber-srv-test--mock-fetch
              nil
              (list (jabber-srv-test--make-answer 10 50 5222 "example.com")))))
    (let ((result (jabber-srv-lookup-mixed "example.com")))
      ;; SRV already has example.com:5222, no dup fallback
      (should (= (length result) 1))
      (should (equal (car result) '("example.com" 5222 nil))))))

;;; jabber-srv-targets

(ert-deftest jabber-srv-test-targets-user-override ()
  "User-specified network-server returns single STARTTLS target."
  (let ((result (jabber-srv-targets "example.com" "custom.host" nil)))
    (should (= (length result) 1))
    (should (equal (car result) '("custom.host" 5222 nil)))))

(ert-deftest jabber-srv-test-targets-user-port ()
  "User-specified port returns single STARTTLS target."
  (let ((result (jabber-srv-targets "example.com" nil 5223)))
    (should (= (length result) 1))
    (should (equal (car result) '("example.com" 5223 nil)))))

(ert-deftest jabber-srv-test-targets-fallback ()
  "No SRV records falls back to server:5222."
  (cl-letf (((symbol-function 'jabber-srv-lookup-mixed)
             (lambda (_server) nil)))
    (let ((jabber-direct-tls-lookup t))
      (let ((result (jabber-srv-targets "example.com" nil nil)))
        (should (= (length result) 1))
        (should (equal (car result) '("example.com" 5222 nil)))))))

(provide 'jabber-srv-tests)

;;; jabber-srv-tests.el ends here
