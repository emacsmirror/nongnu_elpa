;;; jabber-disco-tests.el --- Tests for jabber-disco  -*- lexical-binding: t; -*-

(require 'ert)

;; Pre-define variables expected at load time.
(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)
(defvar jabber-jid-obarray (make-vector 127 0))

(require 'jabber-disco)
(require 'jabber-db)

;;; Group 1: jabber-caps--store-hash

(ert-deftest jabber-disco-test-store-hash-sets-caps-on-resource ()
  "Storing a caps hash sets the caps property on the resource plist."
  (let ((jabber-jid-obarray (make-vector 127 0)))
    (jabber-caps--store-hash "alice@example.com/mobile" '("sha-1" . "abc123"))
    (let* ((sym (intern-soft "alice@example.com" jabber-jid-obarray))
           (resources (get sym 'resources))
           (entry (assoc "mobile" resources)))
      (should entry)
      (should (equal (plist-get (cdr entry) 'caps)
                     '("sha-1" . "abc123"))))))

(ert-deftest jabber-disco-test-store-hash-updates-existing-resource ()
  "Storing a caps hash updates an existing resource entry, not duplicating it."
  (let ((jabber-jid-obarray (make-vector 127 0)))
    ;; Store initial caps.
    (jabber-caps--store-hash "alice@example.com/mobile" '("sha-1" . "v1"))
    ;; Update caps for same resource.
    (jabber-caps--store-hash "alice@example.com/mobile" '("sha-1" . "v2"))
    (let* ((sym (intern-soft "alice@example.com" jabber-jid-obarray))
           (resources (get sym 'resources))
           (matching (cl-remove-if-not
                      (lambda (r) (string= (car r) "mobile"))
                      resources)))
      ;; Only one resource entry for "mobile".
      (should (= (length matching) 1))
      ;; Updated to v2.
      (should (equal (plist-get (cdr (car matching)) 'caps)
                     '("sha-1" . "v2"))))))

(ert-deftest jabber-disco-test-store-hash-bare-jid ()
  "Storing caps for a bare JID (no resource) uses empty string as resource key."
  (let ((jabber-jid-obarray (make-vector 127 0)))
    (jabber-caps--store-hash "bob@example.com" '("sha-256" . "xyz"))
    (let* ((sym (intern-soft "bob@example.com" jabber-jid-obarray))
           (resources (get sym 'resources))
           (entry (assoc "" resources)))
      (should entry)
      (should (equal (plist-get (cdr entry) 'caps)
                     '("sha-256" . "xyz"))))))

;;; Group 2: jabber-caps--query-if-needed

(ert-deftest jabber-disco-test-query-if-needed-cache-hit ()
  "On cache hit, disco info is copied to jabber-disco-info-cache."
  (let ((jabber-caps-cache (make-hash-table :test 'equal))
        (jabber-disco-info-cache (make-hash-table :test 'equal))
        (cached-data '(("id1") ("feat1" "feat2")))
        (key '("sha-1" . "ver1")))
    (puthash key cached-data jabber-caps-cache)
    (jabber-caps--query-if-needed nil "alice@example.com/res"
                                  "sha-1" "http://node" "ver1"
                                  key cached-data)
    (should (equal (gethash '("alice@example.com/res" . nil)
                            jabber-disco-info-cache)
                   cached-data))))

(ert-deftest jabber-disco-test-query-if-needed-cache-miss ()
  "On cache miss, a pending entry is created in jabber-caps-cache."
  (let ((jabber-caps-cache (make-hash-table :test 'equal))
        (jabber-disco-info-cache (make-hash-table :test 'equal))
        (key '("sha-1" . "ver1"))
        (iq-sent nil))
    ;; Stub jabber-send-iq to record the call without needing a connection.
    (cl-letf (((symbol-function 'jabber-send-iq)
               (lambda (&rest _args) (setq iq-sent t))))
      (jabber-caps--query-if-needed nil "alice@example.com/res"
                                    "sha-1" "http://node" "ver1"
                                    key nil)
      ;; A pending entry should exist.
      (let ((entry (gethash key jabber-caps-cache)))
        (should (consp entry))
        (should (floatp (car entry))))
      ;; An IQ query should have been dispatched.
      (should iq-sent))))

(ert-deftest jabber-disco-test-query-if-needed-pending-recent ()
  "On recent pending query (<10s), JID is added to fallback list."
  (let ((jabber-caps-cache (make-hash-table :test 'equal))
        (jabber-disco-info-cache (make-hash-table :test 'equal))
        (key '("sha-1" . "ver1"))
        (pending-entry (list (float-time))))
    (puthash key pending-entry jabber-caps-cache)
    (jabber-caps--query-if-needed nil "bob@example.com/laptop"
                                  "sha-1" "http://node" "ver1"
                                  key pending-entry)
    ;; bob's JID should be in the fallback list (cdr of entry).
    (should (member "bob@example.com/laptop" (cdr pending-entry)))))

(ert-deftest jabber-disco-test-query-if-needed-pending-stale ()
  "On stale pending query (>10s), a new disco query is sent."
  (let ((jabber-caps-cache (make-hash-table :test 'equal))
        (jabber-disco-info-cache (make-hash-table :test 'equal))
        (key '("sha-1" . "ver1"))
        ;; Create a pending entry from 15 seconds ago.
        (pending-entry (list (- (float-time) 15.0)))
        (iq-sent nil))
    (puthash key pending-entry jabber-caps-cache)
    (cl-letf (((symbol-function 'jabber-send-iq)
               (lambda (&rest _args) (setq iq-sent t))))
      (jabber-caps--query-if-needed nil "carol@example.com/phone"
                                    "sha-1" "http://node" "ver1"
                                    key pending-entry)
      ;; Timestamp should be refreshed (recent).
      (should (< (- (float-time) (car pending-entry)) 2.0))
      ;; A new IQ query should have been dispatched.
      (should iq-sent))))

;;; Group 3: jabber-process-caps-modern (integration)

(ert-deftest jabber-disco-test-process-caps-modern-unsupported-hash ()
  "When the hash algorithm is not in jabber-caps-hash-names, nothing happens."
  (let ((jabber-jid-obarray (make-vector 127 0))
        (jabber-caps-cache (make-hash-table :test 'equal))
        (jabber-disco-info-cache (make-hash-table :test 'equal)))
    ;; "md5" is not in jabber-caps-hash-names.
    (jabber-process-caps-modern nil "alice@example.com/res" "md5" "http://node" "ver1")
    ;; No symbol should have been interned for this JID.
    (should-not (intern-soft "alice@example.com" jabber-jid-obarray))))

(ert-deftest jabber-disco-test-process-caps-modern-stores-and-queries ()
  "With a supported hash and empty cache, store-hash and query are both called."
  (let ((jabber-jid-obarray (make-vector 127 0))
        (jabber-caps-cache (make-hash-table :test 'equal))
        (jabber-disco-info-cache (make-hash-table :test 'equal))
        (iq-sent nil))
    (cl-letf (((symbol-function 'jabber-send-iq)
               (lambda (&rest _args) (setq iq-sent t))))
      (jabber-process-caps-modern nil "alice@example.com/phone"
                                  "sha-1" "http://node" "ver1")
      ;; Hash should be stored on the resource.
      (let* ((sym (intern-soft "alice@example.com" jabber-jid-obarray))
             (resources (get sym 'resources))
             (entry (assoc "phone" resources)))
        (should entry)
        (should (equal (plist-get (cdr entry) 'caps)
                       '("sha-1" . "ver1"))))
      ;; Query should have been sent.
      (should iq-sent))))

(provide 'jabber-disco-tests)
;;; jabber-disco-tests.el ends here
