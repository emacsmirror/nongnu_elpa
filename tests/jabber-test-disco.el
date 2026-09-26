;;; jabber-test-disco.el --- Tests for jabber-disco  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0030 Service Discovery and XEP-0115 Entity Caps.

;;; Code:

(require 'ert)

;; Pre-define variables expected at load time.
(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)
(defvar jabber-jid-obarray (make-vector 127 0))

(require 'jabber-disco)
(require 'jabber-db)
(require 'jabber-disco-test-helpers)

;;; Group 1: jabber-caps--store-hash

(ert-deftest jabber-test-disco-store-hash-sets-caps-on-resource ()
  "Store the mapping on the explicitly owned contact resource."
  (jabber-test-disco-owner--with-state
    (jabber-caps--store-hash "alice@example.com/mobile" '("sha-1" . "abc123") a)
    (let* ((sym (jabber-jid-symbol "alice@example.com" a))
           (entry (assoc "mobile" (get sym 'resources))))
      (should entry)
      (should (equal (plist-get (cdr entry) 'caps) '("sha-1" . "abc123")))
      (should-not (get (jabber-jid-symbol "alice@example.com" b) 'resources)))))

(ert-deftest jabber-test-disco-store-hash-updates-existing-resource ()
  "Update only the specified owner's existing resource."
  (jabber-test-disco-owner--with-state
    (jabber-caps--store-hash "alice@example.com/mobile" '("sha-1" . "v1") a)
    (jabber-caps--store-hash "alice@example.com/mobile" '("sha-1" . "b") b)
    (jabber-caps--store-hash "alice@example.com/mobile" '("sha-1" . "v2") a)
    (let ((resources (get (jabber-jid-symbol "alice@example.com" a) 'resources)))
      (should (= (length resources) 1))
      (should (equal (plist-get (cdar resources) 'caps) '("sha-1" . "v2"))))
    (should (equal (plist-get (cdar (get (jabber-jid-symbol "alice@example.com" b)
                                        'resources)) 'caps)
                   '("sha-1" . "b")))))

(ert-deftest jabber-test-disco-store-hash-bare-jid ()
  "Use the empty resource for a bare JID on its explicit owner."
  (jabber-test-disco-owner--with-state
    (jabber-caps--store-hash "bob@example.com" '("sha-256" . "xyz") a)
    (let ((entry (assoc "" (get (jabber-jid-symbol "bob@example.com" a) 'resources))))
      (should (equal (plist-get (cdr entry) 'caps) '("sha-256" . "xyz"))))))

;;; Group 2: jabber-caps--query-if-needed

(ert-deftest jabber-test-disco-query-if-needed-cache-hit ()
  "Reuse verified data globally but qualify observation aliases."
  (jabber-test-disco-owner--with-state
    (let ((info '(("id1") ("feat1" "feat2")))
          (key '("sha-1" . "ver1")))
      (puthash key info jabber-caps-cache)
      (jabber-caps--query-if-needed a "alice@example.com/res" "sha-1" "node" "ver1" key info)
      (should (eq (jabber-disco-get-info-immediately "alice@example.com/res" nil a) info))
      (should-not (jabber-disco-get-info-immediately "alice@example.com/res" nil b))
      (jabber-caps--query-if-needed b "alice@example.com/res" "sha-1" "node-b" "ver1" key info)
      (should (eq (jabber-disco-get-info-immediately "alice@example.com/res" nil b) info))
      (should-not wire))))

(ert-deftest jabber-test-disco-query-if-needed-cache-miss ()
  "Keep pending candidates out of the verified payload cache."
  (jabber-test-disco-owner--with-state
    (let ((key '("sha-1" . "ver1")))
      (jabber-caps--query-if-needed a "alice@example.com/res" "sha-1" "node" "ver1" key nil)
      (should-not (gethash key jabber-caps-cache))
      (should (gethash key jabber-caps--pending))
      (should (= (length wire) 1)))))

(ert-deftest jabber-test-disco-query-if-needed-pending-recent ()
  "Deduplicate by owned identity including the advertised node."
  (jabber-test-disco-owner--with-state
    (let ((key '("sha-1" . "ver1")))
      (jabber-caps--query-if-needed a "same.example/r" "sha-1" "node-a" "ver1" key nil)
      (jabber-caps--query-if-needed b "same.example/r" "sha-1" "node-b" "ver1" key nil)
      (jabber-caps--query-if-needed b "same.example/r" "sha-1" "node-b" "ver1" key nil)
      (jabber-caps--query-if-needed b "same.example/r" "sha-1" "node-c" "ver1" key nil)
      (should (= (length wire) 1))
      (should (= (length (plist-get (gethash key jabber-caps--pending) :queue)) 2)))))

(ert-deftest jabber-test-disco-query-if-needed-pending-stale ()
  "Settle a timed-out last candidate and admit a later advertisement."
  (jabber-test-disco-owner--with-state
    (let ((key '("sha-1" . "ver1")))
      (jabber-caps--query-if-needed a "same.example/r" "sha-1" "node" "ver1" key nil)
      (jabber-test-disco-owner--expire (gethash key jabber-caps--pending))
      (should-not (gethash key jabber-caps--pending))
      (jabber-caps--query-if-needed a "same.example/r" "sha-1" "node" "ver1" key nil)
      (should (= (length wire) 2)))))

;;; Group 3: jabber-process-caps-modern (integration)

(ert-deftest jabber-test-disco-parse-info-preserves-xdata-forms ()
  "Disco info parsing preserves XEP-0128 data forms."
  (let* ((form `(x ((xmlns . ,jabber-xdata-xmlns) (type . "result"))
                   (field ((var . "FORM_TYPE") (type . "hidden"))
                          (value () "urn:xmpp:http:upload:0"))
                   (field ((var . "max-file-size"))
                          (value () "5242880"))))
         (result (jabber-disco-parse-info
                  `(iq ((from . "upload.example.net") (type . "result"))
                       (query ((xmlns . ,jabber-disco-xmlns-info))
                              (identity ((category . "store")
                                         (type . "file")
                                         (name . "HTTP File Upload")))
                              (feature ((var . "urn:xmpp:http:upload:0")))
                              ,form)))))
    (should (equal (nth 1 result) '("urn:xmpp:http:upload:0")))
    (should (equal (nth 2 result) (list form)))))

(ert-deftest jabber-test-disco-process-caps-modern-unsupported-hash ()
  "When the hash algorithm is not in jabber-caps-hash-names, nothing happens."
  (let ((jabber-jid-obarray (make-vector 127 0))
        (jabber-caps-cache (make-hash-table :test 'equal))
        (jabber-disco-info-cache (make-hash-table :test 'equal)))
    ;; "md5" is not in jabber-caps-hash-names.
    (jabber-process-caps-modern nil "alice@example.com/res" "md5" "http://node" "ver1")
    ;; No symbol should have been interned for this JID.
    (should-not (intern-soft "alice@example.com" jabber-jid-obarray))))

(ert-deftest jabber-test-disco-process-caps-modern-stores-and-queries ()
  "Store the scoped resource and dispatch through the advertised owner."
  (jabber-test-disco-owner--with-state
    (jabber-process-caps-modern a "alice@example.com/phone" "sha-1" "node" "ver1")
    (let ((entry (assoc "phone" (get (jabber-jid-symbol "alice@example.com" a)
                                    'resources))))
      (should (equal (plist-get (cdr entry) 'caps) '("sha-1" . "ver1"))))
    (should (eq (caar wire) a))))

(ert-deftest jabber-test-disco-advertise-feature-runs-change-hook-once ()
  "Advertising a new connected feature runs the change hook once."
  (let ((jabber-advertised-features nil)
        (jabber-caps-current-hash "old")
        (jabber-disco-features-changed-hook nil)
        (recalculations 0)
        (changes 0))
    (add-hook 'jabber-disco-features-changed-hook
              (lambda () (cl-incf changes)))
    (cl-letf (((symbol-function 'jabber-caps-recalculate-hash)
               (lambda () (cl-incf recalculations))))
      (jabber-disco-advertise-feature "urn:test:feature")
      (jabber-disco-advertise-feature "urn:test:feature"))
    (should (= recalculations 1))
    (should (= changes 1))))

(ert-deftest jabber-test-disco-caps-xep-0115-known-answer ()
  "Match the XEP-0115 Unicode identities and data forms reference hash."
  (let ((query
         (with-temp-buffer
           (insert "<query xmlns='http://jabber.org/protocol/disco#info'
           node='http://psi-im.org#q07IKJEyjvHSyhy//CH0CxmKi8w='>
      <identity xml:lang='en' category='client' name='Psi 0.11' type='pc'/>
      <identity xml:lang='el' category='client' name='Ψ 0.11' type='pc'/>
      <feature var='http://jabber.org/protocol/caps'/>
      <feature var='http://jabber.org/protocol/disco#info'/>
      <feature var='http://jabber.org/protocol/disco#items'/>
      <feature var='http://jabber.org/protocol/muc'/>
      <x xmlns='jabber:x:data' type='result'>
        <field var='FORM_TYPE' type='hidden'>
          <value>urn:xmpp:dataforms:softwareinfo</value>
        </field>
        <field var='ip_version'>
          <value>ipv4</value>
          <value>ipv6</value>
        </field>
        <field var='os'>
          <value>Mac</value>
        </field>
        <field var='os_version'>
          <value>10.5.1</value>
        </field>
        <field var='software'>
          <value>Psi</value>
        </field>
        <field var='software_version'>
          <value>0.11</value>
        </field>
      </x>
    </query>")
           (car (xml-parse-region (point-min) (point-max))))))
    (should (equal "q07IKJEyjvHSyhy//CH0CxmKi8w="
                   (jabber-caps-ver-string query "sha-1")))))

(provide 'jabber-test-disco)
;;; jabber-test-disco.el ends here
