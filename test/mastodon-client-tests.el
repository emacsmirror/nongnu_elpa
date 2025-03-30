;;; mastodon-client-test.el --- Tests for mastodon-client.el  -*- lexical-binding: nil -*-

(require 'el-mock)
(require 'mastodon-client)
(require 'mastodon-http)

(ert-deftest mastodon-client--register ()
  "Should POST to /apps."
  (with-mock
    (mock (mastodon-http--api "apps") => "https://instance.url/api/v1/apps")
    (mock (mastodon-http--post "https://instance.url/api/v1/apps"
                               '(("client_name" . "mastodon.el")
                                 ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                                 ("scopes" . "read write follow")
                                 ("website" . "https://codeberg.org/martianh/mastodon.el"))
                               nil
                               :unauthenticated))
    (mastodon-client--register)))

(ert-deftest mastodon-client--fetch ()
  "Should return client registration JSON."
  (with-temp-buffer
    (with-mock
     (mock (mastodon-client--register) => (progn
                                            (insert "\n\n{\"foo\":\"bar\"}")
                                            (current-buffer)))
     (should (equal (mastodon-client--fetch) '(:foo "bar"))))))

;; FIXME: broken by new encrypted plstore flow
;; (asks for gpg passphrase)
;; otherwise test passes
(ert-deftest mastodon-client--store ()
  "Test the value `mastodon-client--store' returns/stores."
  (let ((mastodon-instance-url "http://mastodon.example")
        (plist '(:client_id "id" :client_secret "secret")))
    (with-mock
     (mock (mastodon-client--token-file) => "stubfile.plstore")
     (mock (mastodon-client--fetch) => plist)
     (should (equal (mastodon-client--store) plist)))
    (let* ((plstore (plstore-open "stubfile.plstore"))
           (client (mastodon-client--remove-key-from-plstore
                    (plstore-get plstore "mastodon-http://mastodon.example"))))
      (plstore-close plstore)
      (should (equal client plist))
      ;; clean up - delete the stubfile
      (delete-file "stubfile.plstore"))))

(ert-deftest mastodon-client--read-finds-match ()
  "Should return mastodon client from `mastodon-token-file' if it exists."
  (let ((mastodon-instance-url "http://mastodon.example"))
    (with-mock
     (mock (mastodon-client--token-file) => "fixture/client.plstore")
     (should (equal (mastodon-client--read)
                    '(:client_id "id2" :client_secret "secret2"))))))

(ert-deftest mastodon-client--general-read-finds-match ()
  (with-mock
   (mock (mastodon-client--token-file) => "fixture/client.plstore")
   (should (equal (mastodon-client--general-read "user-test8000@mastodon.example")
                  '(:username "test8000@mastodon.example"
                              :instance "http://mastodon.example"
                              :client_id "id2" :client_secret "secret2"
                              :access_token "token2")))))

(ert-deftest mastodon-client--general-read-finds-no-match ()
  (with-mock
   (mock (mastodon-client--token-file) => "fixture/client.plstore")
   (should (equal (mastodon-client--general-read "nonexistant-key")
                  nil))))

(ert-deftest mastodon-client--general-read-empty-store ()
  (with-mock
   (mock (mastodon-client--token-file) => "fixture/empty.plstore")
   (should (equal (mastodon-client--general-read "something")
                  nil))))

(ert-deftest mastodon-client--read-finds-no-match ()
  "Should return mastodon client from `mastodon-token-file' if it exists."
  (let ((mastodon-instance-url "http://mastodon.social"))
    (with-mock
     (mock (mastodon-client--token-file) => "fixture/client.plstore")
     (should (equal (mastodon-client--read) nil)))))

(ert-deftest mastodon-client--read-empty-store ()
  "Should return nil if mastodon client is not present in the plstore."
  (with-mock
   (mock (mastodon-client--token-file) => "fixture/empty.plstore")
   (should (equal (mastodon-client--read) nil))))

(ert-deftest mastodon-client--client-set-and-matching ()
  "Should return `mastondon-client' if `mastodon-client--client-details-alist' is non-nil and instance url is included."
  (let ((mastodon-instance-url "http://mastodon.example")
        (mastodon-client--client-details-alist '(("https://other.example" . :no-match)
                                                 ("http://mastodon.example" . :matches))))
    (should (eq (mastodon-client) :matches))))

(ert-deftest mastodon-client--client-set-but-not-matching ()
  "Should read from `mastodon-token-file' if wrong data is cached."
  (let ((mastodon-instance-url "http://mastodon.example")
        (mastodon-client--client-details-alist '(("http://other.example" :wrong))))
    (with-mock
     (mock (mastodon-client--read) => '(:client_id "foo" :client_secret "bar"))
     (should (equal (mastodon-client) '(:client_id "foo" :client_secret "bar")))
     (should (equal mastodon-client--client-details-alist
                    '(("http://mastodon.example" :client_id "foo" :client_secret "bar")
                      ("http://other.example" :wrong)))))))

(ert-deftest mastodon-client--client-unset ()
  "Should read from `mastodon-token-file' if available."
  (let ((mastodon-instance-url "http://mastodon.example")
        (mastodon-client--client-details-alist nil))
    (with-mock
     (mock (mastodon-client--read) => '(:client_id "foo" :client_secret "bar"))
     (should (equal (mastodon-client) '(:client_id "foo" :client_secret "bar")))
     (should (equal mastodon-client--client-details-alist
                    '(("http://mastodon.example" :client_id "foo" :client_secret "bar")))))))

(ert-deftest mastodon-client--client-unset-and-not-in-storage ()
  "Should store client data in plstore if it can't be read."
  (let ((mastodon-instance-url "http://mastodon.example")
        (mastodon-client--client-details-alist nil))
    (with-mock
     (mock (mastodon-client--read))
     (mock (mastodon-client--store) => '(:client_id "foo" :client_secret "baz"))
     (should (equal (mastodon-client) '(:client_id "foo" :client_secret "baz")))
     (should (equal mastodon-client--client-details-alist
                    '(("http://mastodon.example" :client_id "foo" :client_secret "baz")))))))

(ert-deftest mastodon-client--form-user-from-vars ()
  (let ((mastodon-active-user "test9000")
        (mastodon-instance-url "https://mastodon.example"))
    (should
     (equal (mastodon-client--form-user-from-vars)
            "test9000@mastodon.example"))))

(ert-deftest mastodon-client--current-user-active-p ()
  (let ((mastodon-active-user "test9000")
        (mastodon-instance-url "https://mastodon.example"))
    ;; when the current user /is/ the active user
    (with-mock
     (mock (mastodon-client--general-read "active-user") => '(:username "test9000@mastodon.example" :client_id "id1"))
     (should (equal (mastodon-client--current-user-active-p)
                    '(:username "test9000@mastodon.example" :client_id "id1"))))
    ;; when the current user is /not/ the active user
    (with-mock
     (mock (mastodon-client--general-read "active-user") => '(:username "user@other.example" :client_id "id1"))
     (should (null (mastodon-client--current-user-active-p))))))

;; FIXME: broken by new encrypted plstore flow
;; (asks for gpg passphrase)
;; otherwise test passes
(ert-deftest mastodon-client--store-access-token ()
  (let ((mastodon-instance-url "https://mastodon.example")
        (mastodon-active-user "test8000")
        (user-details ;; order changed for new encrypted auth flow:
         '( :client_id "id" :client_secret "secret"
            :access_token "token"
            :username "test8000@mastodon.example"
            :instance "https://mastodon.example"))
        (mastodon-auth-use-auth-source nil)) ;; FIXME: test auth source
    ;; test if mastodon-client--store-access-token /returns/ right
    ;; value
    (with-mock
     (mock (mastodon-client) => '(:client_id "id" :client_secret "secret"))
     (mock (mastodon-client--token-file) => "stubfile.plstore")
     (should (equal (mastodon-client--store-access-token "token")
                    user-details)))
    ;; test if mastodon-client--store-access-token /stores/ right value
    (with-mock
     (mock (mastodon-client--token-file) => "stubfile.plstore")
     (should (equal (mastodon-client--general-read
                     "user-test8000@mastodon.example")
                    user-details)))
    (delete-file "stubfile.plstore")))

;; FIXME: broken by new encrypted plstore flow
;; (asks for gpg passphrase)
;; otherwise test passes
(ert-deftest mastodon-client--make-user-active ()
  ;; match new encrypted plstore return value:
  (let ((user-details '( :access_token nil
                         :client_id nil
                         :client_secret nil
                         :username "test@mastodon.example"))
        (mastodon-auth-use-auth-source nil)) ;; FIXME: test auth source
    (with-mock
     (mock (mastodon-client--token-file) => "stubfile.plstore")
     (mastodon-client--make-user-active user-details)
     (should (equal (mastodon-client--general-read "active-user")
                    user-details)))
    (delete-file "stubfile.plstore")))
