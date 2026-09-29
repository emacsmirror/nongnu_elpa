;;; mastodon-auth-test.el --- Tests for mastodon-auth.el  -*- lexical-binding: nil -*-

(require 'el-mock)
(require 'mastodon)
(require 'mastodon-auth)

;; NB: since switching to encrypted (client) plstore, some tests fail if
;; `plistore-encrypt-to' is not set to a working gpg key

;; NB: since adding `mastodon-auth-encrypt-tokens-plstore', we just nil it everywhere
;; and don't test encrypted plstore at all.

(ert-deftest mastodon-auth--handle-token-response--good ()
  "Should extract the access token from a good response."
  (let ((mastodon-auth-encrypt-tokens-plstore nil)
        ;; else we are interactively asked to save to ~/authinfo.gpg:
        (mastodon-auth-use-auth-source nil))
    (should
     (string=
      "foo"
      (mastodon-auth--handle-token-response
       '(:access_token "foo" :token_type "Bearer" :scope "read write follow" :created_at 0))))))

(ert-deftest mastodon-auth--handle-token-response--unknown ()
  "Should throw an error when the response is unparsable."
  (should
   (equal
    '(error "Unknown response from mastodon-auth--get-token!")
    (condition-case error
        (progn
          (mastodon-auth--handle-token-response '(:herp "derp"))
          nil)
      (t error)))))

(ert-deftest mastodon-auth--handle-token-response--failure ()
  "Should throw an error when the response indicates an error."
  (let ((error-message "The provided authorization grant is invalid, expired, revoked, does not match the redirection URI used in the authorization request, or was issued to another client."))
    (should
     (equal
      `(error ,(format "Mastodon-auth--access-token: invalid_grant: %s" error-message))
      (condition-case error
          (mastodon-auth--handle-token-response
           `(:error "invalid_grant" :error_description ,error-message))
        (t error))))))

(ert-deftest mastodon-auth--get-token ()
  "Should generate token and return JSON response."
  (with-temp-buffer
    (with-mock
      (mock (mastodon-auth--generate-token) => (progn
                                                 (insert "\n\n{\"access_token\":\"abcdefg\"}")
                                                 (current-buffer)))
      (should
       (equal (mastodon-auth--get-token)
              '(:access_token "abcdefg"))))))

(ert-deftest mastodon-auth--access-token-found ()
  "Should return value in `mastodon-auth--token-alist' if found."
  (let ((mastodon-instance-url "https://instance.url")
        (mastodon-auth--token-alist '(("https://instance.url" . "foobar")) ))
    (should
     (string= (mastodon-auth--access-token) "foobar"))))

(ert-deftest mastodon-auth--access-token-not-found ()
  "Should set and return `mastodon-auth--token' if nil."
  (let ((mastodon-instance-url "https://instance.url")
        (mastodon-active-user "user")
        (mastodon-auth--token-alist nil))
    (with-mock
      (mock (mastodon-auth--get-token) => '(:access_token "foobaz"))
      (mock (mastodon-client--store-access-token "foobaz"))
      (stub mastodon-client--make-user-active)
      (should
       (string= (mastodon-auth--access-token)
                "foobaz"))
      (should
       (equal mastodon-auth--token-alist
              '(("https://instance.url" . "foobaz")))))))

(ert-deftest mastodon-auth--user-unaware ()
  (let ((mastodon-instance-url "https://instance.url")
        (mastodon-active-user nil)
        (mastodon-auth--token-alist nil))
    (with-mock
      (mock (mastodon-client--active-user))
      (should-error (mastodon-auth--access-token)))))

(ert-deftest mastodon-auth-plstore-token-check ()
  (let* ((mastodon-instance-url "https://mastodon.example")
         (mastodon-active-user "test8000")
         (mastodon-auth-encrypt-tokens-plstore nil)
         (mastodon-auth-use-auth-source nil) ;; :access_token is stored in plstore
         ;; if clause so we can not lose the encrypted plist structure:
         (user-details ;; order changed for new encrypted auth flow:
          (if mastodon-auth-encrypt-tokens-plstore
              '( :client_id "id" :client_secret "secret"
                 :access_token "token"
                 :username "test8000@mastodon.example"
                 :instance "https://mastodon.example")
            '( :username "test8000@mastodon.example"
               :instance "https://mastodon.example"
               :client_id "id"
               :client_secret "secret"
               :access_token "token"))))
    ;; setup plstore: store access token, not using auth source:
    (with-mock
     (mock (mastodon-client) => '(:client_id "id" :client_secret "secret"))
     (mock (mastodon-client--token-file) => "stubfile.plstore")
     (should
      (equal (mastodon-client--store-access-token "token")
             user-details))
     ;; should non-nil if we check with auth-source:
     ;; because we saved with non auth-source:
     (let ((mastodon-auth-use-auth-source t))
       (should
        (equal
         (mastodon-auth--plstore-access-token-member :auth-source)
         ;; if clause so we can not lose the encrypted plist structure:
         (if mastodon-auth-encrypt-tokens-plstore
             '(:secret-access_token t :username "test8000@mastodon.example"
                                    :instance "https://mastodon.example")
           '(:access_token "token")))))
     ;; FIXME: ideally we would also mock up a non-encrypted plstore and
     ;; test against it too, as that's the work we really want
     ;; `mastodon-auth--plstore-access-token-member' to do
     ;; but we don't currently have a way to mock one up.
     (delete-file "stubfile.plstore"))))

(ert-deftest mastodon-auth-plstore-token-check-auth-source ()
  (let* ((mastodon-instance-url "https://mastodon.example")
         (mastodon-active-user "test8000")
         (file "fixture/stubfile-auth-source.plstore")
         (mastodon-auth-encrypt-tokens-plstore nil)
         ;; if clause so we can not lose the encrypted plist structure:
         (user-details ;; order changed for new encrypted auth flow:
          (if mastodon-auth-encrypt-tokens-plstore
              '( :client_id "id" :client_secret "secret"
                 :access_token "token"
                 :username "test8000@mastodon.example"
                 :instance "https://mastodon.example")
            '( :username "test8000@mastodon.example"
               :instance "https://mastodon.example"
               :client_id "id"
               :client_secret "secret"
               :access_token "token"))))
    ;; setup plstore: store access token, using auth source:
    (let ((mastodon-auth-use-auth-source t)
          (auth-sources "fixture/auth-source-stub"))
      (with-mock
        (mock (mastodon-client) => '(:client_id "id" :client_secret "secret"))
        (mock (mastodon-client--token-file) => file)
        (mastodon-client--store-access-token "token")
        ;; should nil if we don't check with auth source
        ;; because we saved in auth-source instead:

        ;; FIXME: this fails because we currently DO save access-token in
        ;; plstore even if using auth-source.
        (let ((mastodon-auth-use-auth-source nil))
          (should (equal
                   (mastodon-auth--plstore-access-token-member)
                   nil))))
      (delete-file file))))


(ert-deftest mastodon-auth-auth-source-search ()
  (let* ((mastodon-instance-url "https://mastodon.example")
         (mastodon-active-user "test8000")
         (auth-source-backend 'netrc)
         (host (url-domain
                (url-generic-parse-url mastodon-instance-url)))
         (auth-sources "/home/mouse/code/elisp/mastodon.el/test/fixture/auth-source-stub.gpg")
         (mastodon-auth-use-auth-source t)
         (auth-source-debug t)
         (token "token")
         (creds
          (mastodon-auth-source-get mastodon-active-user host token :create))
         (epa-file-encrypt-to "02348176F1E0FFC3"))
    ;; try to save token to unencrypted auth-source:
    (should (equal 3 ; should return list of user, token, save-fun:
                   (length creds)))
    ;; it does so but does not save to our file, so if we save then fetch,
    ;; we get zilch
    (should
     (not (eq nil (nth 2 creds))))
    ;; ))

    (should
     (equal "token"
            (mastodon-auth-source-token mastodon-instance-url mastodon-active-user
                           token)))
    ))
