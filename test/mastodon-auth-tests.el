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
    (with-mock
      ;; ensure no actual POST request (works offline):
      (mock (mastodon-client) => '(:client_id "id" :client_secret "secret"))
      (should
       (string=
        "foo"
        (mastodon-auth--handle-token-response
         '(:access_token "foo" :token_type "Bearer" :scope "read write follow" :created_at 0)))))))

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
  "Check that saving token to plstore and fetching works.
Store with `mastodon-client--store-access-token'.
Fetch with `mastodon-auth--plstore-access-token-member'.
We also check that fetching works `mastodon-auth-use-auth-source' is enabled after
saving, but before fetching."
  (let* ((mastodon-instance-url "https://mastodon.example")
         (mastodon-active-user "test8000")
         (mastodon-client--token-file "stubfile.plstore")
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
      (should
       (equal (mastodon-client--store-access-token "token")
              user-details))
      ;; should non-nil if we check with auth-source:
      ;; because we saved with non auth-source:
      (let ((mastodon-auth-use-auth-source t))
        (should
         (equal
          (mastodon-auth--plstore-access-token-member)
          ;; if clause so we can not lose the encrypted plist structure:
          (if mastodon-auth-encrypt-tokens-plstore
              '(:secret-access_token t :username "test8000@mastodon.example"
                                     :instance "https://mastodon.example")
            '(:access_token "token")))))
      (delete-file "stubfile.plstore"))))

(ert-deftest mastodon-auth-plstore-token-check-auth-source ()
  ;; :expected-result :failed
  "Test that, when `mastodon-auth-use-auth-source',
`mastodon-client--store-access-token' does not store a token in
`mastodon-client--token-file'. We call `mastodon-auth--plstore-access-token-member'
to check if the token is present. To ensure we actually save to auth
sources, we create a new file then delete it."
  (let* ((mastodon-instance-url "https://mastodon.example")
         (mastodon-active-user "test8000")
         (mastodon-client--token-file "fixture/stubfile-auth-source.plstore")
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
    (let* ((mastodon-auth-use-auth-source t)
           (auth-source-do-cache nil)
           (auth-sources '("fixture/auth-info-check"))
           (auth-source-save-behavior t) ;; disable prompting
           (file (car auth-sources))
           (filename (nth 1 (split-string file
                                          "/"))))
      (auth-source-forget-all-cached)
      ;; create auth source file:
      (find-file-noselect file)
      ;; save and kill it:
      (with-current-buffer filename
        (save-buffer)
        (kill-buffer filename))
      (with-mock
        (mock (mastodon-client) => '(:client_id "id" :client_secret "secret"))
        (mastodon-client--store-access-token "token")
        ;; should nil if we don't check with auth source
        ;; because we saved in auth-source instead:
        (let ((mastodon-auth-use-auth-source nil))
          (should (equal
                   (mastodon-auth--plstore-access-token-member)
                   nil)))
        ;; NB: if we error in `mastodon-auth-source-get', this won't run:
        (delete-file mastodon-client--token-file)
        (delete-file file)))))

(ert-deftest mastodon-auth-auth-source-search-only ()
  "Test searching an existing auth-source file.
We test that fetching works, result is 3-elt list, with elt 2 a token.
Test also that token is same as fetching it from same file using
`mastodon-auth-source-token'."
  ;; :expected-result :failed
  (let* ((mastodon-instance-url "https://mastodon.example")
         (mastodon-active-user "test8000")
         (host (url-domain
                (url-generic-parse-url mastodon-instance-url)))
         (auth-sources '("fixture/auth-source-search-only"))
         (mastodon-auth-use-auth-source t)
         (auth-source-do-cache nil)
         (token "12341234")
         (creds
          ;; no :create arg,
          ;; fetch from manually added to fixture/auth-source-stub
          (mastodon-auth-source-get mastodon-active-user host token)))
    ;; should return list of user, token, (empty) save-fun:
    (should (equal 3 (length creds)))
    (should
     (not (eq token (nth 1 creds))))
    (should
     (equal token
            ;; check against `mastodon-auth-source-token' too:
            (mastodon-auth-source-token mastodon-instance-url
                           (concat mastodon-active-user "@" host)
                           :token)))))

(ert-deftest mastodon-auth-auth-source-save-check ()
  "Test that we can save an (unencrypted) auth-source entry.
Test that creating a new entry works.
Test that doing so returns a three element list, with elt 2 as token and
elt three is a function."
  (auth-source-forget-all-cached)
  (let* ((mastodon-instance-url "https://mastodon.example")
         (mastodon-active-user "test8000")
         (host (url-domain
                (url-generic-parse-url mastodon-instance-url)))
         (auth-sources '("fixture/auth-source-save-check"))
         (file (car auth-sources))
         (filename (nth 1 (split-string file
                                        "/")))
         (mastodon-auth-use-auth-source t)
         (auth-source-do-cache nil)
         (auth-source-save-behavior t) ;; disable prompting
         (backup-inhibited t)
         (token "12341234"))
    ;; to reliably add an entry to auth-source, we seem to need to create
    ;; an empty file (deleting is unreliable/doesn't work). this way we
    ;; know that auth-source won't withhold an entry's save-function:
    ;; create auth source file:
    (find-file-noselect file)
    ;; save and kill it:
    (with-current-buffer filename
      (save-buffer)
      (kill-buffer filename))
    ;; create entry:
    (let ((result
           (mastodon-auth-source-get mastodon-active-user
                        mastodon-instance-url
                        token :create)))
      ;; should return list of user, token, (non-empty) save-fun:
      (should
       (= 3 (length result)))
      (should
       (equal token (nth 1 result)))
      ;; third elt should be (save) fun, non-nil:
      (should
       (functionp (nth 2 result))))
    (delete-file file)))
