;;; hermes-endpoints-tests.el --- Named endpoint lifecycle tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'hermes)

(ert-deftest hermes-endpoints-public-lifecycle-is-discoverable ()
  "The native setup surface offers a separate named endpoint lifecycle."
  (should (commandp 'hermes-endpoints))
  (dolist (command '(hermes-endpoints-new hermes-endpoints-edit
                     hermes-endpoints-test hermes-endpoints-save
                     hermes-endpoints-activate hermes-endpoints-delete))
    (should (commandp command))))

(defun hermes-endpoints-test--row ()
  "Return a fresh redacted saved row."
  (copy-tree
   '((id . "Mixed endpoint") (name . "Fixture")
     (base_url . "http://127.0.0.1:19001") (model . "fixture-model")
     (source . "providers") (discover_models . :false)
     (api_mode . "codex_responses") (has_api_key . t)
     (api_key_preview . "MASKED-NEVER-REPLAY") (is_current . t))))

(defmacro hermes-endpoints-test--with-view (mode &rest body)
  "Run BODY in an owned MODE, with real browser and REST request builders."
  (declare (indent 1))
  `(let* ((hermes-instances '(("fixture" . "http://127.0.0.1:19000")))
          (hermes-dashboard-transport-url nil)
          (client (make-hermes-dashboard-transport-client
                   :base-url "http://127.0.0.1:19000" :token "fixture-session"))
          (buffer (hermes-buffer--get (generate-new-buffer-name " *endpoint test*") ,mode))
          (reply (list (cons 'endpoints (list (hermes-endpoints-test--row)))
                       (copy-tree '(current . ((provider . "Mixed endpoint") (model . "fixture-model"))))))
          (handler (lambda (_request) (hermes--promise-resolved
                                      (list :status 200 :body reply))))
          requests (released 0))
     (unwind-protect
         (save-window-excursion
           (cl-letf (((symbol-function 'hermes-browser--existing-client) (lambda () nil))
                     ((symbol-function 'hermes-dashboard-transport-acquire)
                      (lambda (&rest _) client))
                     ((symbol-function 'hermes-dashboard-transport-release)
                      (lambda (_) (cl-incf released)))
                     ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                      (lambda (request) (push request requests) (funcall handler request))))
             (with-current-buffer buffer
               (hermes-browser--own-instance (car hermes-instances))
               (setq hermes-endpoints--profile "fixture-profile"
                     hermes-endpoints--snapshot reply
                     hermes-endpoints--draft
                     (hermes-endpoints--draft-from-row (hermes-endpoints-test--row)))
               ,@body)))
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest hermes-endpoints-save-preserves-dto-semantics-and-reads-back ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (hermes-endpoints-save))
    (should (= 2 (length requests)))
    (let* ((write (cadr requests))
           (body (plist-get write :body))
           (wire (json-serialize body :false-object :false :null-object nil)))
      (should (string-match-p "\"make_default\":false" wire))
      (should (eq :false (alist-get 'discover_models body)))
      (dolist (field '(api_key key_env api_mode context_length models model_details))
        (should-not (assq field body)))
      (should (equal "Mixed endpoint" (alist-get 'id body))))
    (dolist (request requests)
      (should (equal (plist-get request :url)
                     "http://127.0.0.1:19000/api/providers/custom-endpoints?profile=fixture-profile")))
    (should (equal (plist-get (car requests) :method) "GET"))
    (should (= released 1))
    (should-not (string-match-p "MASKED" (buffer-string)))
    (should (string-match-p "Active model config: Mixed endpoint" (buffer-string)))))

(ert-deftest hermes-endpoints-secret-preserve-replace-clear ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
    (let ((choice "Replace") (minibuffer-history nil))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) choice))
                ((symbol-function 'read-passwd) (lambda (&rest _) "synthetic-private-key"))
                ((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (hermes-endpoints-key)
        (should (equal "synthetic-private-key" (alist-get 'api_key hermes-endpoints--draft)))
        (should-not (string-match-p "synthetic-private-key" (buffer-string)))
        (should-not minibuffer-history)
        (hermes-endpoints-save)
        (should (member "synthetic-private-key" (plist-get (cadr requests) :secrets)))
        (should-not (assq 'api_key hermes-endpoints--draft))
        (setq choice "Clear" requests nil)
        (hermes-endpoints-key)
        (hermes-endpoints-save)
        (should (equal "" (alist-get 'api_key (plist-get (cadr requests) :body))))
        (setq choice "Preserve" requests nil)
        (hermes-endpoints-key)
        (hermes-endpoints-save)
        (should-not (assq 'api_key (plist-get (cadr requests) :body)))))))

(ert-deftest hermes-endpoints-blank-replace-retains-key-policy ()
  "Refuse persistence-empty keys before changing any retained draft state."
  (dolist (key (append '("" "\u00a0\u0085\u1680\u2003\u3000"
                        "\u200b" "\ufeff" "\u180e" "\u200b\ufeff\u180e"
                        "λ" "漢字" "😀" "\u0301" "\U0010ffff"
                        "\u200b\n\r\ufeff" " \u200b\r\nλ\t"
                        "\u0085\u200b\n\r\u3000")
                       (mapcar #'char-to-string
                               '(#x9 #xa #xb #xc #xd #x1c #x1d #x1e #x1f #x20
                                 #x85 #xa0 #x1680 #x2000 #x2001 #x2002 #x2003
                                 #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200a
                                 #x2028 #x2029 #x202f #x205f #x3000))))
    (dolist (prior '(nil "synthetic-pending-key" ""))
      (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
        (when prior (push (cons 'api_key prior) hermes-endpoints--draft))
        (setq hermes-endpoints--probe '((ok . t)))
        (hermes-endpoints--render-draft)
        (let ((before (copy-tree hermes-endpoints--draft))
              (draft hermes-endpoints--draft)
              (probe hermes-endpoints--probe)
              (text (buffer-string)) entered)
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Replace"))
                    ((symbol-function 'read-passwd)
                     (lambda (&rest _) (setq entered t) key)))
            (should-error (call-interactively #'hermes-endpoints-key) :type 'user-error))
          (should entered)
          (should (eq draft hermes-endpoints--draft))
          (should (equal before hermes-endpoints--draft))
          (should (eq probe hermes-endpoints--probe))
          (should (equal text (buffer-string)))
          (should-not requests)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (hermes-endpoints-save))
          (let ((body (json-parse-string
                       (json-serialize (plist-get (cadr requests) :body)
                                       :false-object :false :null-object nil)
                       :object-type 'alist)))
            (should (equal (assq 'api_key body) (and prior (cons 'api_key prior))))))))))

(ert-deftest hermes-endpoints-nonblank-replace-is-literal ()
  "Retain admitted keys literally, without claiming literal remote storage."
  (dolist (key '("synthetic-key" "\u0000" "\u007f" "\u0001"
                "\u200b \ufeff" "λ\t漢" "λ\u001c漢"
                "a\u200b" "\ufeffb" "λc😀" "a\r\nb"
                "\u00a0synthetic-key\u3000" "a\u0085b"))
    (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Replace"))
                ((symbol-function 'read-passwd) (lambda (&rest _) key))
                ((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (hermes-endpoints-key)
        (should (equal key (alist-get 'api_key hermes-endpoints--draft)))
        (hermes-endpoints-save))
      (let ((body (json-parse-string
                   (json-serialize (plist-get (cadr requests) :body)
                                   :false-object :false :null-object nil)
                   :object-type 'alist)))
        (should (equal key (alist-get 'api_key body)))))))

(ert-deftest hermes-endpoints-old-empty-replacement-cannot-save-or-probe ()
  "Refuse previously admitted destructive drafts before consent or effects."
  (dolist (command '(hermes-endpoints-save hermes-endpoints-test))
    (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
      (push '(api_key . "\u200b\n\ufeff") hermes-endpoints--draft)
      (setq hermes-endpoints--probe '((ok . t)))
      (hermes-endpoints--render-draft)
      (let ((draft hermes-endpoints--draft)
            (before (copy-tree hermes-endpoints--draft))
            (probe hermes-endpoints--probe)
            (text (buffer-string)) prompted)
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (_) (setq prompted t) t)))
          (should-error (call-interactively command) :type 'user-error))
        (should-not prompted)
        (should-not requests)
        (should (eq draft hermes-endpoints--draft))
        (should (equal before hermes-endpoints--draft))
        (should (eq probe hermes-endpoints--probe))
        (should (equal text (buffer-string)))))))

(ert-deftest hermes-endpoints-test-consent-and-observations ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
    (let (consent)
      (setq reply '((ok . t) (reachable . t) (models . ("fixture-model"))
                    (transport_checked . "codex_responses")
                    (resolved_base_url . "http://127.0.0.1:19001/v1")))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt) (setq consent prompt) nil)))
        (hermes-endpoints-test))
      (should-not requests)
      (dolist (text '("19001" "credentials" "/v1" "paid inference" "16" "No save"))
        (should (string-match-p (regexp-quote text) consent)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (hermes-endpoints-test))
      (should (= 1 (length requests)))
      (should (equal "codex_responses" (alist-get 'api_mode (plist-get (car requests) :body))))
      (should (string-match-p "/validate?" (plist-get (car requests) :url)))
      (should (equal "http://127.0.0.1:19001" (alist-get 'base_url hermes-endpoints--draft)))
      (should (string-match-p "401/429" (buffer-string)))
      (should (string-match-p "inconclusive" (buffer-string)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (hermes-endpoints-adopt-probe))
      (should (equal "http://127.0.0.1:19001/v1" (alist-get 'base_url hermes-endpoints--draft)))
      (should (= 1 (length requests))))))

(ert-deftest hermes-endpoints-saved-actions-exact-id-and-profile ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-list-mode
    (setf (alist-get 'id (car (alist-get 'endpoints reply))) "fixture_endpoint")
    (hermes-endpoints--accept-list reply)
    (goto-char (point-min))
    (let (prompts)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (text) (push text prompts) t)))
        (hermes-endpoints-activate)
        (hermes-endpoints-delete))
      (should (= 4 (length requests)))
      (should (string-match-p (regexp-quote "fixture_endpoint/activate?profile=fixture-profile")
                              (plist-get (nth 3 requests) :url)))
      (should (equal "DELETE" (plist-get (nth 1 requests) :method)))
      (should (string-match-p "detach provider/URL/key" (car prompts)))
      (should (string-match-p "fixture-profile" (car prompts))))))

(ert-deftest hermes-endpoints-recursive-consent-retirement-refuses-write ()
  (dolist (retire '(profile backend file))
    (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_)
                   (pcase retire
                     ('profile (setq hermes-endpoints--profile "other"))
                     ('backend (setq hermes-instance '("other" . "http://127.0.0.1:19002")))
                     ('file (setq buffer-file-name "/fixture-notes")
                            (run-hooks 'after-set-visited-file-name-hook)
                            (setq buffer-file-name nil)))
                   t)))
        (hermes-endpoints-save))
      (should-not requests)
      (should-not hermes-browser--owned-cleanup))))

(ert-deftest hermes-endpoints-auth-wait-cancellation-refuses-dispatch ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
    (let ((auth (hermes--promise-make)))
      (setf (hermes-dashboard-transport-client-token client) nil)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                 (lambda (&optional _) auth)))
        (hermes-endpoints-save)
        (should hermes-browser--owned-cleanup)
        (hermes-browser--next-request-generation)
        (hermes--promise-resolve auth '(:base-url "http://127.0.0.1:19000")))
      (should-not requests)
      (should (= 1 released))
      (should-not hermes-browser--owned-cleanup))))

(ert-deftest hermes-endpoints-error-retains-draft-without-secret-diagnostics ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
    (progn
      (setq handler (lambda (_) (hermes--promise-rejected "synthetic-secret timeout")))
      (setf (alist-get 'api_key hermes-endpoints--draft) "synthetic-secret")
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (hermes-endpoints-save))
      (should (= 1 (length requests)))
      (should (equal "synthetic-secret" (alist-get 'api_key hermes-endpoints--draft)))
      (should-not (string-match-p "synthetic-secret" hermes-browser--status))
      (should-not hermes-browser--owned-cleanup))))

(ert-deftest hermes-endpoints-stale-readback-does-not-repaint-successor ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
    (let ((pending (hermes--promise-make)))
      (setq handler (lambda (request)
                      (if (equal (plist-get request :method) "GET") pending
                        (hermes--promise-resolved '(:status 200 :body ((ok . t)))))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))) (hermes-endpoints-save))
      (should (= 2 (length requests)))
      (setq hermes-endpoints--profile "successor")
      (let ((inhibit-read-only t)) (erase-buffer) (insert "Successor draft"))
      (hermes--promise-resolve pending (list :status 200 :body reply))
      (should (equal "Successor draft" (buffer-string)))
      (should (= 1 released)))))

(ert-deftest hermes-endpoints-public-open-retry-and-new-draft ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-list-mode
    (let ((origin (generate-new-buffer " *endpoint origin*")) view draft)
      (unwind-protect
          (progn
            (setq handler (lambda (_) (hermes--promise-rejected "offline")))
            (with-current-buffer origin (hermes-endpoints))
            (setq view (window-buffer (selected-window)))
            (set-buffer view)
            (should (derived-mode-p 'hermes-endpoint-list-mode))
            (should (string-match-p "Failed" hermes-browser--status))
            (should-not hermes-endpoints--profile)
            (setq handler (lambda (_) (hermes--promise-resolved (list :status 200 :body reply))))
            (execute-kbd-macro (kbd "g"))
            (should (string-match-p "Active provider:" hermes-browser--status))
            (execute-kbd-macro (kbd "c"))
            (setq draft (current-buffer))
            (should (derived-mode-p 'hermes-endpoint-edit-mode))
            (should (equal "" (alist-get 'id hermes-endpoints--draft)))
            (should-not (assq 'api_key hermes-endpoints--draft)))
        (dolist (owned (list origin view draft))
          (when (buffer-live-p owned) (kill-buffer owned)))))))

(ert-deftest hermes-endpoints-probe-redaction-survives-key-forgetting ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
    (setf (alist-get 'api_key hermes-endpoints--draft) "synthetic-echo-key")
    (setq reply '((ok . :false) (reachable . t) (message . "echo synthetic-echo-key")
                  (models . ("echo synthetic-echo-key"))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))) (hermes-endpoints-test))
    (setq hermes-endpoints--draft (assq-delete-all 'api_key hermes-endpoints--draft))
    (hermes-endpoints--render-draft)
    (should-not (string-match-p "synthetic-echo-key" (buffer-string)))
    (should-not (string-match-p "synthetic-echo-key" (prin1-to-string hermes-endpoints--probe)))))

(ert-deftest hermes-endpoints-secret-reader-quit-and-retirement ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
    (let ((before (copy-tree hermes-endpoints--draft)) entered)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Replace"))
                ((symbol-function 'read-passwd)
                 (lambda (&rest _) (setq entered t) (signal 'quit nil))))
        (condition-case nil (hermes-endpoints-key) (quit nil)))
      (should entered)
      (should (equal before hermes-endpoints--draft))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Replace"))
                ((symbol-function 'read-passwd)
                 (lambda (&rest _)
                   (setq hermes-endpoints--profile "successor") "do-not-store")))
        (hermes-endpoints-key))
      (should (equal before hermes-endpoints--draft))
      (should-not requests))))

(ert-deftest hermes-endpoints-unsafe-backend-key-refuses-activation-and-delete ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-list-mode
    (hermes-endpoints--accept-list reply)
    (goto-char (point-min))
    (let (prompted)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (setq prompted t) t)))
        (should-error (hermes-endpoints-activate) :type 'user-error)
        (should-error (hermes-endpoints-delete) :type 'user-error))
      (should-not prompted)
      (should-not requests))))

(ert-deftest hermes-endpoints-profile-switch-failure-cannot-lend-old-rows ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-list-mode
    (let ((origin (generate-new-buffer " *endpoint profile origin*")) view)
      (unwind-protect
          (progn
            (with-current-buffer origin
              (setq-local major-mode 'hermes-chat-mode hermes-chat--profile "first")
              (hermes-endpoints))
            (setq view (window-buffer (selected-window)))
            (with-current-buffer view
              (should hermes-endpoints--snapshot)
              (should tabulated-list-entries))
            (setq handler (lambda (_) (hermes--promise-rejected "offline")))
            (with-current-buffer origin
              (setq hermes-chat--profile "second")
              (hermes-endpoints))
            (with-current-buffer view
              (should (equal "second" hermes-endpoints--profile))
              (should-not hermes-endpoints--snapshot)
              (should-not tabulated-list-entries)
              (should-error (hermes-endpoints-edit) :type 'user-error)))
        (dolist (owned (list origin view))
          (when (buffer-live-p owned) (kill-buffer owned)))))))

(ert-deftest hermes-endpoints-new-save-retains-normalized-id-for-later-edits ()
  (hermes-endpoints-test--with-view #'hermes-endpoint-edit-mode
    (setf (alist-get 'id hermes-endpoints--draft) "")
    (setq handler (lambda (request)
                    (hermes--promise-resolved
                     (list :status 200 :body
                           (if (equal "POST" (plist-get request :method))
                               '((ok . t) (id . "normalized-id")) reply)))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (hermes-endpoints-save)
      (should (equal "normalized-id" (alist-get 'id hermes-endpoints--draft)))
      (setf (alist-get 'name hermes-endpoints--draft) "Renamed")
      (hermes-endpoints-save))
    (should (= 4 (length requests)))
    (should (equal "normalized-id" (alist-get 'id (plist-get (nth 1 requests) :body))))))

(ert-deftest hermes-endpoints-literal-saved-ids-roundtrip-through-auth ()
  "Admit released IDs without slugging the confirmation or wire target."
  (dolist (id '("has.dot" "-edge" "edge_" "UPPER" "local-127.0.0.1:8283"
                "Key" "ſafe" "İD" "Ελληνικά" "-" "_" "a--b" "a__b"
                "has?query" "has#fragment" "has%percent" "has%20escape" "has\\backslash"
                "has\ttab" "has\u00a0space" "has\u2003space" "\u200bedge"))
    (hermes-endpoints-test--with-view #'hermes-endpoint-list-mode
      (setf (alist-get 'id (car (alist-get 'endpoints reply))) id)
      (hermes-endpoints--accept-list reply)
      (switch-to-buffer buffer)
      (goto-char (point-min))
      (let ((before (copy-tree hermes-endpoints--draft)) prompts)
        (setq handler
              (lambda (request)
                (pcase (plist-get request :method)
                  ("POST"
                   (setf (alist-get 'is_current (car (alist-get 'endpoints reply))) t
                         (alist-get 'current reply) `((provider . ,id) (model . "activated"))))
                  ("DELETE"
                   (setq reply '((endpoints) (current . ((provider . "") (model . "retained")))))))
                (hermes--promise-resolved
                 (list :status 200 :body
                       (if (equal (plist-get request :method) "GET") reply '((ok . t)))))))
        (dolist (key '("a" "d"))
          (let ((auth (hermes--promise-make)))
            (setf (hermes-dashboard-transport-client-token client) nil)
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (text) (push text prompts) t))
                      ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                       (lambda (&optional _) auth)))
              (execute-kbd-macro key)
              (should (= (length requests) (if (equal key "a") 0 2)))
              (hermes--promise-resolve auth '(:base-url "http://127.0.0.1:19000")))
            (should-not hermes-browser--owned-cleanup)
            (should (equal reply hermes-endpoints--snapshot))
            (if (equal key "a")
                (progn
                  (should (equal id (alist-get 'provider (alist-get 'current hermes-endpoints--snapshot))))
                  (should (equal "Active" (aref (cadar tabulated-list-entries) 1))))
              (should-not tabulated-list-entries)
              (should (string-match-p "model: retained" hermes-browser--status)))))
        (should (= 4 (length requests)))
        (should (= 2 released))
        (should (equal before hermes-endpoints--draft))
        (dolist (prompt prompts)
          (should (string-match-p (regexp-quote (concat "[" id "]")) prompt))
          (should (string-match-p "fixture / fixture-profile" prompt)))
        (let* ((base "http://127.0.0.1:19000/api/providers/custom-endpoints")
               (suffix "?profile=fixture-profile")
               (encoded (concat base "/" (url-hexify-string id))))
          (should (equal (mapcar (lambda (r) (list (plist-get r :method)
                                                  (plist-get r :url)))
                                (reverse requests))
                         (list (list "POST" (concat encoded "/activate" suffix))
                               (list "GET" (concat base suffix))
                               (list "DELETE" (concat encoded suffix))
                               (list "GET" (concat base suffix))))))
        (should (equal reply hermes-endpoints--snapshot))))))

(ert-deftest hermes-endpoints-unsafe-literal-ids-never-prompt-or-dispatch ()
  "Refuse released normalization and addressing hazards without repair."
  (dolist (id '("Mixed endpoint" "custom:foo" "CUSTOM:foo" "custom:custom:foo"
                "has/slash" "." ".." " edge" "edge "
                "\tedge" "edge\n" "\u0085edge" "edge\u1680" "\u00a0edge"
                "edge\u2003" "\u2028edge" "edge\u2029" "\u001cedge" ""))
    (hermes-endpoints-test--with-view #'hermes-endpoint-list-mode
      (setf (alist-get 'id (car (alist-get 'endpoints reply))) id)
      (hermes-endpoints--accept-list reply)
      (goto-char (point-min))
      (let ((before (copy-tree hermes-endpoints--draft)) prompted authenticated)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (setq prompted t) t))
                  ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                   (lambda (&optional _) (setq authenticated t))))
          (should-error (hermes-endpoints-activate) :type 'user-error)
          (should-error (hermes-endpoints-delete) :type 'user-error))
        (should-not prompted)
        (should-not authenticated)
        (should-not requests)
        (should (equal before hermes-endpoints--draft))))))

(ert-deftest hermes-endpoints-literal-id-retirement-during-consent-or-auth ()
  "Newly admitted IDs keep the same ownership fences at both waits."
  (dolist (command '(hermes-endpoints-activate hermes-endpoints-delete))
    (dolist (wait '(consent auth))
      (dolist (retire '(profile backend generation))
        (hermes-endpoints-test--with-view #'hermes-endpoint-list-mode
          (setf (alist-get 'id (car (alist-get 'endpoints reply))) "has.dot")
          (hermes-endpoints--accept-list reply)
          (goto-char (point-min))
          (let ((auth (hermes--promise-make)) entered
                (before (copy-tree hermes-endpoints--draft)))
            (setf (hermes-dashboard-transport-client-token client) nil)
            (cl-labels ((retire-owner ()
                         (pcase retire
                           ('profile (setq hermes-endpoints--profile "successor"))
                           ('backend (setq hermes-instance
                                           '("other" . "http://127.0.0.1:19002")))
                           ('generation (hermes-browser--next-request-generation)))))
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (_) (setq entered t)
                           (when (eq wait 'consent) (retire-owner)) t))
                        ((symbol-function 'hermes-dashboard-transport-api-auth-async)
                         (lambda (&optional _) auth)))
                (funcall command)
                (when (eq wait 'auth) (retire-owner))
                (hermes--promise-resolve auth '(:base-url "http://127.0.0.1:19000"))))
            (should entered)
            (should-not requests)
            (should (equal before hermes-endpoints--draft))
            (should-not hermes-browser--owned-cleanup)))))))

(provide 'hermes-endpoints-tests)
;;; hermes-endpoints-tests.el ends here
