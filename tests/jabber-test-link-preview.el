;;; jabber-test-link-preview.el --- Tests for link previews  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'jabber-chat)
(require 'jabber-link-preview)

(defconst jabber-test-link-preview--html
  "<html><head>
<meta property='og:title' content='  A useful page  '>
<meta property='og:description' content='A short description.'>
<meta property='og:site_name' content='Example'>
<meta property='og:image' content='/card.png'>
<meta property='og:image:alt' content='Card alt text'>
<title>Ignored title</title>
</head></html>")

(ert-deftest jabber-test-link-preview-parses-open-graph ()
  (should
   (equal (jabber-link-preview-parse-html
           jabber-test-link-preview--html "https://example.org/news/item")
          '(:url "https://example.org/news/item"
            :site "Example"
            :title "A useful page"
            :description "A short description."
            :image "https://example.org/card.png"
            :image-alt "Card alt text"))))

(ert-deftest jabber-test-link-preview-falls-back-to-html-title ()
  (let ((html "<html><head><title> Plain   title </title>
<meta name='description' content=' Plain description '></head></html>"))
    (should
     (equal (jabber-link-preview-parse-html html "https://example.org/a")
            '(:url "https://example.org/a"
              :site "example.org"
              :title "Plain title"
              :description "Plain description")))))

(ert-deftest jabber-test-link-preview-selects-first-page-url ()
  (should
   (equal (jabber-link-preview-url
           "photo https://example.org/pic.png then https://example.org/page?q=1")
          "https://example.org/page?q=1")))

(ert-deftest jabber-test-link-preview-rejects-http-and-local-hosts ()
  (should-not (jabber-link-preview-url "http://example.org/page"))
  (should-not (jabber-link-preview-url "https://localhost/private"))
  (should-not (jabber-link-preview-url "https://127.0.0.1/private")))

(ert-deftest jabber-test-link-preview-card-remains-readable-without-faces ()
  (let ((card (jabber-link-preview-format
               '(:url "https://example.org/a"
                 :site "Example" :title "Title" :description "Summary"))))
    (should (equal (substring-no-properties card)
                   "\n│ Example\n│ Title\n│ Summary"))
    (should (eq (get-text-property 2 'face card) 'shadow))
    (should (equal (get-text-property 2 'jabber-chat-link-preview-url card)
                   "https://example.org/a"))))

(ert-deftest jabber-test-chat-marks-only-first-preview-url ()
  (with-temp-buffer
    (insert "See https://example.org/one and https://example.org/two")
    (let ((jabber-chat--body-start (point-min)))
      (jabber-chat-mark-link-preview-url
       '(:body "See https://example.org/one and https://example.org/two")
       :foreign :insert))
    (goto-char (point-min))
    (search-forward "https://example.org/one")
    (should (equal (get-text-property (1- (point))
                                      'jabber-chat-link-preview-url)
                   "https://example.org/one"))
    (search-forward "https://example.org/two")
    (should-not (get-text-property (1- (point))
                                   'jabber-chat-link-preview-url))))

(ert-deftest jabber-test-chat-preview-keymap-overrides-goto-address ()
  (with-temp-buffer
    (insert "See https://example.org/one")
    (let ((jabber-chat--body-start (point-min)))
      (jabber-chat-goto-address nil nil :insert)
      (jabber-chat-mark-link-preview-url
       '(:body "See https://example.org/one") :foreign :insert))
    (goto-char (point-min))
    (search-forward "https://example.org/one")
    (backward-char)
    (should (eq (key-binding (kbd "RET"))
                #'jabber-chat-url-action-at-point))))

(ert-deftest jabber-test-link-preview-rejects-mixed-dns-answers ()
  (cl-letf (((symbol-function 'network-lookup-address-info)
             (lambda (&rest _)
               '([93 184 216 34 0] [127 0 0 1 0]))))
    (should-not
     (jabber-link-preview-safe-url-p "https://example.org/a" t))))

(ert-deftest jabber-test-link-preview-rejects-ipv4-mapped-ipv6 ()
  (should-not
   (jabber-link-preview--public-address-p
    [0 0 0 0 0 65535 32512 1 0]))
  (should-not
   (jabber-link-preview--public-address-p
    [0 0 0 0 0 65535 2560 1 0]))
  ;; This transport deliberately pins IPv4 only; mapped public IPv4 is
  ;; rejected rather than expanding the IPv6 trust boundary.
  (should-not
   (jabber-link-preview--public-address-p
    [0 0 0 0 0 65535 23994 55330 0])))

(ert-deftest jabber-test-link-preview-curl-pins-vetted-address ()
  (let ((command (jabber-link-preview--curl-command
                  "https://example.org/a" "93.184.216.34")))
    (should (member "--noproxy" command))
    (should (member "*" command))
    (should (member "--resolve" command))
    (should (member "example.org:443:93.184.216.34" command))
    (should-not (member "--location" command))))

(ert-deftest jabber-test-link-preview-parser-unavailable-is-an-error-result ()
  (cl-letf (((symbol-function 'libxml-parse-html-region) nil))
    (should (equal (jabber-link-preview--parse-result
                    "<title>Page</title>" "https://example.org/")
                   '(:error parser-unavailable)))))

(ert-deftest jabber-test-link-preview-filter-aborts-oversize-response ()
  (let* ((buffer (generate-new-buffer " *jabber-preview-filter-test*"))
         (process (make-process :name "jabber-preview-filter-test"
                                :buffer buffer :command '("cat")
                                :noquery t))
         (jabber-link-preview-max-html-bytes 8))
    (unwind-protect
        (progn
          (jabber-link-preview--process-filter
           process "HTTP/1.1 200 OK\r\n\r\n123456789")
          (should (process-get process 'jabber-link-preview-too-large))
          (with-current-buffer buffer
            (should (<= (buffer-size)
                        jabber-link-preview--response-overhead-bytes))))
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest jabber-test-link-preview-filter-bounds-terminated-huge-header ()
  (let* ((buffer (generate-new-buffer " *jabber-preview-header-test*"))
         (process (make-process :name "jabber-preview-header-test"
                                :buffer buffer :command '("cat")
                                :sentinel #'ignore :noquery t))
         (jabber-link-preview--response-overhead-bytes 32))
    (unwind-protect
        (progn
          (jabber-link-preview--process-filter
           process (concat "HTTP/1.1 200 OK\r\nX-Fill: "
                           (make-string 64 ?x) "\r\n\r\nbody"))
          (should (process-get process 'jabber-link-preview-too-large))
          (with-current-buffer buffer
            (should (<= (buffer-size)
                        jabber-link-preview--response-overhead-bytes))))
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest jabber-test-link-preview-filter-bounds-unterminated-header ()
  (let* ((buffer (generate-new-buffer " *jabber-preview-open-header-test*"))
         (process (make-process :name "jabber-preview-open-header-test"
                                :buffer buffer :command '("cat")
                                :sentinel #'ignore :noquery t))
         (jabber-link-preview--response-overhead-bytes 32))
    (unwind-protect
        (progn
          (jabber-link-preview--process-filter
           process (concat "HTTP/1.1 200 OK\r\nX-Fill: "
                           (make-string 64 ?x)))
          (should (process-get process 'jabber-link-preview-too-large))
          (with-current-buffer buffer
            (should (<= (buffer-size)
                        jabber-link-preview--response-overhead-bytes))))
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest jabber-test-link-preview-sentinel-kills-error-buffer ()
  (let* ((buffer (generate-new-buffer " *jabber-preview-sentinel-test*"))
         (result nil)
         (process (make-process :name "jabber-preview-sentinel-test"
                                :buffer buffer :command '("cat")
                                :coding 'binary :noquery t
                                :sentinel #'jabber-link-preview--process-sentinel)))
    (process-put process 'jabber-link-preview-callback
                 (lambda (value) (setq result value)))
    (process-put process 'jabber-link-preview-cbargs nil)
    (process-put process 'jabber-link-preview-url "https://example.org/")
    (process-send-eof process)
    (while (process-live-p process) (accept-process-output process 0.1))
    (accept-process-output nil 0.1)
    (should (equal result '(:error response)))
    (should-not (buffer-live-p buffer))))

(ert-deftest jabber-test-chat-dead-buffer-completion-clears-loading-state ()
  (let* ((url "https://example.org/a")
         (token (list 'loading (gensym)))
         (buffer (generate-new-buffer " *jabber-preview-dead-test*"))
         (result '(:url "https://example.org/a" :title "Page")))
    (unwind-protect
        (progn
          (jabber-link-preview-put url token)
          (kill-buffer buffer)
          (jabber-chat--handle-link-preview result url token nil buffer)
          (should (equal (jabber-link-preview-get url) result)))
      (remhash url jabber-link-preview--cache)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest jabber-test-chat-stale-preview-completion-cannot-overwrite-newer ()
  (let* ((url "https://example.org/a")
         (old-token (list 'loading 'old))
         (new-token (list 'loading 'new)))
    (unwind-protect
        (progn
          (jabber-link-preview-put url new-token)
          (jabber-chat--handle-link-preview
           '(:url "https://example.org/a" :title "Old")
           url old-token nil nil)
          (should (eq (jabber-link-preview-get url) new-token)))
      (remhash url jabber-link-preview--cache))))

(ert-deftest jabber-test-chat-link-preview-ret-lifecycle ()
  (with-temp-buffer
    (let* ((url "https://example.org/page")
           (msg (list :body url))
           pending
           opened)
      (unwind-protect
          (progn
            (setq-local
             jabber-chat-ewoc
             (ewoc-create
              (lambda (data)
                (let ((message (cadr data))
                      (jabber-chat--body-start (point)))
                  (jabber-chat-print-body message :foreign :insert)
                  (jabber-chat-print-link-preview message :foreign :insert)
                  (jabber-chat-goto-address message :foreign :insert)
                  (jabber-chat-mark-link-preview-url
                   message :foreign :insert)))))
            (jabber-chat-ewoc-enter (list :foreign msg))
            (goto-char (point-min))
            (search-forward url)
            (backward-char)
            (should (equal (get-text-property
                            (point) 'jabber-chat-link-preview-url)
                           url))
            (cl-letf (((symbol-function 'jabber-link-preview-fetch)
                       (lambda (_url callback &rest args)
                         (setq pending (cons callback args)))))
              (jabber-chat-url-action-at-point))
            (should (eq (car (jabber-link-preview-get url)) 'loading))
            (apply (car pending)
                   (list :url url :site "Example" :title "Page")
                   (cdr pending))
            (should (string-match-p "│ Page" (buffer-string)))
            (goto-char (point-min))
            (search-forward url)
            (backward-char)
            (cl-letf (((symbol-function 'browse-url)
                       (lambda (target &rest _) (setq opened target))))
              (jabber-chat-url-action-at-point))
            (should (equal opened url)))
        (remhash url jabber-link-preview--cache)))))

(provide 'jabber-test-link-preview)

;;; jabber-test-link-preview.el ends here