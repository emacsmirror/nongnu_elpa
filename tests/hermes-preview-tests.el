;;; hermes-preview-tests.el --- Generated output preview tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'hermes-test-helpers)
(require 'hermes-preview)

(defmacro hermes-preview-test--with-api (&rest body)
  "Run BODY with captured asynchronous API requests and owned test buffers."
  (declare (indent 0) (debug t))
  `(let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
         (network-attempts 0) calls buffers)
     (save-window-excursion
       (cl-letf (((symbol-function 'url-retrieve)
                  (lambda (&rest _) (cl-incf network-attempts) (error "No network")))
                 ((symbol-function 'url-retrieve-synchronously)
                  (lambda (&rest _) (cl-incf network-attempts) (error "No network")))
                 ((symbol-function 'hermes-dashboard-transport-api-request-async)
                  (lambda (method route &rest args)
                    (let ((promise (hermes--promise-make)))
                      (push (list method route args promise) calls)
                      promise)))
                 ((symbol-function 'hermes-dashboard-transport-release)
                  (lambda (client)
                    (cl-decf (hermes-dashboard-transport-client-refcount client)))))
         (unwind-protect (progn ,@body (should (= network-attempts 0)))
           (dolist (buffer buffers)
             (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(defun hermes-preview-test--chat (name)
  "Create a chat owning a distinct client and session named NAME."
  (let ((buffer (generate-new-buffer " *preview chat*")))
    (with-current-buffer buffer
      (hermes-chat-mode)
      (setq-local hermes-instance (cons name (concat "http://" name ".invalid")))
      (setq hermes-chat--dashboard-client
            (make-hermes-dashboard-transport-client
             :base-url (cdr hermes-instance) :refcount 1)
            hermes-chat--dashboard-active-session-id name))
    buffer))

(defun hermes-preview-test--entry (path)
  "Return a normalized image generation tool entry publishing PATH."
  (let* ((event (car (hermes-dashboard-transport--tool-complete-events
                     "tool.complete" nil
                     `((tool_id . "call") (name . "image_generate")
                       (result . ((success . t) (image . ,path))))))))
    (list :id "tool:call" :role 'tool :status 'done :content "Generated output"
          :metadata (hermes-chat--transport-entry-metadata "assistant" event))))

(defun hermes-preview-test--open (chat path)
  "Render a generated PATH in CHAT and activate its real Preview button."
  (with-current-buffer chat
    (hermes-chat--insert-entry (hermes-preview-test--entry path))
    (hermes-test--push-button-labeled "Preview"))
  (window-buffer (selected-window)))

(defun hermes-preview-test--response (path bytes)
  "Return a managed read response for PATH containing exact BYTES."
  `((path . ,path) (size . ,(length bytes))
    (data_url . ,(concat "data:application/octet-stream;base64,"
                        (base64-encode-string bytes t)))))

(ert-deftest hermes-preview-generated-two-owners-and-no-redraw-io ()
  (hermes-preview-test--with-api
    (let* ((first (hermes-preview-test--chat "one"))
           (second (hermes-preview-test--chat "two")))
      (setq buffers (list first second))
      (with-current-buffer first
        (let ((node (hermes-chat--insert-entry (hermes-preview-test--entry "/output.png"))))
          (ewoc-invalidate hermes-chat--ewoc node)
          (should-not calls)))
      (with-current-buffer first (hermes-test--push-button-labeled "Preview"))
      (let* ((view-one (window-buffer (selected-window)))
             (request-one (car calls))
             (view-two (hermes-preview-test--open second "/output.png"))
             (request-two (car calls)))
        (setq buffers (append (list view-one view-two) buffers))
        (should-not (eq view-one view-two))
        (should (eq (plist-get (nth 2 request-one) :client)
                    (buffer-local-value 'hermes-chat--dashboard-client first)))
        (should (eq (plist-get (nth 2 request-two) :client)
                    (buffer-local-value 'hermes-chat--dashboard-client second)))
        (should (equal (cadr request-one) "/api/files/read"))
        (should (equal (plist-get (nth 2 request-one) :query) '((path . "/output.png"))))
        (pop-to-buffer first)
        (goto-char (point-max))
        (insert "draft")
        (let ((window (selected-window)) (point (point)) (undo buffer-undo-list))
          (hermes--promise-resolve (nth 3 request-two)
                                  (hermes-preview-test--response "/output.png" "two"))
          (hermes--promise-resolve (nth 3 request-one)
                                  (hermes-preview-test--response "/output.png" "one"))
          (should (eq window (selected-window)))
          (should (= point (point)))
          (should (eq undo buffer-undo-list))
          (should (equal (buffer-substring-no-properties hermes-chat--input-marker (point-max)) "draft")))
        (should (equal (buffer-local-value 'hermes-files--bytes view-one) "one"))
        (should (equal (buffer-local-value 'hermes-files--bytes view-two) "two"))))))

(ert-deftest hermes-preview-cancel-retry-discards-old-success-and-rejection ()
  (hermes-preview-test--with-api
    (let* ((chat (hermes-preview-test--chat "cancel"))
           (viewer (hermes-preview-test--open chat "/file"))
           (old (nth 3 (car calls))))
      (setq buffers (list viewer chat))
      (with-current-buffer viewer
        (call-interactively (key-binding (kbd "C-g")))
        (should (string-match-p "Cancelled" header-line-format))
        (call-interactively (key-binding (kbd "g")))
        (should (equal hermes-files--status "Loading"))
        (hermes--promise-resolve old (hermes-preview-test--response "/file" "old"))
        (should-not hermes-files--bytes)
        (hermes--promise-reject (nth 3 (car calls)) 'unavailable)
        (should (string-match-p "Read failed" header-line-format))
        (should-not hermes-preview--cleanup)
        (should (= 1 (hermes-dashboard-transport-client-refcount
                      (nth 3 hermes-preview--owner))))))))

(ert-deftest hermes-preview-retirement-settles-pending-view ()
  (dolist (retirement '(invalidate stop kill mode session))
    (hermes-preview-test--with-api
      (let* ((chat (hermes-preview-test--chat "retire"))
             (viewer (hermes-preview-test--open chat "/file"))
             (promise (nth 3 (car calls))))
        (setq buffers (list viewer chat))
        (with-current-buffer chat
          (pcase retirement
            ('invalidate (hermes-chat--invalidate-transport-state))
            ('stop (hermes-dashboard-transport-stop hermes-chat--dashboard-client))
            ('kill (kill-buffer chat))
            ('mode (fundamental-mode))
            ('session (setq hermes-chat--dashboard-active-session-id "replacement"))))
        (unless (eq retirement 'session)
          (should (string-match-p "Cancelled"
                                  (buffer-local-value 'header-line-format viewer))))
        (hermes--promise-resolve promise (hermes-preview-test--response "/file" "stale"))
        (with-current-buffer viewer
          (should-not hermes-files--bytes)
          (should-not hermes-preview--cleanup)
          (should (string-match-p "Cancelled" header-line-format)))))))

(ert-deftest hermes-preview-exact-byte-save-and-native-image-path ()
  (hermes-preview-test--with-api
    (let* ((chat (hermes-preview-test--chat "bytes"))
           (viewer (hermes-preview-test--open chat "/image.png"))
           (bytes (concat (unibyte-string 137 80 78 71 13 10 26 10 0 255) "literal"))
           (directory (make-temp-file "preview-save-" t))
           (filename (expand-file-name "output" directory)) image-bytes)
      (setq buffers (list viewer chat))
      (unwind-protect
          (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
                   ((symbol-function 'image-type-available-p) (lambda (_) t))
                   ((symbol-function 'create-image)
                    (lambda (data kind data-p &rest _)
                      (should (eq kind 'png)) (should data-p)
                      (setq image-bytes data) '(image :type png :data "test")))
                   ((symbol-function 'insert-image) (lambda (&rest _) (insert "IMAGE"))))
            (hermes--promise-resolve (nth 3 (car calls))
                                    (hermes-preview-test--response "/image.png" bytes))
            (with-current-buffer viewer
              (should (equal image-bytes bytes))
              (should (equal (buffer-string) "IMAGE"))
              (hermes-file-save filename)
              (should-error (hermes-file-save filename) :type 'user-error))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally filename)
              (should (equal (buffer-string) bytes))))
        (delete-directory directory t)))))

(ert-deftest hermes-preview-html-inert-source-retained-and-no-network ()
  (skip-unless (fboundp 'libxml-parse-html-region))
  (hermes-preview-test--with-api
    (let* ((chat (hermes-preview-test--chat "html"))
           (source "<!doctype html><html><body><h1>Hello</h1><script>BAD()</script><img src='https://invalid/a'><iframe src='file:///etc/passwd'>BAD</iframe><a href='javascript:BAD()'>Read me</a><p onclick='BAD()'>Safe text</p></body></html>\n"))
      (setq buffers (list chat))
      (with-current-buffer chat
        (hermes-chat--insert-entry
         (list :id "answer" :role 'assistant :status 'done
               :content (concat "Ordinary prose\n```html\n" source "```\nAfter")))
        (should (string-match-p "Ordinary prose" (buffer-string)))
        (hermes-test--push-button-labeled "Preview"))
      (let ((viewer (window-buffer (selected-window))))
        (push viewer buffers)
        (with-current-buffer viewer
          (should-not calls)
          (should (string-match-p "Hello" (buffer-string)))
          (should (string-match-p "Safe text" (buffer-string)))
          (should-not (string-match-p "BAD\\|https:\\|javascript:\\|passwd" (buffer-string)))
          (should-not (text-property-not-all (point-min) (point-max) 'shr-url nil))
          (should (equal hermes-files--bytes (encode-coding-string source 'utf-8-unix)))
          (call-interactively (key-binding (kbd "v")))
          (should (equal (buffer-string) source))
          (should (eq (key-binding (kbd "n")) #'next-line))
          (should (eq (key-binding (kbd "p")) #'previous-line)))))))

(ert-deftest hermes-preview-svg-rejects-active-content-before-decoder ()
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (let ((safe "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='40'><rect x='1' y='1' width='20' height='20' fill='red' stroke-linecap='round' stroke-linejoin='round'/><text x='2' y='20' font-family='sans-serif' font-weight='bold' font-style='italic'>Label</text></svg>"))
    (dolist (source (list safe
                          "<svg><script>alert(1)</script></svg>"
                          "<svg><image href='file:///secret'/></svg>"
                          "<svg><rect fill='url(https://invalid/x)'/></svg>"
                          "<!DOCTYPE svg [<!ENTITY x SYSTEM 'file:///secret'>]><svg>&x;</svg>"
                          "<svg onload='BAD()'><rect/></svg>"))
      (with-temp-buffer
        (let (decoded)
          (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
                    ((symbol-function 'image-type-available-p) (lambda (_) t))
                    ((symbol-function 'create-image)
                     (lambda (text &rest _) (setq decoded text) '(image :type svg)))
                    ((symbol-function 'insert-image) (lambda (&rest _) (insert "SVG"))))
            (should (eq (not (null (hermes-preview--render-markup 'svg source)))
                        (equal source safe)))
            (should (equal decoded (and (equal source safe) safe)))))))))

(ert-deftest hermes-preview-detection-is-explicit-and-fence-aware ()
  (should-not (hermes-preview--paths
               '(:name "read_file" :status "completed" :args (:path "/input")
                 :result (:path "/input"))))
  (should-not (hermes-preview--paths
               '(:name "write_file" :status "completed"
                 :result (:error "denied" :resolved_path "/refused"))))
  (should (equal (hermes-preview--paths
                  '(:name "write_file" :status "completed"
                    :result "{\"bytes_written\":1,\"resolved_path\":\"/exact.\"}"))
                 '("/exact.")))
  (should (equal (hermes-preview--paths
                  '(:name "text_to_speech" :status "completed"
                    :result (:file_paths ["/one" "/two"] :file_path "/one")))
                 '("/one" "/two")))
  (should-not (hermes-preview--fences "```html\n<p>unfinished</p>"))
  (should-not (hermes-preview--fences "````text\n```html\n<p>example</p>\n```\n````"))
  (should (= 1 (length (hermes-preview--fences "~~~html\n<p>Closed</p>\n~~~"))))
  (should-not (hermes-preview-entry '(:role assistant :status streaming
                                     :content "```html\n<p>Partial</p>\n```"))))

(ert-deftest hermes-preview-prose-diffs-and-popup-dispatch ()
  (hermes-preview-test--with-api
    (let ((chat (hermes-preview-test--chat "popup")))
      (setq buffers (list chat))
      (with-current-buffer chat
        (hermes-chat--insert-entry
         '(:id "answer" :role assistant :status done
           :content "- prose\n+ prose\n```diff\n--- a/file\n+++ b/file\n@@ -1 +1 @@\n-old\n+new\n```\n```html\n<p>Hi</p>\n```"))
        (should (= 1 (hermes-test--count-buttons-labeled "View Diff")))
        (should (= 1 (hermes-test--count-buttons-labeled "Preview")))
        (should (string-match-p (regexp-quote "- prose\n+ prose") (buffer-string)))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _) (caar choices))))
          (call-interactively (lookup-key hermes-chat-work-map (kbd "o")))))
      (push (window-buffer (selected-window)) buffers)
      (should (eq (buffer-local-value 'major-mode (car buffers)) 'hermes-preview-mode))
      (should-not calls))))

(ert-deftest hermes-preview-remote-url-honest-fallback ()
  (hermes-preview-test--with-api
    (let* ((chat (hermes-preview-test--chat "url"))
           (url "https://example.invalid/output.png")
           (viewer (hermes-preview-test--open chat url)))
      (setq buffers (list viewer chat))
      (with-current-buffer viewer
        (should-not calls)
        (should-not hermes-files--bytes)
        (should (string-match-p "never fetched" (buffer-string)))
        (should-error (hermes-preview-retry) :type 'user-error)
        (should-not calls)
        (let ((kill-ring nil))
          (call-interactively (key-binding (kbd "y")))
          (should (equal (car kill-ring) url)))))))

(ert-deftest hermes-preview-unselected-redraw-preserves-draft-and-undo ()
  (hermes-preview-test--with-api
    (let ((chat (hermes-preview-test--chat "unselected"))
          (other (generate-new-buffer " *preview other*")))
      (setq buffers (list chat other))
      (switch-to-buffer other)
      (let ((chat-window (split-window-right)))
        (set-window-buffer chat-window chat)
        (with-current-buffer chat
          (goto-char (point-max))
          (buffer-enable-undo)
          (insert "unsent draft")
          (set-window-point chat-window (point))
          (let ((point (copy-marker (point)))
                (undo buffer-undo-list)
                (window (selected-window)))
            (hermes-chat--insert-entry
             '(:id "markup" :role assistant :status done
               :content "```html\n<h1>Generated</h1>\n```"))
            (hermes-chat--update-entry "markup" #'identity)
            (should-not calls)
            (should (eq window (selected-window)))
            (should (= point (point)))
            (should (= (window-point chat-window) point))
            (should (eq undo buffer-undo-list))
            (should (equal (buffer-substring-no-properties
                            hermes-chat--input-marker (point-max)) "unsent draft"))
            (let ((inhibit-read-only t))
              (primitive-undo 1 buffer-undo-list))
            (should (equal (buffer-substring-no-properties
                            hermes-chat--input-marker (point-max)) ""))))))))

(ert-deftest hermes-preview-rejection-validation-and-recovery ()
  (hermes-preview-test--with-api
    (let* ((chat (hermes-preview-test--chat "failure"))
           (viewer (hermes-preview-test--open chat "/file")))
      (setq buffers (list viewer chat))
      (hermes--promise-resolve (nth 3 (car calls))
                              (hermes-preview-test--response "/different" "wrong"))
      (with-current-buffer viewer
        (should-not hermes-files--bytes)
        (should (string-match-p "Read failed" header-line-format))
        (hermes-preview-retry)
        (let ((old (nth 3 (car calls))))
          (hermes-preview-cancel)
          (hermes-preview-retry)
          (hermes--promise-reject old 'late)
          (should (equal hermes-files--status "Loading")))
        (hermes--promise-resolve (nth 3 (car calls))
                                (hermes-preview-test--response "/file" "retry succeeded"))
        (should (equal hermes-files--status "Ready"))
        (should (equal hermes-files--bytes "retry succeeded"))
        (should-not hermes-preview--cleanup)))))

(ert-deftest hermes-preview-picker-revalidates-after-prompt ()
  (hermes-preview-test--with-api
    (let ((chat (hermes-preview-test--chat "picker")))
      (setq buffers (list chat))
      (with-current-buffer chat
        (hermes-chat--insert-entry (hermes-preview-test--entry "/output"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     (hermes-chat--invalidate-transport-state)
                     (caar choices))))
          (should-error (hermes-chat-preview-output) :type 'user-error))
        (should-not calls)))))

(ert-deftest hermes-preview-synchronous-setup-failure-settles-and-releases ()
  (hermes-preview-test--with-api
    (let* ((chat (hermes-preview-test--chat "setup"))
           (viewer (hermes-preview-test--open chat "/file")))
      (setq buffers (list viewer chat))
      (with-current-buffer viewer
        (hermes-preview-cancel)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-subscribe)
                   (lambda (&rest _) (error "Subscription failed"))))
          (should-error (hermes-preview-retry)))
        (should-not hermes-preview--cleanup)
        (should (string-match-p "Read unavailable" header-line-format))
        (should (= 1 (hermes-dashboard-transport-client-refcount
                      (nth 3 hermes-preview--owner))))
        (hermes-preview-retry)
        (should (equal hermes-files--status "Loading"))))))

(ert-deftest hermes-preview-pending-retry-blocks-source-toggle ()
  (hermes-preview-test--with-api
    (let* ((chat (hermes-preview-test--chat "source-retry"))
           (viewer (hermes-preview-test--open chat "/page.html")))
      (setq buffers (list viewer chat))
      (hermes--promise-resolve (nth 3 (car calls))
                              (hermes-preview-test--response "/page.html" "<p>Old</p>"))
      (with-current-buffer viewer
        (hermes-preview-retry)
        (let ((request hermes-preview--cleanup)
              (owner hermes-preview--owner)
              (header header-line-format)
              (content (buffer-string)))
          (should-error (call-interactively (key-binding (kbd "v"))) :type 'user-error)
          (should (eq request hermes-preview--cleanup))
          (should (eq owner hermes-preview--owner))
          (should (equal header header-line-format))
          (should (equal content (buffer-string)))
          (should (equal hermes-files--status "Loading"))
          (should-not hermes-preview--source-p))
        (hermes--promise-resolve (nth 3 (car calls))
                                (hermes-preview-test--response "/page.html" "<p>New</p>"))
        (call-interactively (key-binding (kbd "v")))
        (should (equal (buffer-string) "<p>New</p>"))
        (should-not hermes-preview--cleanup)))))

(ert-deftest hermes-preview-zoom-rejects-non-image-and-pending-states ()
  (hermes-preview-test--with-api
    (let* ((chat (hermes-preview-test--chat "zoom-unavailable"))
           (viewer (hermes-preview-test--open chat "/page.html")))
      (setq buffers (list viewer chat))
      (with-current-buffer viewer
        (dolist (key '("+" "-" "0"))
          (should-error (call-interactively (key-binding (kbd key))) :type 'user-error))
        (hermes--promise-resolve (nth 3 (car calls))
                                (hermes-preview-test--response "/page.html" "<p>Text</p>"))
        (dolist (key '("+" "-" "0"))
          (should-error (call-interactively (key-binding (kbd key))) :type 'user-error))
        (hermes-preview-source)
        (should-error (call-interactively (key-binding (kbd "+"))) :type 'user-error)
        (should (equal (buffer-string) "<p>Text</p>"))
        (should (= (length calls) 1))))))

(ert-deftest hermes-preview-zoom-real-images-retain-bytes-and-owner ()
  ;; Batch Emacs has no image frame; run this test in a disposable GUI too.
  (skip-unless (and (display-images-p) (memq 'scale (image-transforms-p))
                    (image-type-available-p 'png) (image-type-available-p 'svg)
                    (fboundp 'libxml-parse-xml-region)))
  (dolist (fixture
           (list (cons "/image.png"
                       (base64-decode-string
                        "iVBORw0KGgoAAAANSUhEUgAAACgAAAAUCAIAAABwJOjsAAAAJElEQVR4nO3NMQ0AAAwEofdvupVxCwk7uy3RrGKxWCwWi8WJB336HQ594lo5AAAAAElFTkSuQmCC"))
                 (cons "/image.svg"
                       "<svg xmlns='http://www.w3.org/2000/svg' width='1200' height='600'><rect width='1200' height='600' fill='red'/></svg>")))
    (hermes-preview-test--with-api
      (let* ((chat (hermes-preview-test--chat "zoom"))
             (viewer (hermes-preview-test--open chat (car fixture)))
             (bytes (cdr fixture))
             (directory (make-temp-file "preview-zoom-" t))
             (filename (expand-file-name "saved" directory)))
        (setq buffers (list viewer chat))
        (unwind-protect
            (progn
              (hermes--promise-resolve (nth 3 (car calls))
                                      (hermes-preview-test--response (car fixture) bytes))
              (with-current-buffer viewer
                (let* ((image (get-text-property (point-min) 'display))
                       (original (copy-sequence image))
                       (size (image-size image t))
                       (owner hermes-preview--owner))
                  (should (eq (car image) 'image))
                  (should-not (image-property image :file))
                  (goto-char (point-max))
                  (call-interactively (key-binding (kbd "+")))
                  (should (> (car (image-size image t)) (car size)))
                  (should (> (cdr (image-size image t)) (cdr size)))
                  (call-interactively (key-binding (kbd "-")))
                  (should (equal (image-size image t) size))
                  (call-interactively (key-binding (kbd "+")))
                  (hermes-file-save filename)
                  (call-interactively (key-binding (kbd "0")))
                  (should (equal image original))
                  (should (equal (image-size image t) size))
                  (should (eq owner hermes-preview--owner))
                  (should (eq major-mode 'hermes-preview-mode))
                  (should (equal hermes-files--bytes bytes))
                  (should (= (length calls) 1))
                  (dotimes (_ 30) (hermes-preview-zoom-in))
                  (should (<= (car (image-size image t)) (* 4 (car size))))
                  (dotimes (_ 60) (hermes-preview-zoom-out))
                  (should (>= (car (image-size image t)) (max 1 (round (* 0.1 (car size))))))
                  (hermes-preview-zoom-reset)
                  (should (= (length calls) 1))
                  (cl-letf (((symbol-function 'image-transforms-p) (lambda (&rest _) nil)))
                    (should-error (hermes-preview-zoom-in) :type 'user-error))
                  (hermes-preview-retry)
                  (let ((request hermes-preview--cleanup) (header header-line-format))
                    (should-error (hermes-preview-zoom-in) :type 'user-error)
                    (should (eq request hermes-preview--cleanup))
                    (should (equal header header-line-format))
                    (should (equal hermes-files--status "Loading")))
                  (hermes-preview-cancel)
                  (when (string-suffix-p ".svg" (car fixture))
                    (hermes-preview-source)
                    (should-error (hermes-preview-zoom-in) :type 'user-error)
                    (should (equal (buffer-string) bytes))
                    (hermes-preview-source)
                    (hermes-preview-zoom-in))))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally filename)
                (should (equal (buffer-string) bytes))))
          (delete-directory directory t))))))

(ert-deftest hermes-preview-auth-wait-retirement-prevents-wire-read ()
  (save-window-excursion
    (dolist (retire '(cancel retry chat-replace viewer-mode viewer-kill))
      (let* ((auth (hermes--promise-make)) sent
             (chat (hermes-preview-test--chat "auth-owner")) viewer
             (client (buffer-local-value 'hermes-chat--dashboard-client chat)))
        (unwind-protect
            (cl-letf (((symbol-function 'hermes-dashboard-transport-api-auth-async) (lambda () auth))
                      ((symbol-function 'hermes-dashboard-transport--http-json-request-async)
                       (lambda (request &rest _)
                         (push request sent)
                         (hermes--promise-resolved
                          (list :status 200 :body
                                (hermes-preview-test--response "/generated.txt" "new")))))
                      ((symbol-function 'hermes-dashboard-transport-release)
                       (lambda (client) (cl-decf (hermes-dashboard-transport-client-refcount client)))))
              (with-current-buffer chat
                (setq viewer (hermes-preview-open '(:path "/generated.txt" :label "output")
                                                  (hermes-chat--preview-owner))))
              (should-not sent)
              (pcase retire
                ('cancel (with-current-buffer viewer (call-interactively (key-binding (kbd "C-g")))))
                ('retry (with-current-buffer viewer (call-interactively (key-binding (kbd "g")))))
                ('chat-replace (with-current-buffer chat
                                 (setq hermes-chat--dashboard-active-session-id "successor")))
                ('viewer-mode (with-current-buffer viewer (fundamental-mode)))
                ('viewer-kill (kill-buffer viewer)))
              (hermes--promise-resolve auth '(:session-token "test"))
              (should (= (length sent) (if (eq retire 'retry) 1 0)))
              (should (= (hermes-dashboard-transport-client-refcount client) 1))
              (when (memq retire '(cancel retry chat-replace))
                (should (equal (buffer-local-value 'hermes-files--status viewer)
                               (if (eq retire 'retry) "Ready" "Cancelled"))))
              (when (eq retire 'retry)
                (should (equal (buffer-local-value 'hermes-files--bytes viewer) "new"))))
          (when (buffer-live-p viewer) (kill-buffer viewer))
          (when (buffer-live-p chat) (kill-buffer chat)))))))

(provide 'hermes-preview-tests)
;;; hermes-preview-tests.el ends here
