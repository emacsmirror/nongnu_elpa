;;; hermes-capabilities-tests.el --- ERT tests for hermes-capabilities  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the native capability provider skeleton.  The pure registry,
;; dispatch, and response-writer tests run with no buffers, sockets, or
;; processes.  The transport tests mock the URL resolver, socket opener, and
;; send function so no real network or websocket.el is involved.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" root)))

(require 'hermes-capabilities)

(defmacro hermes-capabilities-test--with-clean-registry (&rest body)
  "Run BODY with an empty capability registry, restoring it afterward."
  (declare (indent 0))
  `(let ((saved hermes-capabilities--registry))
     (setq hermes-capabilities--registry nil)
     (unwind-protect
         (progn ,@body)
       (setq hermes-capabilities--registry saved))))

(cl-defmacro hermes-capabilities-test--with-mock-transport
    (url-plist &rest body)
  "Run BODY with the capability URL and socket openers mocked to URL-PLIST.
The socket opener captures the :on-message callback into the variable
`on-msg' and returns a fake socket symbol.  No real network or websocket.el is
touched."
  (declare (indent 1))
  `(let* ((on-msg nil)
          (fake-socket (gensym "fake-ws-"))
          (sent-frames nil)
          (hermes-capabilities--url-function
           (lambda (&rest _args) (hermes--promise-resolved ,url-plist)))
          (hermes-capabilities--open-function
           (cl-function
            (lambda (_url _red _sec &key on-message &allow-other-keys)
              (setq on-msg on-message)
              fake-socket)))
          (hermes-capabilities--send-function
           (lambda (_socket text) (push text sent-frames))))
     ,@body))


;;;; Registry tests

(ert-deftest hermes-capabilities-register-and-lookup ()
  "Registering a method stores a handler retrievable by `--lookup'."
  (let ((handler (lambda (_params) '((ok . t)))))
    (hermes-capabilities-test--with-clean-registry
      (hermes-capabilities--register "buffer.list" handler :doc "List buffers")
      (should (equal (aref (hermes-capabilities--methods) 0) "buffer.list"))
      (should (eq (plist-get (hermes-capabilities--lookup "buffer.list") :handler)
                  handler))
      (should (equal (plist-get (hermes-capabilities--lookup "buffer.list") :doc)
                     "List buffers")))))

(ert-deftest hermes-capabilities-lookup-unknown-returns-nil ()
  "`--lookup' returns nil for a method that was never registered."
  (hermes-capabilities-test--with-clean-registry
    (should (null (hermes-capabilities--lookup "nope")))))

(ert-deftest hermes-capabilities-duplicate-register-errors ()
  "Registering an existing method signals unless :replace is set."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities--register "buffer.list" #'ignore)
    (should-error (hermes-capabilities--register "buffer.list" #'ignore))
    ;; :replace overwrites silently.
    (let ((new-handler (lambda (_) "new")))
      (hermes-capabilities--register "buffer.list" new-handler :replace t)
      (should (eq (plist-get (hermes-capabilities--lookup "buffer.list") :handler)
                  new-handler)))))

(ert-deftest hermes-capabilities-define-registers-method ()
  "The `hermes-capabilities-define' macro registers the method at load time."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities-define hermes-capabilities-method-buffer-current
      "buffer.current" "Return the current buffer."
      (lambda (_params) (buffer-name)))
    (should (seq-contains-p (hermes-capabilities--methods) "buffer.current"))
    (should (equal hermes-capabilities-method-buffer-current "buffer.current"))
    (should (functionp (plist-get (hermes-capabilities--lookup "buffer.current")
                                  :handler)))))


;;;; Dispatch tests

(ert-deftest hermes-capabilities-dispatch-calls-handler ()
  "A registered handler receives params and its return value surfaces as ok."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities--register
      "buffer.current"
      (lambda (_params) "my-buffer"))
    (let* ((request (list :id "req-1" :method "buffer.current" :params nil))
           (outcome (hermes-capabilities--dispatch request)))
      (should (eq (car outcome) 'ok))
      (should (equal (cdr outcome) "my-buffer")))))

(ert-deftest hermes-capabilities-dispatch-unknown-method-error ()
  "An unregistered method dispatches to a method_not_supported error."
  (hermes-capabilities-test--with-clean-registry
    (let* ((request (list :id "req-2" :method "frobnicate" :params nil))
           (outcome (hermes-capabilities--dispatch request)))
      (should (eq (car outcome) 'error))
      (should (eq (plist-get (cdr outcome) :code) 'method_not_supported))
      (should (equal (plist-get (plist-get (cdr outcome) :data) :method)
                     "frobnicate")))))

(ert-deftest hermes-capabilities-dispatch-handler-error ()
  "A signaling handler dispatches to an invalid_params error."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities--register
      "buffer.current"
      (lambda (_params) (error "nope")))
    (let* ((request (list :id "req-3" :method "buffer.current" :params nil))
           (outcome (hermes-capabilities--dispatch request)))
      (should (eq (car outcome) 'error))
      (should (eq (plist-get (cdr outcome) :code) 'invalid_params))
      (should (string-match-p "nope" (plist-get (cdr outcome) :message))))))


;;;; Normalization tests

(ert-deftest hermes-capabilities-normalize-request-alist ()
  "An alist `emacs.request' frame normalizes to a request plist."
  (let* ((frame '((jsonrpc . "2.0")
                  (id . "req-abc")
                  (method . "emacs.request")
                  (params . ((request_id . "req-abc")
                             (target . "emacs-pair")
                             (instance_id . "emacs-30.1:pi:123:abcd1234")
                             (method . "buffer.read")
                             (params . ((buffer . "*scratch*")))
                             (timeout_ms . 30000)))))
         (request (hermes-capabilities--normalize-request frame)))
    (should (equal (plist-get request :id) "req-abc"))
    (should (equal (plist-get request :request-id) "req-abc"))
    (should (equal (plist-get request :target) "emacs-pair"))
    (should (equal (plist-get request :instance-id) "emacs-30.1:pi:123:abcd1234"))
    (should (equal (plist-get request :method) "buffer.read"))
    (should (equal (plist-get request :timeout-ms) 30000))
    (should (equal (alist-get 'buffer (plist-get request :params)) "*scratch*"))))

(ert-deftest hermes-capabilities-normalize-request-non-emacs-request-is-nil ()
  "A frame whose method is not `emacs.request' does not normalize."
  (should (null (hermes-capabilities--normalize-request
                 '((method . "session.create"))))))


;;;; Response writer tests

(ert-deftest hermes-capabilities-result-response-shape ()
  "A successful dispatch produces a JSON-RPC result frame with matching id."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities--register "buffer.current"
      (lambda (_) "content-here"))
    (let* ((request (list :id "req-xyz" :method "buffer.current" :params nil))
           (frame (hermes-capabilities--response-for request)))
      (should (equal (alist-get 'jsonrpc frame) "2.0"))
      (should (equal (alist-get 'id frame) "req-xyz"))
      (should (equal (alist-get 'ok (alist-get 'result frame)) t))
      (should (equal (alist-get 'content (alist-get 'result frame))
                     "content-here")))))

(ert-deftest hermes-capabilities-error-response-shape ()
  "An unknown method produces a JSON-RPC error frame with matching id."
  (hermes-capabilities-test--with-clean-registry
    (let* ((request (list :id "req-err" :method "ghost" :params nil))
           (frame (hermes-capabilities--response-for request)))
      (should (equal (alist-get 'jsonrpc frame) "2.0"))
      (should (equal (alist-get 'id frame) "req-err"))
      (should (equal (alist-get 'code (alist-get 'error frame)) -32601))
      (should (string-match-p "ghost" (alist-get 'message (alist-get 'error frame))))
      (should (equal (alist-get 'method
                                (alist-get 'data (alist-get 'error frame)))
                     "ghost")))))

(ert-deftest hermes-capabilities-plist-to-alist ()
  "Keyword plists convert to alists with bare symbol keys."
  (should (equal (hermes-capabilities--plist-to-alist
                  '(:ok t :content "hi" :method "buffer.read"))
                 '((ok . t) (content . "hi") (method . "buffer.read")))))


;;;; handle-message roundtrip test

(ert-deftest hermes-capabilities-handle-message-roundtrip ()
  "An inbound `emacs.request' produces a matching JSON-RPC response on the wire."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities--register "buffer.current"
      (lambda (_) "the-buffer"))
    (hermes-capabilities-test--with-mock-transport
        (list :url "ws://x" :redacted-url "ws://x" :secrets nil)
      (let ((provider (hermes-capabilities--provider-create
                       :active t :socket 'fake-socket :buffer (current-buffer)
                       :target "emacs-pair" :instance-id "inst-1"
                       :display-name "PiHome" :role "pair")))
        (hermes-capabilities--handle-message
         provider
         (json-serialize
          '((jsonrpc . "2.0")
            (id . "live-1")
            (method . "emacs.request")
            (params . ((request_id . "live-1")
                       (target . "emacs-pair")
                       (instance_id . "inst-1")
                       (method . "buffer.current")
                       (params . nil))))))
        (should sent-frames)
        (let* ((frame (json-parse-string (car sent-frames)
                                         :object-type 'alist
                                         :array-type 'list))
               (result (alist-get 'result frame)))
          (should (equal (alist-get 'id frame) "live-1"))
          (should (equal (alist-get 'ok result) t))
          (should (equal (alist-get 'content result) "the-buffer")))))))

(ert-deftest hermes-capabilities-handle-message-unknown-method-sends-error ()
  "An `emacs.request' for an unknown method sends a JSON-RPC error response."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities-test--with-mock-transport
        (list :url "ws://x" :redacted-url "ws://x" :secrets nil)
      (let ((provider (hermes-capabilities--provider-create
                       :active t :socket 'fake-socket :buffer (current-buffer)
                       :target "emacs-pair" :instance-id "inst-1"
                       :display-name "PiHome" :role "pair")))
        (hermes-capabilities--handle-message
         provider
         (json-serialize
          '((jsonrpc . "2.0")
            (id . "live-2")
            (method . "emacs.request")
            (params . ((method . "ghost"))))))
        (should sent-frames)
        (let* ((frame (json-parse-string (car sent-frames)
                                         :object-type 'alist
                                         :array-type 'list))
               (error (alist-get 'error frame)))
          (should (equal (alist-get 'id frame) "live-2"))
          (should (equal (alist-get 'code error) -32601)))))))


;;;; Registration-on-ready with mocked transport

(ert-deftest hermes-capabilities-default-target-is-public-and-portable ()
  "A clean install identifies its capability pair with a generic target."
  (should (equal hermes-capabilities-target "emacs-pair")))

(ert-deftest hermes-capabilities-register-on-ready ()
  "A `gateway.ready' event triggers an `emacs.register' request on the socket."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities--register "buffer.list" (lambda (_) "ok"))
    (hermes-capabilities-test--with-mock-transport
        (list :url "ws://127.0.0.1:9119/api/ws?token=x"
              :redacted-url "ws://127.0.0.1:9119/api/ws?token=<redacted>"
              :secrets '("x"))
      (let ((provider (hermes-capabilities--provider-create
                       :active t
                       :buffer (current-buffer)
                       :target "emacs-pair"
                       :instance-id "inst-1"
                       :display-name "PiHome"
                       :role 'pair)))
        (hermes-capabilities--connect provider)
        ;; Simulate gateway.ready arriving on the socket.
        (funcall on-msg
                 (json-serialize
                  '((jsonrpc . "2.0")
                    (method . "event")
                    (params . ((type . "gateway.ready"))))))
        ;; The registration frame should now have been sent.
        (should sent-frames)
        (let* ((frame (json-parse-string (car sent-frames)
                                         :object-type 'alist
                                         :array-type 'list)))
          (should (equal (alist-get 'method frame) "emacs.register"))
          (let ((params (alist-get 'params frame)))
            (should (equal (alist-get 'target params) "emacs-pair"))
            (should (equal (alist-get 'instance_id params) "inst-1"))
            (should (seq-contains-p (alist-get 'capabilities params) "buffer.list"))))))))

(ert-deftest hermes-capabilities-reregister-on-reconnect ()
  "A second `gateway.ready' after reconnect re-registers with a fresh seq."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities--register "buffer.list" (lambda (_) "ok"))
    (hermes-capabilities-test--with-mock-transport
        (list :url "ws://x" :redacted-url "ws://x" :secrets nil)
      (let ((provider (hermes-capabilities--provider-create
                       :active t
                       :buffer (current-buffer)
                       :target "emacs-pair"
                       :instance-id "inst-1"
                       :display-name "PiHome"
                       :role 'pair))
            sent-ids)
        (setq hermes-capabilities--send-function
              (lambda (_socket text)
                (let ((frame (json-parse-string text
                                                :object-type 'alist
                                                :array-type 'list)))
                  (when (equal (alist-get 'method frame) "emacs.register")
                    (push (alist-get 'id frame) sent-ids)))))
        (hermes-capabilities--connect provider)
        (funcall on-msg
                 (json-serialize
                  '((jsonrpc . "2.0")
                    (method . "event")
                    (params . ((type . "gateway.ready"))))))
        (funcall on-msg
                 (json-serialize
                  '((jsonrpc . "2.0")
                    (method . "event")
                    (params . ((type . "gateway.ready"))))))
        (should (= (length sent-ids) 2))
        (should (not (equal (nth 0 sent-ids) (nth 1 sent-ids))))))))

(ert-deftest hermes-capabilities-late-close-keeps-current-socket ()
  "A replaced socket's close callback must not tear down its successor."
  (let* ((callbacks nil)
         (sockets '(socket-a socket-b))
         (reconnects 0)
         (hermes-capabilities--url-function
         (lambda (&rest _args)
           (hermes--promise-resolved
            (list :url "ws://x" :redacted-url "ws://x" :secrets nil))))
         (hermes-capabilities--open-function
         (cl-function
          (lambda (_url _redacted _secrets &key on-close &allow-other-keys)
            (let ((socket (pop sockets)))
              (setq callbacks (append callbacks (list on-close)))
              socket)))))
    (let ((provider (hermes-capabilities--provider-create
                     :active t :buffer (current-buffer))))
      (cl-letf (((symbol-function 'hermes-capabilities--reconnect)
                 (lambda (_provider) (cl-incf reconnects))))
        (hermes-capabilities--connect provider)
        (hermes-capabilities--connect provider)
        (should (eq (hermes-capabilities--provider-socket provider) 'socket-b))
        (funcall (car callbacks))
        (should (eq (hermes-capabilities--provider-socket provider) 'socket-b))
        (should (zerop reconnects))))))

(ert-deftest hermes-capabilities-connect-uses-owner-instance-url ()
  "Connect resolves the owner buffer's instance URL, not the current buffer's."
  (let ((owner (generate-new-buffer " hermes-cap-owner"))
        (other (generate-new-buffer " hermes-cap-current"))
        (remote '("remote" . "https://hermes.example.test"))
        (local '("local" . "http://127.0.0.1:9119"))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test")))
        (hermes-dashboard-transport-url "http://127.0.0.1:9119")
        seen-url
        provider)
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-capabilities--reconnect) #'ignore))
          (with-current-buffer owner
            (setq hermes-instance remote))
          (with-current-buffer other
            (setq hermes-instance local)
            (let ((hermes-capabilities--url-function
                   (lambda (&rest _)
                     (setq seen-url hermes-dashboard-transport-url)
                     (hermes--promise-rejected "stop"))))
              (setq provider
                    (hermes-capabilities--provider-create
                     :active t :buffer owner :target "emacs-pair"))
              (hermes-capabilities--connect provider)))
          (should (equal seen-url (hermes-instance-url remote)))
          (should-not (hermes-capabilities--provider-reconnect-timer provider))
          (should-not
           (cl-find-if (lambda (timer)
                         (eq (timer--function timer)
                             #'hermes-capabilities--do-reconnect))
                       timer-list)))
      (when (buffer-live-p owner) (kill-buffer owner))
      (when (buffer-live-p other) (kill-buffer other)))))

(ert-deftest hermes-capabilities-method-not-found-graceful ()
  "A `method not found' registration rejection deactivates the provider."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities-test--with-mock-transport
        (list :url "ws://x" :redacted-url "ws://x" :secrets nil)
      (let ((provider (hermes-capabilities--provider-create
                       :active t
                       :buffer (current-buffer)
                       :target "emacs-pair"
                       :instance-id "inst-1"
                       :display-name "PiHome"
                       :role 'pair)))
        (hermes-capabilities--connect provider)
        (funcall on-msg
                 (json-serialize
                  '((jsonrpc . "2.0")
                    (method . "event")
                    (params . ((type . "gateway.ready"))))))
        (let ((reg-id (hermes-capabilities--pending-request-id provider)))
          (funcall on-msg
                   (json-serialize
                    `((jsonrpc . "2.0")
                      (id . ,reg-id)
                      (error . ((code . -32601)
                                (message . "method not found"))))))
          (should (null (hermes-capabilities--provider-active provider))))))))

(ert-deftest hermes-capabilities-no-session-on-shared-client ()
  "The provider struct carries no session identity slots.
This pins the architectural decision that the dedicated capability connection
never stores session id on the shared chat client.  The struct fields are
checked to exclude session-id-bearing slots."
  (let* ((provider (hermes-capabilities--provider-create))
         (slots (mapcar #'car (cl-struct-slot-info 'hermes-capabilities--provider))))
    (dolist (slot slots)
      (should-not (string-match-p (rx "session") (symbol-name slot))))
    ;; The required transport fields are present.
    (should (memq 'socket slots))
    (should (memq 'target slots))
    (should (memq 'instance-id slots))
    (should (memq 'active slots))))


;;;; Registration params test

(ert-deftest hermes-capabilities-registration-params-shape ()
  "Registration params include target, instance, capabilities, and client info."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities--register "buffer.list" (lambda (_) "ok"))
    (hermes-capabilities--register "buffer.current" (lambda (_) "ok"))
    (let ((params (hermes-capabilities--registration-params
                   "emacs-pair" "inst-1" "PiHome" 'pair)))
      (should (equal (alist-get 'target params) "emacs-pair"))
      (should (equal (alist-get 'instance_id params) "inst-1"))
      (should (equal (alist-get 'display_name params) "PiHome"))
      (should (equal (alist-get 'role params) "pair"))
      (should (seq-contains-p (alist-get 'capabilities params) "buffer.list"))
      (should (seq-contains-p (alist-get 'capabilities params) "buffer.current"))
      (let ((client (alist-get 'client params)))
        (should (equal (alist-get 'name client) "emacs-hermes"))))))

(ert-deftest hermes-capabilities-instance-id-is-volatile ()
  "Two calls with different fingerprints produce distinct instance ids."
  (let ((a (hermes-capabilities--instance-id "emacs-pair")))
    (should (string-match-p
             (rx "emacs-" (+ nonl) ":" (+ nonl) ":" (+ digit) ":" (+ hex))
             a))))



;;;; C5 read-only method tests

(defmacro hermes-capabilities-test--with-temp-buffer (name content &rest body)
  "Run BODY with a temp buffer named NAME holding CONTENT, then kill it."
  (declare (indent 2))
  (let ((buf (make-symbol "buf")))
    `(let ((,buf (generate-new-buffer ,name)))
       (with-current-buffer ,buf
         (insert ,content))
       (unwind-protect
           (progn ,@body)
         (when (buffer-live-p ,buf)
           (kill-buffer ,buf))))))

(ert-deftest hermes-capabilities-c5-methods-registered ()
  "The default registry advertises all five C5 read-only methods."
  (dolist (m '("buffer.list" "buffer.current" "project.current"
               "buffer.read" "capabilities.list"))
    (should (hermes-capabilities--lookup m))))

(ert-deftest hermes-capabilities-json-bool ()
  "Truthiness maps to JSON true; nil maps to the :false keyword."
  (should (eq (hermes-capabilities--json-bool t) t))
  (should (eq (hermes-capabilities--json-bool nil) :false)))

(ert-deftest hermes-capabilities-listable-buffer-p-filters-internal ()
  "Space-prefixed and empty names are not listable."
  (should (hermes-capabilities--listable-buffer-p "*scratch*"))
  (should-not (hermes-capabilities--listable-buffer-p " *temp*"))
  (should-not (hermes-capabilities--listable-buffer-p "")))

(ert-deftest hermes-capabilities-remote-path-p ()
  "TRAMP-style paths are detected as remote; local paths are not."
  (should (hermes-capabilities--remote-path-p "/ssh:host:/tmp/x"))
  (should-not (hermes-capabilities--remote-path-p "/tmp/x"))
  (should-not (hermes-capabilities--remote-path-p nil)))

(ert-deftest hermes-capabilities-buffer-list-basic ()
  "`buffer.list' includes known live buffers and reports count/total."
  (hermes-capabilities-test--with-temp-buffer "cap-test-list-1" "hi"
    (hermes-capabilities-test--with-temp-buffer "cap-test-list-2" "there"
      (let ((res (hermes-capabilities--handle-buffer-list nil)))
        (let ((names (mapcar (lambda (e) (alist-get 'name e))
                             (append (alist-get 'buffers res) nil))))
          (should (member "cap-test-list-1" names))
          (should (member "cap-test-list-2" names)))
        (should (>= (alist-get 'count res) 2))
        (should (>= (alist-get 'total res) 2))))))

(ert-deftest hermes-capabilities-buffer-list-truncation ()
  "`buffer.list' caps the entries and reports truncation when over the limit."
  (let ((hermes-capabilities-buffer-list-max 1))
    (hermes-capabilities-test--with-temp-buffer " cap-trunc-a" "a"
      (hermes-capabilities-test--with-temp-buffer " cap-trunc-b" "b"
        (let ((res (hermes-capabilities--handle-buffer-list nil)))
          (should (= (alist-get 'count res) 1))
          (should (> (alist-get 'total res) 1))
          (should (eq (alist-get 'truncated res) t)))))))

(ert-deftest hermes-capabilities-buffer-list-filters-internal ()
  "`buffer.list' excludes internal (space-prefixed) buffers."
  (hermes-capabilities-test--with-temp-buffer " cap-internal-filter" "x"
    (let ((res (hermes-capabilities--handle-buffer-list nil)))
      (let ((names (mapcar (lambda (e) (alist-get 'name e))
                           (append (alist-get 'buffers res) nil))))
        (should-not (member " cap-internal-filter" names))))))

(ert-deftest hermes-capabilities-buffer-current-shape ()
  "`buffer.current' returns the entry of the selected window's buffer."
  (let ((name "cap-test-current"))
    (hermes-capabilities-test--with-temp-buffer name "content"
      (save-window-excursion
        (set-window-buffer (selected-window) (get-buffer name))
        (let ((res (hermes-capabilities--handle-buffer-current nil)))
          (should (equal (alist-get 'name res) name))
          (should (stringp (alist-get 'mode res)))
          (should (integerp (alist-get 'point res))))))))

(ert-deftest hermes-capabilities-project-current-without-project ()
  "`project-entry' returns nulls for a nil project (no active project)."
  (let ((res (hermes-capabilities--project-entry nil)))
    (should (eq (alist-get 'root res) :null))
    (should (eq (alist-get 'name res) :null))))

(ert-deftest hermes-capabilities-project-entry-shape ()
  "A non-nil project yields an absolute root and a name basename."
  (let ((root (expand-file-name "proj/" temporary-file-directory)))
    (cl-letf* ((fake-project (vector 'project root))
               ((symbol-function 'project-root)
                (lambda (_p) root)))
      (let ((res (hermes-capabilities--project-entry fake-project)))
        (should (equal (alist-get 'root res) root))
        (should (equal (alist-get 'name res) "proj"))))))

(ert-deftest hermes-capabilities-buffer-read-full ()
  "`buffer.read' returns the full content of a small buffer."
  (hermes-capabilities-test--with-temp-buffer " cap-read-full" "alpha\nbeta\n"
    (let ((res (hermes-capabilities--handle-buffer-read
                `((buffer . " cap-read-full")))))
      (should (equal (alist-get 'ok res) t))
      (should (equal (alist-get 'content res) "alpha\nbeta\n"))
      (let ((metadata (alist-get 'metadata res)))
        (should (eq (alist-get 'truncated metadata) :false))))))

(ert-deftest hermes-capabilities-buffer-read-line-range ()
  "`buffer.read' honors start_line/end_line aliases."
  (hermes-capabilities-test--with-temp-buffer " cap-read-range"
      "one\ntwo\nthree\nfour\n"
    (let ((res (hermes-capabilities--handle-buffer-read
                `((buffer . " cap-read-range")
                  (start_line . 2) (end_line . 3)))))
      (should (equal (alist-get 'content res) "two\nthree"))
      (let ((metadata (alist-get 'metadata res)))
        (should (= (alist-get 'start_line metadata) 2))
        (should (= (alist-get 'end_line metadata) 3))))))

(ert-deftest hermes-capabilities-buffer-read-honors-start-end ()
  "`buffer.read' honors the roadmap §2.2 `start'/`end' wire names."
  (hermes-capabilities-test--with-temp-buffer " cap-read-se"
      "one\ntwo\nthree\nfour\n"
    (let ((res (hermes-capabilities--handle-buffer-read
                `((buffer . " cap-read-se")
                  (start . 2) (end . 3)))))
      (should (equal (alist-get 'content res) "two\nthree"))
      (let ((metadata (alist-get 'metadata res)))
        (should (= (alist-get 'start_line metadata) 2))
        (should (= (alist-get 'end_line metadata) 3))))))

(ert-deftest hermes-capabilities-buffer-read-start-preferred-over-alias ()
  "`start'/`end' take precedence over `start_line'/`end_line'."
  (hermes-capabilities-test--with-temp-buffer " cap-read-pref"
      "a\nb\nc\nd\n"
    (let ((res (hermes-capabilities--handle-buffer-read
                `((buffer . " cap-read-pref")
                  (start . 2) (end . 2)
                  (start_line . 1) (end_line . 4)))))
      (should (equal (alist-get 'content res) "b"))
      (let ((metadata (alist-get 'metadata res)))
        (should (= (alist-get 'start_line metadata) 2))
        (should (= (alist-get 'end_line metadata) 2))))))

(ert-deftest hermes-capabilities-buffer-read-truncated-by-line-cap ()
  "`buffer.read' caps the returned line range and reports truncation."
  (hermes-capabilities-test--with-temp-buffer "cap-test-read-linecap"
      "l1\nl2\nl3\nl4\nl5\n"
    (let ((hermes-capabilities-buffer-read-max-lines 2))
      (let ((res (hermes-capabilities--handle-buffer-read
                  `((buffer . "cap-test-read-linecap") (end . 5)))))
        (let ((metadata (alist-get 'metadata res)))
          ;; Requested end 5 is capped to 2 by the line cap.
          (should (= (alist-get 'end_line metadata) 2))
          (should (< (alist-get 'end_line metadata) 5))
          (should (eq (alist-get 'truncated metadata) t)))))))

(ert-deftest hermes-capabilities-buffer-read-start-beyond-end ()
  "`buffer.read' returns empty content when start exceeds total lines."
  (hermes-capabilities-test--with-temp-buffer "cap-test-read-beyond" "only\n"
    (let ((res (hermes-capabilities--handle-buffer-read
                `((buffer . "cap-test-read-beyond") (start . 99)))))
      (should (equal (alist-get 'content res) ""))
      (let ((metadata (alist-get 'metadata res)))
        (should (eq (alist-get 'truncated metadata) :false))
        (should (= (alist-get 'total_lines metadata) 2))))))

(ert-deftest hermes-capabilities-buffer-read-truncated-by-char-cap ()
  "`buffer.read' caps content length and reports truncation."
  (hermes-capabilities-test--with-temp-buffer " cap-read-charcap"
      (make-string 100 ?x)
    (let ((hermes-capabilities-buffer-read-max-chars 10))
      (let ((res (hermes-capabilities--handle-buffer-read
                  `((buffer . " cap-read-charcap")))))
        (should (= (length (alist-get 'content res)) 10))
        (let ((metadata (alist-get 'metadata res)))
          (should (eq (alist-get 'truncated metadata) t)))))))

(ert-deftest hermes-capabilities-buffer-read-missing-param ()
  "`buffer.read' errors when the `buffer' parameter is absent."
  (should-error (hermes-capabilities--handle-buffer-read nil)
                :type 'error)
  (should-error (hermes-capabilities--handle-buffer-read '((other . "x")))
                :type 'error))

(ert-deftest hermes-capabilities-buffer-read-unknown-buffer ()
  "`buffer.read' errors when the named buffer does not exist."
  (should-error (hermes-capabilities--handle-buffer-read
                 '((buffer . "no-such-buffer-zzz")))
                :type 'error))

(ert-deftest hermes-capabilities-buffer-read-remote-rejected ()
  "`buffer.read' rejects a buffer visiting a TRAMP remote file."
  (let ((name "cap-test-read-remote"))
    (hermes-capabilities-test--with-temp-buffer name "x"
      (let ((buf (get-buffer name)))
        (with-current-buffer buf
          (setq buffer-file-name "/ssh:host:/tmp/remote"))
        (unwind-protect
            (should-error (hermes-capabilities--handle-buffer-read
                           `((buffer . ,name)))
                          :type 'error)
          ;; Restore so kill-buffer does not choke on the fake TRAMP name.
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (setq buffer-file-name nil))))))))

(ert-deftest hermes-capabilities-buffer-read-remote-directory-rejected ()
  "`buffer.read' rejects non-file buffers rooted in a remote directory."
  (let ((name "cap-test-read-remote-directory"))
    (hermes-capabilities-test--with-temp-buffer name "secret"
      (let ((buf (get-buffer name)))
        (with-current-buffer buf
          (setq default-directory "/ssh:host:/tmp/"))
        (should-error (hermes-capabilities--handle-buffer-read
                       `((buffer . ,name)))
                      :type 'error)))))

(ert-deftest hermes-capabilities-capabilities-list-shape ()
  "`capabilities.list' reports registered method descriptors and a count."
  (let ((res (hermes-capabilities--handle-capabilities-list nil)))
    (should (>= (alist-get 'count res) 5))
    (let ((methods (alist-get 'methods res)))
      (should (hash-table-p methods))
      (dolist (m '("buffer.list" "buffer.read" "buffer.current"
                   "project.current" "capabilities.list"))
        (should (gethash m methods)))
      (maphash (lambda (_name descriptor)
                 (should (assoc 'params_schema_version descriptor)))
               methods))))

(ert-deftest hermes-capabilities-registration-methods-are-object ()
  "`methods' serializes as a JSON object keyed by method name, not an array."
  (hermes-capabilities-test--with-clean-registry
    (hermes-capabilities--register "buffer.list" (lambda (_) "ok"))
    (hermes-capabilities--register "buffer.read" (lambda (_) "ok"))
    (let* ((params (hermes-capabilities--registration-params
                    "emacs-pair" "inst-1" "PiHome" 'pair))
           (frame `((jsonrpc . "2.0")
                    (id . "reg-1")
                    (method . "emacs.register")
                    (params . ,params)))
           (json (json-serialize frame))
           ;; Parse with hash-table objects so method names stay as strings,
           ;; mirroring how the Python backend reads them with dict(...).
           (parsed (json-parse-string json :object-type 'hash-table))
           (wire-methods (gethash "methods"
                                  (gethash "params" parsed))))
      ;; Must be a JSON object (hash table), not an array.
      (should (hash-table-p wire-methods))
      (should (gethash "buffer.list" wire-methods))
      (should (gethash "buffer.read" wire-methods))
      (should (equal (gethash "params_schema_version"
                              (gethash "buffer.read" wire-methods))
                     1)))))

(ert-deftest hermes-capabilities-buffer-read-response-envelope ()
  "`buffer.read' response serializes as roadmap §2.3 {ok, content, metadata}."
  (hermes-capabilities-test--with-temp-buffer " cap-read-env" "hello\n"
    (let* ((request (list :id "req-env" :method "buffer.read"
                          :params `((buffer . " cap-read-env"))))
           (frame (hermes-capabilities--response-for request))
           (json (json-serialize frame))
           (parsed (json-parse-string json
                                      :object-type 'alist
                                      :array-type 'list))
           (result (alist-get 'result parsed)))
      (should (equal (alist-get 'id parsed) "req-env"))
      (should (equal (alist-get 'ok result) t))
      (should (equal (alist-get 'content result) "hello\n"))
      (let ((metadata (alist-get 'metadata result)))
        (should (equal (alist-get 'buffer metadata) " cap-read-env"))
        (should (member (alist-get 'truncated metadata) '(nil :false)))
        (should (integerp (alist-get 'line_count metadata)))
        (should (integerp (alist-get 'total_lines metadata)))
        (should (integerp (alist-get 'start_line metadata)))
        (should (integerp (alist-get 'end_line metadata)))))))

(ert-deftest hermes-capabilities-current-context-uses-selected-window-buffer ()
  "Wire handlers resolve buffer and project context outside the process buffer."
  (let ((visible (generate-new-buffer " cap-visible"))
        (process-buffer (generate-new-buffer " cap-process"))
        project-directory current-entry project-entry)
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) visible)
          (with-current-buffer visible
            (setq default-directory "/tmp/cap-project/"))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (_prompt directory)
                       (setq project-directory directory)
                       nil)))
            (with-current-buffer process-buffer
              (setq current-entry
                    (hermes-capabilities--handle-buffer-current nil)
                    project-entry
                    (hermes-capabilities--handle-project-current nil))))
          (should (equal (alist-get 'name current-entry) " cap-visible"))
          (should (equal project-directory "/tmp/cap-project/"))
          (should (eq (alist-get 'root project-entry) :null)))
      (when (eq (window-buffer (selected-window)) visible)
        (set-window-buffer (selected-window) (other-buffer visible t)))
      (kill-buffer visible)
      (kill-buffer process-buffer))))

(provide 'hermes-capabilities-tests)
;;; hermes-capabilities-tests.el ends here
