;;; hermes-chat-attachments-tests.el --- Workspace attachment tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Public entries and real REST/RPC builders; only external wire effects stubbed.
;; Disposable local bytes and owned chats; no live gateway/workspace writes.

;;; Code:

(require 'hermes-test-helpers)
(require 'hermes-chat-attachments)
(require 'hermes-files)

(defmacro hermes-attachments-test--chat (&rest body)
  "Run BODY with an attached chat, local fixture and deferred wire effects."
  (declare (indent 0) (debug t))
  `(let ((file (make-temp-file "hermes-attachment-" nil ".txt"))
         (bytes (encode-coding-string "harmless café\n" 'utf-8-unix))
         http rpc op consent)
     (unwind-protect
         (progn
           (let ((coding-system-for-write 'no-conversion)) (write-region bytes nil file nil 'silent))
           (cl-letf (((symbol-function 'hermes-dashboard-transport--default-http-request-async)
                      (lambda (url &rest args)
                        (let ((promise (hermes--promise-make)))
                          (push (cons (plist-put args :url url) promise) http) promise)))
                     ((symbol-function 'websocket-close) #'ignore)
                     ((symbol-function 'yes-or-no-p) (lambda (prompt) (setq consent prompt) t)))
             (let ((hermes-dashboard-transport-websocket-send-function
                    (lambda (_socket text)
                      (push (json-parse-string text :object-type 'alist :array-type 'list) rpc))))
               (hermes-test-with-chat-buffer
                 (let ((client (hermes-test--dashboard-client)))
                   (setf (hermes-dashboard-transport-client-ready-p client) t
                         (hermes-dashboard-transport-client-base-url client) "http://attachments.invalid"
                         (hermes-dashboard-transport-client-token client) "disposable-token")
                   (setq hermes-chat--dashboard-client client
                         hermes-chat--dashboard-session-ready-p t
                         hermes-chat--dashboard-active-session-id "owner-runtime"
                         hermes-chat--session-id "owner-stored"
                         hermes-chat--working-directory "/workspace"
                         hermes-chat--resolved-start-mode 'remote)
                   (hermes-chat--replace-input-tail "literal draft")
                   ,@body
                   (hermes-chat-attachment-cancel)
                   (mapc (lambda (view) (when (buffer-live-p view) (kill-buffer view)))
                         hermes-chat-attachments--recoveries))))))
       (delete-file file))))

(defun hermes-attachments-test--rpc (client rpc result)
  "Deliver RESULT for the latest native RPC frame in RPC on CLIENT."
  (let ((pending (hermes-dashboard-transport--take-pending client (alist-get 'id (car rpc)))))
    (when pending (funcall (plist-get pending :resolve) result))))

(defun hermes-attachments-test--http (http body)
  "Deliver BODY through the latest real HTTP builder request in HTTP."
  (hermes--promise-resolve (cdar http) (list :status 200 :body body)))

(defun hermes-attachments-test--start (file)
  "Begin the public choose/upload command for FILE and return its owner."
  (hermes-chat-attach-file file)
  hermes-chat-attachments--operation)

(defun hermes-attachments-test--cwd-result ()
  "Return exact owning session activation evidence."
  '((session_id . "owner-runtime") (info . ((cwd . "/workspace")))))

(defun hermes-attachments-test--read-result (op &optional bytes)
  "Return a canonical managed-files envelope for OP and optional BYTES."
  (let ((payload (or bytes (plist-get op :bytes))))
    `((path . ,(plist-get op :path)) (size . ,(length payload))
      (mime_type . "text/plain")
      (data_url . ,(concat "data:text/plain;base64," (base64-encode-string payload t))))))

(defun hermes-attachments-test--ref (op)
  "Return the released safe relative reference for OP's generated filename."
  (concat "@file:" (file-name-nondirectory (plist-get op :path))))

(defun hermes-attachments-test--attached (op)
  "Return an exact native file.attach receipt for OP."
  `((attached . t) (uploaded . :false) (path . ,(plist-get op :path))
    (ref_text . ,(hermes-attachments-test--ref op))))

(ert-deftest hermes-chat-attachments-public-command ()
  (should (commandp 'hermes-chat-attach-file))
  (should (equal (command-modes 'hermes-chat-attach-file) '(hermes-chat-mode)))
  (should (eq (lookup-key hermes-chat-files-map (kbd "f")) #'hermes-chat-attach-file))
  (should (eq (lookup-key hermes-chat-images-map (kbd "f")) #'hermes-chat-attach-image-file)))

(ert-deftest hermes-chat-attachments-native-builders-success ()
  (hermes-attachments-test--chat
    (setq op (hermes-attachments-test--start file))
    (should (equal "session.activate" (alist-get 'method (car rpc))))
    (should (equal "owner-runtime" (alist-get 'session_id (alist-get 'params (car rpc)))))
    (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
    (should (string-match-p (regexp-quote "/api/files?path=%2Fworkspace") (plist-get (caar http) :url)))
    (hermes-attachments-test--http http '((path . "/workspace")))
    (should (string-match-p "GATEWAY-HOST" consent))
    (should (string-match-p "non-atomic" consent))
    (let ((body (json-parse-string (decode-coding-string (plist-get (caar http) :data) 'utf-8)
                                 :object-type 'alist :false-object :false)))
      (should (eq :false (alist-get 'overwrite body)))
      (should (equal (plist-get op :path) (alist-get 'path body)))
      (should (equal (concat "data:text/plain;base64," (base64-encode-string bytes t))
                     (alist-get 'data_url body))))
    (hermes-attachments-test--http http `((ok . t) (path . ,(plist-get op :path))))
    (should (eq 'readback (plist-get op :state)))
    (hermes-attachments-test--http http (hermes-attachments-test--read-result op))
    (should (equal "file.attach" (alist-get 'method (car rpc))))
    (should (equal "owner-runtime" (alist-get 'session_id (alist-get 'params (car rpc)))))
    (hermes-attachments-test--rpc client rpc (hermes-attachments-test--attached op))
    (should (equal (concat "literal draft\n" (hermes-attachments-test--ref op)) (hermes-chat-input-string)))
    (should-not hermes-chat-attachments--operation)
    (should (eq 'attached (plist-get op :state)))
    (with-current-buffer (plist-get op :recovery)
      (should (equal bytes hermes-files--bytes))
      (should (string-match-p "literal draft" (buffer-string)))
      (should (string-match-p (regexp-quote (hermes-attachments-test--ref op)) (buffer-string))))))

(ert-deftest hermes-chat-attachments-choose-owner-fence ()
  (hermes-attachments-test--chat
    (cl-letf (((symbol-function 'read-file-name)
               (lambda (&rest _) (setq hermes-chat--dashboard-active-session-id "successor") file)))
      (should-error (call-interactively #'hermes-chat-attach-file) :type 'user-error))
    (should-not rpc)
    (should-not http)
    (should (equal "literal draft" (hermes-chat-input-string)))))

(ert-deftest hermes-chat-attachments-consent-owner-fence ()
  (hermes-attachments-test--chat
    (setq op (hermes-attachments-test--start file))
    (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_) (setq hermes-chat--dashboard-active-session-id "successor") t)))
      (hermes-attachments-test--http http '((path . "/workspace"))))
    (should (= 1 (length http)))
    (should (plist-get op :retired))
    (should (equal bytes (buffer-local-value 'hermes-files--bytes (plist-get op :recovery))))))

(ert-deftest hermes-chat-attachments-newer-and-erased-draft-preserved ()
  (dolist (replacement '("new literal draft" "literal draft"))
    (hermes-attachments-test--chat
      (setq op (hermes-attachments-test--start file))
      (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
      (hermes-attachments-test--http http '((path . "/workspace")))
      (goto-char (point-max)) (insert "edit")
      (hermes-chat--replace-input-tail replacement)
      (hermes-attachments-test--http http `((ok . t) (path . ,(plist-get op :path))))
      (hermes-attachments-test--http http (hermes-attachments-test--read-result op))
      (hermes-attachments-test--rpc client rpc (hermes-attachments-test--attached op))
      (should (equal replacement (hermes-chat-input-string)))
      (should (equal bytes (buffer-local-value 'hermes-files--bytes (plist-get op :recovery))))
      (with-current-buffer (plist-get op :recovery)
        (should (string-match-p (regexp-quote (hermes-attachments-test--ref op)) (buffer-string)))))))

(ert-deftest hermes-chat-attachments-every-boundary-rejects-successors ()
  (dolist (boundary '(activation policy upload readback attach))
    (hermes-attachments-test--chat
      (setq op (hermes-attachments-test--start file))
      (unless (eq boundary 'activation)
        (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result)))
      (unless (memq boundary '(activation policy))
        (hermes-attachments-test--http http '((path . "/workspace"))))
      (unless (memq boundary '(activation policy upload))
        (hermes-attachments-test--http http `((ok . t) (path . ,(plist-get op :path)))))
      (when (eq boundary 'attach)
        (hermes-attachments-test--http http (hermes-attachments-test--read-result op)))
      (let ((before (cons (length rpc) (length http))))
        (setq hermes-chat--dashboard-active-session-id "successor")
        (hermes-chat--replace-input-tail "successor literal")
        (pcase boundary
          ('activation (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result)))
          ('policy (hermes-attachments-test--http http '((path . "/workspace"))))
          ('upload (hermes-attachments-test--http http `((ok . t) (path . ,(plist-get op :path)))))
          ('readback (hermes-attachments-test--http http (hermes-attachments-test--read-result op)))
          ('attach (hermes-attachments-test--rpc client rpc (hermes-attachments-test--attached op))))
        (should (equal before (cons (length rpc) (length http))))
        (should (equal "successor literal" (hermes-chat-input-string)))
        (should (equal bytes (buffer-local-value 'hermes-files--bytes (plist-get op :recovery))))))))

(ert-deftest hermes-chat-attachments-claim-lifetime-routing-connection-fences ()
  (dolist (change '(claim lifetime connection client endpoint profile cwd marker))
    (hermes-attachments-test--chat
      (setq op (hermes-attachments-test--start file))
      (pcase change
        ('claim (hermes-buffer--retire))
        ('lifetime (setq hermes-chat--lifecycle-generation (hermes-chat--next-lifetime-token)))
        ('connection (cl-incf (hermes-dashboard-transport-client-generation client)))
        ('client (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)))
        ('endpoint (setf (hermes-dashboard-transport-client-base-url client) "http://successor.invalid"))
        ('profile (setq hermes-chat--profile "successor"))
        ('cwd (setq hermes-chat--working-directory "/successor"))
        ('marker (setq hermes-chat--input-marker (copy-marker hermes-chat--input-marker))))
      (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
      (should-not http)
      (should (equal "literal draft" (hermes-chat-input-string))))))

(ert-deftest hermes-chat-attachments-invalid-remote-evidence ()
  (dolist (kind '(foreign-session relative-cwd foreign-directory upload-path bytes malformed-data ref path))
    (hermes-attachments-test--chat
      (setq op (hermes-attachments-test--start file))
      (hermes-attachments-test--rpc
       client rpc
       (pcase kind
         ('foreign-session '((session_id . "successor") (info . ((cwd . "/workspace")))))
         ('relative-cwd '((session_id . "owner-runtime") (info . ((cwd . "relative")))))
         (_ (hermes-attachments-test--cwd-result))))
      (unless (plist-get op :retired)
        (hermes-attachments-test--http http `((path . ,(if (eq kind 'foreign-directory) "/elsewhere" "/workspace")))))
      (unless (plist-get op :retired)
        (hermes-attachments-test--http http `((ok . t) (path . ,(if (eq kind 'upload-path) "/wrong" (plist-get op :path))))))
      (unless (plist-get op :retired)
        (hermes-attachments-test--http
         http (pcase kind
                ('bytes (hermes-attachments-test--read-result op "other bytes"))
                ('malformed-data `((path . ,(plist-get op :path)) (size . 3) (data_url . "data:text/plain;base64,!!!")))
                (_ (hermes-attachments-test--read-result op)))))
      (unless (plist-get op :retired)
        (hermes-attachments-test--rpc
         client rpc `((attached . t) (path . ,(if (eq kind 'path) "/wrong" (plist-get op :path)))
                      (ref_text . ,(if (eq kind 'ref) "@file:/outside" (hermes-attachments-test--ref op))))))
      (should (plist-get op :retired))
      (should (equal "literal draft" (hermes-chat-input-string)))
      (should (equal bytes (buffer-local-value 'hermes-files--bytes (plist-get op :recovery)))))))

(ert-deftest hermes-chat-attachments-read-failures-and-binary-limits ()
  (dolist (kind '(missing empty binary invalid-utf8 pdf oversize))
    (hermes-attachments-test--chat
      (pcase kind
        ('missing (delete-file file))
        ('empty (write-region "" nil file nil 'silent))
        ('binary (let ((coding-system-for-write 'no-conversion)) (write-region (unibyte-string 0 255) nil file nil 'silent)))
        ('invalid-utf8 (let ((coding-system-for-write 'no-conversion))
                         (write-region (unibyte-string 255) nil file nil 'silent)))
        ('pdf (let ((coding-system-for-write 'no-conversion)) (write-region "%PDF-1.4" nil file nil 'silent)))
        ('oversize (let ((coding-system-for-write 'no-conversion))
                     (write-region (make-string (+ 2 hermes-chat-attachments--max-bytes) ?a) nil file nil 'silent))))
      (should-error (hermes-chat-attach-file file))
      (should-not rpc) (should-not http)
      (should (equal "literal draft" (hermes-chat-input-string)))
      (unless (eq kind 'missing)
        (let ((retained (buffer-local-value 'hermes-files--bytes (car hermes-chat-attachments--recoveries))))
          (should (<= (length retained) (1+ hermes-chat-attachments--max-bytes)))))
      (unless (file-exists-p file) (write-region "" nil file nil 'silent)))))

(ert-deftest hermes-chat-attachments-uncertain-write-never-replayed ()
  (hermes-attachments-test--chat
    (setq op (hermes-attachments-test--start file))
    (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
    (hermes-attachments-test--http http '((path . "/workspace")))
    (let ((count (length http)))
      (hermes--promise-reject (cdar http) "lost receipt")
      (should (plist-get op :written))
      (should (eq 'failed-or-uncertain (plist-get op :state)))
      (should (= count (length http)))
      (should (= 1 (length rpc)))
      (should (equal bytes (buffer-local-value 'hermes-files--bytes (plist-get op :recovery)))))))

(ert-deftest hermes-chat-attachments-native-retirement-preserves-bytes ()
  (dolist (action '(cancel mode association detach stop))
    (hermes-attachments-test--chat
      (setq op (hermes-attachments-test--start file))
      (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
      (pcase action
        ('cancel (hermes-chat-attachment-cancel))
        ('mode (fundamental-mode))
        ((or 'association 'detach)
         (set-visited-file-name (concat file ".notes") t)
         (when (eq action 'detach) (set-visited-file-name nil t)))
        ('stop (hermes-dashboard-transport-stop client)))
      (hermes-attachments-test--http http '((path . "/workspace")))
      (should (= 1 (length http)))
      (should (plist-get op :retired))
      (should (equal bytes (buffer-local-value 'hermes-files--bytes (plist-get op :recovery)))))))

(ert-deftest hermes-chat-attachments-narrowing-and-recovery-save ()
  (hermes-attachments-test--chat
    (narrow-to-region hermes-chat--input-marker (point-max))
    (setq op (hermes-attachments-test--start file))
    (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
    (hermes-attachments-test--http http '((path . "/workspace")))
    (hermes-attachments-test--http http `((ok . t) (path . ,(plist-get op :path))))
    (hermes-attachments-test--http http (hermes-attachments-test--read-result op))
    (hermes-attachments-test--rpc client rpc (hermes-attachments-test--attached op))
    (should (buffer-narrowed-p))
    (let ((save (concat file ".saved.gz")))
      (unwind-protect
          (with-current-buffer (plist-get op :recovery)
            (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) save)))
              (call-interactively #'hermes-chat-attachment-save))
            (should (equal bytes (with-temp-buffer (set-buffer-multibyte nil)
                                      (let ((file-name-handler-alist nil)) (insert-file-contents-literally save))
                                      (buffer-string)))))
        (when (file-exists-p save) (delete-file save))))))

(ert-deftest hermes-chat-attachments-auth-wait-fences-write-and-readback ()
  (dolist (boundary '(upload readback))
    (hermes-attachments-test--chat
      (setq op (hermes-attachments-test--start file))
      (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
      (when (eq boundary 'readback)
        (hermes-attachments-test--http http '((path . "/workspace"))))
      (setf (hermes-dashboard-transport-client-token client) nil)
      (let ((auth (hermes--promise-make)) (before (length http)))
        (cl-letf (((symbol-function 'hermes-dashboard-transport-api-auth-async) (lambda () auth)))
          (if (eq boundary 'upload)
              (hermes-attachments-test--http http '((path . "/workspace")))
            (hermes-attachments-test--http http `((ok . t) (path . ,(plist-get op :path)))))
          (should (= before (length http)))
          (setq hermes-chat--dashboard-active-session-id "successor")
          (hermes--promise-resolve auth '(:base-url "http://attachments.invalid")))
        (should (= before (length http)))
        (should (equal "literal draft" (hermes-chat-input-string)))))))

(ert-deftest hermes-chat-attachments-native-insertion-hook-fence ()
  (hermes-attachments-test--chat
    (setq op (hermes-attachments-test--start file))
    (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
    (hermes-attachments-test--http http '((path . "/workspace")))
    (hermes-attachments-test--http http `((ok . t) (path . ,(plist-get op :path))))
    (hermes-attachments-test--http http (hermes-attachments-test--read-result op))
    (add-hook 'before-change-functions
              (lambda (&rest _) (setq hermes-chat--dashboard-active-session-id "successor")) nil t)
    (hermes-attachments-test--rpc client rpc (hermes-attachments-test--attached op))
    (should (equal "literal draft" (hermes-chat-input-string)))))

(ert-deftest hermes-chat-attachments-recovery-reader-fence ()
  (hermes-attachments-test--chat
    (setq op (hermes-attachments-test--start file))
    (hermes-chat-attachment-cancel)
    (let ((save (concat file ".saved")))
      (with-current-buffer (plist-get op :recovery)
        (cl-letf (((symbol-function 'read-file-name)
                   (lambda (&rest _)
                     (fundamental-mode) (setq buffer-read-only nil)
                     (goto-char (point-max)) (insert "literal notes") save)))
          (should-error (call-interactively #'hermes-chat-attachment-save) :type 'user-error))
        (should (string-suffix-p "literal notes" (buffer-string)))
        (should (equal bytes (plist-get hermes-chat-attachments--record :bytes))))
      (should-not (file-exists-p save)))))

(ert-deftest hermes-chat-attachments-retired-recovery-reopens-without-notes-mutation ()
  (hermes-attachments-test--chat
    (setq op (hermes-attachments-test--start file))
    (let ((old (plist-get op :recovery)))
      (with-current-buffer old
        (fundamental-mode) (setq buffer-read-only nil)
        (erase-buffer) (insert "literal notes"))
      (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
      (hermes-attachments-test--http http '((path . "/workspace")))
      (hermes-attachments-test--http http `((ok . t) (path . ,(plist-get op :path))))
      (hermes-attachments-test--http http (hermes-attachments-test--read-result op))
      (hermes-attachments-test--rpc client rpc (hermes-attachments-test--attached op))
      (should (equal "literal notes" (with-current-buffer old (buffer-string))))
      (let ((before (cons (length rpc) (length http))))
        (call-interactively #'hermes-chat-attachment-recovery)
        (should (equal before (cons (length rpc) (length http)))))
      (should-not (eq old (plist-get op :recovery)))
      (with-current-buffer (plist-get op :recovery)
        (should (equal bytes hermes-files--bytes))
        (should (string-match-p "literal draft" (buffer-string)))
        (should (string-match-p (regexp-quote (hermes-attachments-test--ref op)) (buffer-string)))))))

(ert-deftest hermes-chat-attachments-recovery-popup-save-fences-and-control ()
  (dolist (retire '(nil t))
    (hermes-attachments-test--chat
      (setq op (hermes-attachments-test--start file))
      (hermes-chat-attachment-cancel)
      (let ((save (concat file ".popup-save")) notes prompt)
        (unwind-protect
            (with-current-buffer (plist-get op :recovery)
              (switch-to-buffer (current-buffer))
              (cl-letf (((symbol-function 'read-file-name)
                         (lambda (label &rest _)
                           (setq prompt label)
                           (when retire
                             (set-visited-file-name (concat file ".notes") t)
                             (set-visited-file-name nil t)
                             (setq buffer-read-only nil)
                             (goto-char (point-max)) (insert "literal successor notes")
                             (setq notes (buffer-string)))
                           save)))
                (if retire
                    (should-error (execute-kbd-macro (kbd "? s")) :type 'user-error)
                  (execute-kbd-macro (kbd "? s"))))
              (should (string-match-p "complete bytes" prompt))
              (if retire
                  (progn (should-not (file-exists-p save))
                         (should (equal notes (buffer-string))))
                (should (equal bytes (with-temp-buffer (set-buffer-multibyte nil)
                                      (insert-file-contents-literally save) (buffer-string))))))
          (keymap-popup-dismiss)
          (when (file-exists-p save) (delete-file save)))))))

(ert-deftest hermes-chat-attachments-recovery-hook-cannot-remove-successor-edit-fence ()
  (hermes-attachments-test--chat
    (setq op (hermes-attachments-test--start file))
    (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
    (hermes-attachments-test--http http '((path . "/workspace")))
    (hermes-attachments-test--http http `((ok . t) (path . ,(plist-get op :path))))
    (hermes-attachments-test--http http (hermes-attachments-test--read-result op))
    (let ((chat (current-buffer)) (old op) successor)
      (with-current-buffer (plist-get old :recovery)
        (add-hook 'before-change-functions
                  (lambda (&rest _)
                    (with-current-buffer chat
                      (hermes-chat-attachment-cancel)
                      (hermes-chat-attach-file file)
                      (setq successor hermes-chat-attachments--operation))) nil t))
      (hermes-attachments-test--rpc client rpc (hermes-attachments-test--attached old))
      (should (eq successor hermes-chat-attachments--operation))
      (should (memq #'hermes-chat-attachments--edited before-change-functions))
      (goto-char (point-max)) (insert "new edit")
      (hermes-chat--replace-input-tail "literal draft")
      (should (plist-get successor :edited))
      (hermes-attachments-test--rpc client rpc (hermes-attachments-test--cwd-result))
      (hermes-attachments-test--http http '((path . "/workspace")))
      (hermes-attachments-test--http http `((ok . t) (path . ,(plist-get successor :path))))
      (hermes-attachments-test--http http (hermes-attachments-test--read-result successor))
      (hermes-attachments-test--rpc client rpc (hermes-attachments-test--attached successor))
      (should (equal "literal draft" (hermes-chat-input-string)))
      (with-current-buffer (plist-get successor :recovery)
        (should (string-match-p (regexp-quote (hermes-attachments-test--ref successor)) (buffer-string)))))))

(ert-deftest hermes-chat-attachments-prefix-and-exact-limit-recovery ()
  (dolist (oversize '(nil t))
    (hermes-attachments-test--chat
      (let* ((payload (make-string (+ hermes-chat-attachments--max-bytes (if oversize 4 0)) ?a))
             (prefix (substring payload 0 (+ hermes-chat-attachments--max-bytes (if oversize 1 0))))
             (save (concat file ".prefix-save")) prompt)
        (let ((coding-system-for-write 'no-conversion)) (write-region payload nil file nil 'silent))
        (if oversize (should-error (hermes-chat-attach-file file) :type 'user-error)
          (setq op (hermes-attachments-test--start file)) (hermes-chat-attachment-cancel))
        (when oversize (should-not rpc) (should-not http))
        (call-interactively #'hermes-chat-attachment-recovery)
        (unwind-protect
            (with-current-buffer (car hermes-chat-attachments--recoveries)
              (should (eq oversize (plist-get hermes-chat-attachments--record :prefix)))
              (should (equal prefix hermes-files--bytes))
              (should (string-match-p (if oversize "INCOMPLETE bounded prefix" "complete bytes retained")
                                      (concat header-line-format (buffer-string))))
              (cl-letf (((symbol-function 'read-file-name)
                         (lambda (label &rest _) (setq prompt label) save)))
                (call-interactively #'hermes-chat-attachment-save))
              (should (string-match-p (if oversize "INCOMPLETE bounded prefix" "complete bytes retained") prompt))
              (should (equal prefix (with-temp-buffer (set-buffer-multibyte nil)
                                      (insert-file-contents-literally save) (buffer-string)))))
          (when (file-exists-p save) (delete-file save)))))))

(ert-deftest hermes-chat-attachments-mode-initialization-cannot-adopt-successor-claim ()
  (hermes-attachments-test--chat
    (let (view successor)
      (let ((hermes-file-view-mode-hook
             (list (lambda ()
                     (setq view (current-buffer))
                     (let ((hermes-file-view-mode-hook nil))
                       (fundamental-mode) (hermes-file-view-mode))
                     (hermes-buffer--claim 'hermes-file-view-mode)
                     (setq successor hermes-buffer--owner
                           header-line-format "Successor viewer")))))
        (should-error (hermes-chat-attach-file file) :type 'user-error))
      (should-not rpc) (should-not http)
      (with-current-buffer view
        (should (eq successor hermes-buffer--owner))
        (should (equal "Successor viewer" header-line-format))
        (should (string-empty-p (buffer-string)))
        (should (equal bytes (plist-get hermes-chat-attachments--record :bytes)))))))

(provide 'hermes-chat-attachments-tests)
;;; hermes-chat-attachments-tests.el ends here
