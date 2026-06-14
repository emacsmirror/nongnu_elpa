;;; hermes-tests.el --- Tests for hermes-el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" root)))

;; The project targets Emacs 29.1+ through keymap-popup.  CI/developer
;; machines should use that.  These tiny shims let the tests still exercise the
;; pure buffer logic on Debian's Emacs 28 when no newer Emacs is available.
(eval-and-compile
  (unless (fboundp 'keymap-set)
    (defun keymap-set (keymap key definition)
      (define-key keymap (kbd key) definition)))
  (unless (fboundp 'keymap-lookup)
    (defun keymap-lookup (keymap key &optional accept-default)
      (lookup-key keymap (kbd key) accept-default)))
  (unless (fboundp 'defvar-keymap)
    (defmacro defvar-keymap (name &rest args)
      (declare (indent 1))
      (let (doc parent bindings)
        (while (keywordp (car args))
          (pcase (pop args)
            (:doc (setq doc (pop args)))
            (:parent (setq parent (pop args)))
            (_ (pop args))))
        (while args
          (let ((key (pop args))
                (definition (pop args)))
            (push `(define-key map (kbd ,key) ,definition) bindings)))
        `(defvar ,name
           (let ((map (make-sparse-keymap)))
             ,@(when parent `((set-keymap-parent map ,parent)))
             ,@(nreverse bindings)
             map)
           ,doc)))))

(require 'keymap-popup)
(require 'hermes)
(require 'hermes-chat)
(require 'hermes-transport)

(defun hermes-test--chat-buffer-name ()
  "Return a fresh chat buffer name for tests."
  (generate-new-buffer-name "*Hermes Chat Test*"))

(defmacro hermes-test-with-chat-buffer (&rest body)
  "Create a fresh Hermes chat buffer and run BODY in it."
  (declare (indent 0) (debug t))
  `(let ((hermes-chat-buffer-name (hermes-test--chat-buffer-name)))
     (unwind-protect
         (progn
           (hermes-chat)
           (with-current-buffer hermes-chat-buffer-name
             ,@body))
       (when-let* ((buffer (get-buffer hermes-chat-buffer-name)))
         (kill-buffer buffer)))))

(ert-deftest hermes-dashboard-opens-special-mode-buffer-and-popup ()
  (let (shown-map)
    (cl-letf (((symbol-function 'keymap-popup)
               (lambda (keymap) (setq shown-map keymap))))
      (unwind-protect
          (progn
            (hermes)
            (should (eq major-mode 'hermes-dashboard-mode))
            (should (eq shown-map hermes-dashboard-mode-map))
            (should (string-match-p "Hermes" (buffer-string))))
        (when-let* ((buffer (get-buffer hermes-dashboard-buffer-name)))
          (kill-buffer buffer))))))

(ert-deftest hermes-dashboard-chat-action-is-keymap-popup-binding ()
  (should (eq (keymap-lookup hermes-dashboard-mode-map "c") #'hermes-chat))
  (let* ((rows (keymap-popup--meta hermes-dashboard-mode-map 'descriptions))
         (entries (mapcan (lambda (row)
                            (mapcan (lambda (group)
                                      (plist-get group :entries))
                                    row))
                          rows)))
    (should (cl-find "c" entries :key (lambda (entry) (plist-get entry :key))
                     :test #'equal))))

(ert-deftest hermes-chat-opens-ewoc-buffer-with-writable-input-tail ()
  (hermes-test-with-chat-buffer
    (should (eq major-mode 'hermes-chat-mode))
    (should hermes-chat--ewoc)
    (should (markerp hermes-chat--input-marker))
    (should (= (marker-position hermes-chat--input-marker) (point-max)))
    (goto-char (point-min))
    (should-error (insert "not writable"))
    (goto-char hermes-chat--input-marker)
    (insert "draft")
    (should (equal (hermes-chat-input-string) "draft"))))

(ert-deftest hermes-chat-mode-map-sends-and-inserts-newlines ()
  (should (eq (keymap-lookup hermes-chat-mode-map "RET") #'hermes-chat-send))
  (should (eq (keymap-lookup hermes-chat-mode-map "C-j") #'hermes-chat-newline))
  (should (eq (keymap-lookup hermes-chat-mode-map "S-<return>") #'hermes-chat-newline)))

(ert-deftest hermes-chat-send-uses-transport-and-creates-pending-assistant ()
  (let (sent callback)
    (hermes-test-with-chat-buffer
      (let ((hermes-transport-send-function
             (lambda (prompt cb)
               (setq sent prompt
                     callback cb)
               'fake-process)))
        (insert "hello Hermes")
        (hermes-chat-send)
        (should (equal sent "hello Hermes"))
        (should (functionp callback))
        (should (equal (hermes-chat-input-string) ""))
        (pcase-let ((`(,user ,assistant) (hermes-chat--entries)))
          (should (equal (plist-get user :role) 'user))
          (should (equal (plist-get user :content) "hello Hermes"))
          (should (equal (plist-get assistant :role) 'assistant))
          (should (equal (plist-get assistant :status) 'pending))
          (should (equal (plist-get assistant :content) "")))))))

(ert-deftest hermes-chat-transport-updates-preserve-draft-input ()
  (let (callback)
    (hermes-test-with-chat-buffer
      (let ((hermes-transport-send-function
             (lambda (_prompt cb)
               (setq callback cb)
               'fake-process)))
        (insert "hi")
        (hermes-chat-send)
        (funcall callback '(:type delta :content "hello"))
        (insert "draft survives")
        (funcall callback '(:type delta :content " there"))
        (should (equal (hermes-chat-input-string) "draft survives"))
        (let ((assistant (cadr (hermes-chat--entries))))
          (should (equal (plist-get assistant :status) 'streaming))
          (should (equal (plist-get assistant :content) "hello there")))
        (funcall callback '(:type done))
        (let ((assistant (cadr (hermes-chat--entries))))
          (should (equal (plist-get assistant :status) 'done))
          (should-not hermes-chat--pending-assistant-id))))))

(ert-deftest hermes-chat-transport-removes-control-bytes-from-assistant-output ()
  (let (callback)
    (hermes-test-with-chat-buffer
      (let ((hermes-transport-send-function
             (lambda (_prompt cb)
               (setq callback cb)
               'fake-process)))
        (insert "hi")
        (hermes-chat-send)
        (funcall callback (list :type 'delta
                                :content (concat "\r\0hello"
                                                 (string #x85)
                                                 "\nλ\r")))
        (let ((assistant (cadr (hermes-chat--entries))))
          (should (equal (plist-get assistant :content) "hello\nλ"))
          (should-not (string-match-p "\r" (buffer-string))))))))

(ert-deftest hermes-chat-fontifies-inline-diff-with-diff-mode-faces ()
  (hermes-test-with-chat-buffer
    (hermes-chat--insert-entry
     (hermes-chat--make-entry 'assistant "Changed:\n- old\n+ new\nDone." 'done))
    (goto-char (point-min))
    (search-forward "- old")
    (should (eq (get-text-property (1- (point)) 'face) 'diff-removed))
    (search-forward "+ new")
    (should (eq (get-text-property (1- (point)) 'face) 'diff-added))))

(ert-deftest hermes-chat-rejects-concurrent-send-in-same-buffer ()
  (hermes-test-with-chat-buffer
    (let ((hermes-transport-send-function (lambda (_prompt _cb) 'fake-process)))
      (insert "first")
      (hermes-chat-send)
      (insert "second")
      (should-error (hermes-chat-send) :type 'user-error))))

(ert-deftest hermes-transport-builds-quiet-chat-command ()
  (let ((hermes-command "hermes"))
    (should (equal (hermes-transport--command "hello")
                   '("hermes" "chat" "-Q" "-q" "hello")))))

(provide 'hermes-tests)
;;; hermes-tests.el ends here
