;;; hermes-chat-fences-tests.el --- Chat topic tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'delsel)
(require 'hermes-test-helpers)

(define-minor-mode hermes-test-fence-minor-mode
  "Detect accidental minor mode dispatch from settled entries.")

(ert-deftest hermes-chat-settled-fences-reject-nonlanguage-modes ()
  (require 'autorevert)
  (let ((before global-auto-revert-mode)
        calls)
    (unwind-protect
        (progn
          (global-auto-revert-mode -1)
          (dolist (native '(t nil))
            (dolist (language '("global-auto-revert" "hermes-test-fence-minor"))
              (hermes-test-with-chat-buffer
               (let ((markdown-fontify-code-blocks-natively native)
                     (global-auto-revert-mode-hook
                      (list (lambda () (push 'global calls))))
                     (hermes-test-fence-minor-mode-hook
                      (list (lambda () (push 'minor calls)))))
                 (hermes-chat--insert-entry
                  (hermes-chat--make-entry
                   'assistant (concat "```" language "\nplain code\n```\n") 'done))
                 (should-not global-auto-revert-mode)
                 (should-not calls)
                 (goto-char (point-min))
                 (should (search-forward "plain code" nil t)))))))
      (global-auto-revert-mode (if before 1 -1)))))

(ert-deftest hermes-chat-settled-fences-reject-nested-markup ()
  (require 'org)
  (let ((markdown-fontify-code-blocks-natively t)
        (org-src-fontify-natively t)
        calls)
    (dolist (text '("```org\n#+begin_src emacs-lisp\n(defun sample ())\n#+end_src\n```\n"
                    "~~~markdown\n```elisp\n(defun sample ())\n```\n~~~\n"))
      (hermes-test-with-chat-buffer
       (let ((emacs-lisp-mode-hook (list (lambda () (push 'elisp calls))))
             (org-mode-hook (list (lambda () (push 'org calls))))
             (markdown-mode-hook (list (lambda () (push 'markdown calls))))
             (buffers (buffer-list)))
         (hermes-chat--insert-entry
          (hermes-chat--make-entry 'assistant text 'done))
         (should-not calls)
         (should (equal buffers (buffer-list)))
         (goto-char (point-min))
         (search-forward "defun")
         (should (equal (get-text-property (- (point) 5) 'face)
                        '(markdown-pre-face markdown-code-face))))))))

(ert-deftest hermes-chat-settled-fences-delay-hooks-through-fontification ()
  (hermes-test-with-chat-buffer
   (let ((markdown-fontify-code-blocks-natively t)
         (ensure (symbol-function 'font-lock-ensure))
         calls)
     (let ((emacs-lisp-mode-hook (list (lambda () (push major-mode calls)))))
       (cl-letf (((symbol-function 'font-lock-ensure)
                  (lambda (&rest args)
                    ;; Mode hooks must stay delayed beyond initialization,
                    ;; both in the Markdown buffer and its language helper.
                    (run-mode-hooks 'emacs-lisp-mode-hook)
                    (apply ensure args))))
         (hermes-chat--insert-entry
          (hermes-chat--make-entry 'assistant "```elisp\n(defun sample ())\n```" 'done)))
       (should-not calls)
       (goto-char (point-min))
       (search-forward "defun")
       (should (equal (get-text-property (- (point) 5) 'face)
                      '(font-lock-keyword-face markdown-code-face)))))))

(ert-deftest hermes-chat-settled-fences-preserve-language-faces-and-literals ()
  (dolist (native '(t nil))
    (dolist (fence '("```" "~~~"))
      (hermes-test-with-chat-buffer
       (insert "DRAFT α")
       (let* ((markdown-fontify-code-blocks-natively native)
              (emacs-lisp-mode-hook (list (lambda () (ert-fail "Elisp hook ran"))))
              (text (concat fence "elisp\n(defun sample (arg)\n"
                            "  (message \"α\" arg))\n\n" fence "\n"))
              (undo buffer-undo-list)
              (node (hermes-chat--insert-entry
                     (hermes-chat--make-entry 'assistant text 'done))))
         (should (equal (plist-get (ewoc-data node) :content) text))
         (should (equal (hermes-chat-input-string) "DRAFT α"))
         (should (eq undo buffer-undo-list))
         (goto-char (point-min))
         (search-forward fence)
         (let ((start (- (point) (length fence))))
           (should (equal (filter-buffer-substring start (+ start (length text))) text))
           (when native
             (search-forward "defun")
             (should (equal (get-text-property (- (point) 5) 'face)
                            '(font-lock-keyword-face markdown-code-face)))
             (search-forward "  (message")
             (should (equal (get-text-property (- (point) 10) 'face)
                            '(markdown-code-face))))))))))

(ert-deftest hermes-chat-settled-unlabelled-fences-preserve-native-default ()
  "Safe defaults preserve native faces, literal copying, and the draft."
  (let ((buffers (buffer-list)))
    (unwind-protect
        (dolist (native '(t nil))
          (dolist (default '(emacs-lisp-mode nil))
            (dolist (fence '("```" "~~~"))
              (let* ((markdown-fontify-code-blocks-natively native)
                     (markdown-fontify-code-block-default-mode default)
                     (text (concat fence "\n(defun sample (arg)\n"
                                   "  (message \"α\" arg))\n\n" fence "\n"))
                     (expected (with-temp-buffer
                                 (insert text)
                                 (markdown-mode)
                                 (font-lock-ensure)
                                 (buffer-string))))
                (hermes-test-with-chat-buffer
                 (insert "DRAFT α")
                 (let* ((undo buffer-undo-list)
                        (owned (buffer-list))
                        (emacs-lisp-mode-hook
                         (list (lambda () (ert-fail "Default mode hook ran"))))
                        (node (hermes-chat--insert-entry
                               (hermes-chat--make-entry 'assistant text 'done))))
                   (should (equal (plist-get (ewoc-data node) :content) text))
                   (should (equal (hermes-chat-input-string) "DRAFT α"))
                   (should (eq undo buffer-undo-list))
                   (should (equal owned (buffer-list)))
                   (goto-char (point-min))
                   (search-forward fence)
                   (let ((start (- (point) (length fence))))
                     (should (equal (filter-buffer-substring
                                     start (+ start (length text))) text))
                     (dotimes (i (length text))
                       (should (equal (get-text-property (+ start i) 'face)
                                      (get-text-property i 'face expected)))))))))))
      ;; Only the native reference is allowed to create shared helpers.
      (dolist (buffer (seq-difference (buffer-list) buffers))
        (kill-buffer buffer)))))

(ert-deftest hermes-chat-settled-unlabelled-fences-reject-unsafe-defaults ()
  (require 'autorevert)
  (require 'org)
  (let ((before global-auto-revert-mode) calls)
    (unwind-protect
        (progn
          (global-auto-revert-mode -1)
          (dolist (default '(global-auto-revert-mode hermes-test-fence-minor-mode
                            org-mode markdown-mode))
            (hermes-test-with-chat-buffer
             (let ((markdown-fontify-code-blocks-natively t)
                   (markdown-fontify-code-block-default-mode default)
                   (global-auto-revert-mode-hook
                    (list (lambda () (push 'global calls))))
                   (hermes-test-fence-minor-mode-hook
                    (list (lambda () (push 'minor calls))))
                   (org-mode-hook (list (lambda () (push 'org calls))))
                   (markdown-mode-hook (list (lambda () (push 'markdown calls))))
                   (emacs-lisp-mode-hook (list (lambda () (push 'elisp calls))))
                   (buffers (buffer-list)))
               (hermes-chat--insert-entry
                (hermes-chat--make-entry
                 'assistant "~~~\n#+begin_src emacs-lisp\n(defun sample ())\n#+end_src\n~~~\n"
                 'done))
               (should-not global-auto-revert-mode)
               (should-not calls)
               (should (equal buffers (buffer-list)))
               (goto-char (point-min))
               (search-forward "defun")
               (should (equal (get-text-property (- (point) 5) 'face)
                              '(markdown-pre-face markdown-code-face)))))))
      (global-auto-revert-mode (if before 1 -1)))))

(ert-deftest hermes-chat-settled-unlabelled-native-off-does-not-resolve-default ()
  (hermes-test-with-chat-buffer
   (let ((markdown-fontify-code-blocks-natively nil)
         (markdown-fontify-code-block-default-mode 'emacs-lisp-mode)
         calls)
     (cl-letf (((symbol-function 'hermes-chat--language-mode)
                (lambda (&rest _) (push 'resolve calls) 'emacs-lisp-mode)))
       (hermes-chat--insert-entry
        (hermes-chat--make-entry 'assistant "```\n(defun sample ())\n```\n" 'done)))
     (should-not calls)
     (goto-char (point-min))
     (should (search-forward "(defun sample ())" nil t)))))

(ert-deftest hermes-chat-settled-diff-examples-stay-literal-in-outer-fences ()
  "Outer code owns nested diff syntax, including unfinished and indented code."
  (dolist (delimiters '(("````markdown" . "````")
                        ("~~~~markdown" . "~~~~")
                        ("  `````markdown" . "  ``````\t")
                        ("````" . "````")
                        ("````markdown" . "")
                        ("~~~~markdown" . "")))
    (dolist (inner '("```diff" "~~~patch" ""))
      (let ((text (concat (car delimiters) "\n"
                          (unless (string-empty-p inner) (concat inner "\n"))
                          "@@ -1 +1 @@\n-old α\n+new β\n"
                          (unless (string-empty-p inner)
                            (concat (substring inner 0 3) "\n"))
                          (cdr delimiters) "\n")))
        (hermes-test-with-chat-buffer
         (insert "DRAFT α")
         (let* ((undo buffer-undo-list)
                (node (hermes-chat--insert-entry
                       (hermes-chat--make-entry 'assistant text 'done))))
           (should (equal (plist-get (ewoc-data node) :content) text))
           (should (equal (hermes-chat-input-string) "DRAFT α"))
           (should (eq undo buffer-undo-list))
           (should (zerop (hermes-test--count-buttons-labeled "View Diff")))
           (goto-char (point-min))
           (search-forward (car delimiters))
           (let ((start (- (point) (length (car delimiters)))))
             (should (equal (filter-buffer-substring
                             start (+ start (length text))) text)))))))))

(ert-deftest hermes-chat-settled-top-level-diffs-survive-outer-fence-skipping ()
  "After opaque code, each top-level diff discloses once; prose stays literal."
  (dolist (delimiters '(("```diff" . "```") ("~~~~patch" . "~~~~~\t")
                        ("  ````diff" . "  ````") ("```patch" . "")))
    (let* ((example "````markdown\n```diff\n-old example\n+new example\n```\n````\n")
           (prose "- ordinary minus\n+ ordinary plus\n")
           (inline "@@ -1 +1 @@\n-old inline\n+new inline\n")
           (fenced "-old fenced\n+new fenced\n")
           (text (concat example prose inline (car delimiters) "\n"
                         fenced (cdr delimiters) "\n")))
      (hermes-test-with-chat-buffer
       (hermes-chat--insert-entry (hermes-chat--make-entry 'assistant text 'done))
       (should (= 2 (hermes-test--count-buttons-labeled "View Diff")))
       (goto-char (point-min))
       (should (search-forward (concat example prose) nil t))
       (should-not (search-forward "old inline" nil t))
       (should-not (search-forward "old fenced" nil t))
       (should (equal (hermes-test--view-diff-content) inline))
       (should (equal (mapcar (lambda (block) (nth 2 block))
                              (hermes-chat--diff-blocks text))
                      (list inline (if (string-empty-p (cdr delimiters))
                                       (concat fenced "\n") fenced))))))))

(ert-deftest hermes-chat-settled-incomplete-diff-header-does-not-skip-fence ()
  "A failed hunk probe must leave the next outer fence available to scan."
  (let ((text "--- incomplete header\n````markdown\n```diff\n-old\n+new\n```\n````\n"))
    (hermes-test-with-chat-buffer
     (hermes-chat--insert-entry (hermes-chat--make-entry 'assistant text 'done))
     (should (zerop (hermes-test--count-buttons-labeled "View Diff")))
     (goto-char (point-min))
     (should (search-forward text nil t)))))

(provide 'hermes-chat-fences-tests)
;;; hermes-chat-fences-tests.el ends here
