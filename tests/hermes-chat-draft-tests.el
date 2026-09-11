;;; hermes-chat-draft-tests.el --- Draft highlighting tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'python)
(require 'hermes-test-helpers)

(defvar ruby-mode-hook)

(defun hermes-test-draft-flush ()
  "Deliver the current request without depending on idle timer timing."
  (should (timerp hermes-chat-draft--timer))
  (let ((timer hermes-chat-draft--timer))
    (cancel-timer timer)
    (apply (timer--function timer) (timer--args timer))))

(defun hermes-test-draft-face (word)
  "Return the displayed face at WORD in the draft."
  (save-excursion
    (goto-char hermes-chat--input-marker)
    (search-forward word)
    (get-char-property (- (point) (length word)) 'face)))

(ert-deftest hermes-chat-draft-real-edit-is-literal-and-undo-neutral ()
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry '(:id "reply" :role assistant :content "**Reply**"))
   (let ((transcript (buffer-substring (point-min) hermes-chat--input-marker))
         (draft "# Heading α\n**bold** and `inline`\n```elisp\n(defun foo () \"literal\")\n```\n")
         (buffers (buffer-list))
         (markdown-mode-hook (list (lambda () (ert-fail "Markdown hook ran")))))
     (insert draft)
     (undo-boundary)
     (let ((undo buffer-undo-list) (position (point))
           (modified (buffer-modified-p)) (tick (buffer-chars-modified-tick))
           (emacs-lisp-mode-hook (list (lambda () (ert-fail "Elisp hook ran")))))
       (hermes-test-draft-flush)
       (should (eq major-mode 'hermes-chat-mode))
       (should (eq undo buffer-undo-list))
       (should (= position (point)))
       (should (eq modified (buffer-modified-p)))
       (should (= tick (buffer-chars-modified-tick)))
       (should (equal (hermes-chat-input-string) draft))
       (should (equal (filter-buffer-substring hermes-chat--input-marker (point-max)) draft))
       (should (equal-including-properties transcript
                                           (buffer-substring (point-min) hermes-chat--input-marker)))
       (should (eq (hermes-test-draft-face "Heading") 'markdown-header-face-1))
       (should (equal (hermes-test-draft-face "defun") '(font-lock-keyword-face markdown-code-face)))
       (should (equal (hermes-test-draft-face "\"literal\"") '(font-lock-doc-face markdown-code-face)))
       (should (equal-including-properties
                (buffer-substring hermes-chat--input-marker (point-max)) draft))
       (should (equal buffers (buffer-list)))
       (dolist (overlay hermes-chat-draft--overlays)
         (should (>= (overlay-start overlay) hermes-chat--input-marker))
         (should (equal (overlay-properties overlay)
                        (list 'face (overlay-get overlay 'face))))))
     (let ((undo-in-region nil)) (undo 1))
     (should (equal (hermes-chat-input-string) "")))))

(ert-deftest hermes-chat-draft-closed-and-unfinished-fences ()
  (dolist (fence '("```" "~~~"))
    (dolist (closing '(nil t))
      (hermes-test-with-chat-buffer
       (insert (concat fence "python\ndef sample():\n    return \"α\""
                       (and closing (concat "\n" fence "\nplain"))))
       (let ((python-mode-hook (list (lambda () (ert-fail "Python hook ran"))))
             (prog-mode-hook (list (lambda () (ert-fail "Prog hook ran")))))
         (hermes-test-draft-flush))
       (should (equal (hermes-test-draft-face "def") '(font-lock-keyword-face markdown-code-face)))
       (should (equal (hermes-test-draft-face "\"α\"") '(font-lock-string-face markdown-code-face)))
       (when closing (should-not (hermes-test-draft-face "plain")))
       ;; Changing the opener to an unknown language must discard old native faces.
       (goto-char hermes-chat--input-marker)
       (search-forward "python")
       (replace-match "unknown-draft-language" t t)
       (hermes-test-draft-flush)
       (should (equal (hermes-test-draft-face "def") '(markdown-pre-face markdown-code-face)))))))

(define-minor-mode hermes-test-draft-minor-mode
  "Detect accidental minor mode dispatch in draft fences.")

(ert-deftest hermes-chat-draft-rejects-nonlanguage-modes ()
  (require 'autorevert)
  (let* ((global-before global-auto-revert-mode)
         (minor-hook-ran nil)
         (action-ran nil)
         (hermes-test-draft-minor-mode-hook
          (list (lambda () (setq minor-hook-ran t)))))
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-test-draft-action-mode)
                   (lambda () (setq action-ran t))))
          (dolist (language '("global-auto-revert" "hermes-test-draft-minor"
                              "hermes-test-draft-action" "unknown-draft-language"))
            (hermes-test-with-chat-buffer
             (insert (concat "```" language "\nplain code\n```"))
             (hermes-test-draft-flush)
             (should (eq global-auto-revert-mode global-before))
             (should-not minor-hook-ran)
             (should-not action-ran)
             (should (equal (hermes-test-draft-face "plain code") '(markdown-pre-face markdown-code-face))))))
      ;; Keep even a failing regression isolated from the rest of the suite.
      (unless (eq global-auto-revert-mode global-before)
        (global-auto-revert-mode (if global-before 1 -1))))))

(ert-deftest hermes-chat-draft-nested-markup-falls-back-without-hooks ()
  (require 'org)
  (let* ((hook-ran nil)
         (emacs-lisp-mode-hook (list (lambda () (setq hook-ran t))))
         (org-src-fontify-natively t)
         (markdown-fontify-code-blocks-natively t))
    (dolist (draft '("```org\n#+begin_src emacs-lisp\n(defun sample ())\n#+end_src\n```"
                     "~~~markdown\n```elisp\n(defun sample ())\n```\n~~~"))
      (hermes-test-with-chat-buffer
       (insert draft)
       (let ((buffers (buffer-list)))
         (hermes-test-draft-flush)
         (should-not hook-ran)
         (should (equal buffers (buffer-list)))
         (should (equal (hermes-test-draft-face "defun") '(markdown-pre-face markdown-code-face))))))))

(ert-deftest hermes-chat-draft-autoloads-language-without-running-hooks ()
  ;; This suite has not loaded Ruby: suitability must not require a prior visit.
  (should (autoloadp (symbol-function 'ruby-mode)))
  (hermes-test-with-chat-buffer
   (insert "~~~ruby\ndef sample\n  \"literal\"\nend")
   (let ((ruby-mode-hook (list (lambda () (ert-fail "Ruby hook ran")))))
     (hermes-test-draft-flush))
   (should-not (autoloadp (symbol-function 'ruby-mode)))
   (should (equal (hermes-test-draft-face "def") '(font-lock-keyword-face markdown-code-face)))
   (should (equal (hermes-test-draft-face "\"literal\"") '(font-lock-string-face markdown-code-face)))))

(ert-deftest hermes-chat-draft-disconnect-preserves-local-highlighting ()
  (dolist (pending '(nil t))
    (hermes-test-with-chat-buffer
     (insert "# draft")
     (unless pending (hermes-test-draft-flush))
     (let ((timer hermes-chat-draft--timer)
           (overlays hermes-chat-draft--overlays))
       (setq hermes-chat--dashboard-active-session-id "draft-test-session")
       (hermes-chat-disconnect)
       (should (equal (hermes-chat-input-string) "# draft"))
       (should (eq timer hermes-chat-draft--timer))
       (should (eq overlays hermes-chat-draft--overlays))
       (when pending (hermes-test-draft-flush))
       (should (eq (hermes-test-draft-face "draft") 'markdown-header-face-1)))
     (insert "\n**still editable**")
     (hermes-test-draft-flush)
     (should (hermes-test-draft-face "still editable")))))

(ert-deftest hermes-chat-draft-stale-edit-reset-mode-and-kill ()
  (hermes-test-with-chat-buffer
   (insert "**old**")
   (let* ((buffer (current-buffer))
          (timer hermes-chat-draft--timer)
          (callback (timer--function timer))
          (args (timer--args timer)))
     (insert " **new**")
     (let ((successor hermes-chat-draft--timer))
       (apply callback args)
       (should (eq successor hermes-chat-draft--timer))
       (should-not hermes-chat-draft--overlays))
     (hermes-test-draft-flush)
     (should hermes-chat-draft--overlays)
     (hermes-chat--reset-transcript)
     (apply callback args)
     (should-not hermes-chat-draft--overlays)
     (should (equal (hermes-chat-input-string) ""))
     (insert "# reset")
     (hermes-test-draft-flush)
     (should (eq (hermes-test-draft-face "reset") 'markdown-header-face-1))
     (let ((overlays hermes-chat-draft--overlays))
       (fundamental-mode)
       (should-not (cl-some #'overlay-buffer overlays)))
     (apply callback args)
     (should-not hermes-chat-draft--overlays)
     (kill-buffer buffer)
     (apply callback args))))

(ert-deftest hermes-chat-draft-current-work-retires-on-editor-teardown ()
  (dolist (teardown '(hermes-chat--reset-transcript fundamental-mode kill-buffer))
    (hermes-test-with-chat-buffer
     (insert "# draft")
     (hermes-test-draft-flush)
     (let ((overlays hermes-chat-draft--overlays))
       (insert " pending")
       (let* ((timer hermes-chat-draft--timer)
              (callback (timer--function timer))
              (args (timer--args timer)))
         (funcall teardown)
         (should-not (memq timer timer-idle-list))
         (should-not (cl-some #'overlay-buffer overlays))
         (apply callback args)
         (when (buffer-live-p (car args))
           (with-current-buffer (car args)
             (should-not hermes-chat-draft--overlays))))))))

(ert-deftest hermes-chat-draft-public-clear-retires-faces-and-timer ()
  (hermes-test-with-chat-buffer
   (insert "# clear")
   (hermes-test-draft-flush)
   (let ((overlays hermes-chat-draft--overlays))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
       (call-interactively #'hermes-chat-clear))
     (should-not (cl-some #'overlay-buffer overlays)))
   (hermes-test-draft-flush)
   (should-not hermes-chat-draft--overlays)
   (should (equal (hermes-chat-input-string) ""))))

(ert-deftest hermes-chat-draft-streaming-preserves-faces-and-draft ()
  (hermes-test-with-chat-buffer
   (insert "# draft")
   (hermes-test-draft-flush)
   (let ((undo buffer-undo-list))
     (hermes-chat--insert-entry '(:id "stream" :role assistant :content "reply"))
     (should (equal (hermes-chat-input-string) "# draft"))
     (should (eq (hermes-test-draft-face "draft") 'markdown-header-face-1))
     (should (eq undo buffer-undo-list))
     (dolist (overlay hermes-chat-draft--overlays)
       (should (>= (overlay-start overlay) hermes-chat--input-marker))))))

(ert-deftest hermes-chat-draft-bounded-work-and-reactivation ()
  (hermes-test-with-chat-buffer
   (insert "# draft")
   (hermes-test-draft-flush)
   (insert (make-string hermes-chat-draft--limit ?x))
   (cl-letf (((symbol-function 'hermes-chat-draft--fontify)
              (lambda (&rest _) (ert-fail "Oversized draft was fontified"))))
     (hermes-test-draft-flush))
   (should-not hermes-chat-draft--overlays)
   (hermes-chat--replace-input-tail "**small**")
   (hermes-chat-draft--activate)
   (hermes-chat-draft--activate)
   (should (= 1 (cl-count #'hermes-chat-draft--changed after-change-functions)))
   (hermes-test-draft-flush)
   (should (hermes-test-draft-face "small"))))

(ert-deftest hermes-chat-draft-face-whitelist-and-font-lock-face ()
  (with-temp-buffer
    (insert (propertize "x" 'font-lock-face 'bold 'display "hidden" 'invisible t)
            (propertize "y" 'face 'italic 'font-lock-face 'bold 'keymap '(keymap)))
    (should (equal (hermes-chat-draft--faces (point-min) (point-max))
                   '((0 1 bold) (1 2 (italic bold)))))))

(ert-deftest hermes-chat-draft-revalidates-after-language-fontifier ()
  (hermes-test-with-chat-buffer
   (insert "# draft")
   (let ((buffer (current-buffer)))
     (cl-letf (((symbol-function 'hermes-chat-draft--fontify)
                (lambda (_text)
                  (with-current-buffer buffer (insert " changed"))
                  '((0 2 bold)))))
       (hermes-test-draft-flush)))
   (should-not hermes-chat-draft--overlays)
   (should (timerp hermes-chat-draft--timer))))

(ert-deftest hermes-chat-draft-send-keeps-literal-submission ()
  (hermes-test-with-chat-buffer
   (let ((draft "**bold α**\n```elisp\n(+ 1 2)") sent)
     (insert draft)
     (hermes-test-draft-flush)
     (cl-letf (((symbol-function 'hermes-chat--submit-content)
                (lambda (content &rest _) (setq sent content) t)))
       (call-interactively #'hermes-chat-send))
     (should (equal-including-properties sent draft))
     (should (equal (hermes-chat-input-string) ""))
     (hermes-test-draft-flush)
     (should-not hermes-chat-draft--overlays))))

(ert-deftest hermes-chat-draft-fontifier-suppresses-owned-buffer-hooks ()
  (let* ((calls 0)
         (python-mode-hook (list (lambda () (cl-incf calls)))))
    (with-temp-buffer
      (insert "(message \"literal\")")
      ;; Keep mode hooks delayed throughout work in the owned helper buffer.
      (cl-letf (((symbol-function 'font-lock-ensure)
                 (lambda (&rest _) (python-mode))))
        (hermes-chat-draft--code-faces 'emacs-lisp-mode (point-min) (point-max))))
    (should (zerop calls))))

(defun hermes-test-draft-face-vector (spans size)
  "Expand face SPANS into a SIZE-character vector, including unstyled gaps."
  (let ((faces (make-vector size nil)))
    (dolist (span spans)
      (cl-loop for pos from (nth 0 span) below (nth 1 span)
               do (aset faces pos (nth 2 span))))
    faces))

(ert-deftest hermes-chat-draft-faces-match-native-markdown ()
  ;; Compare every character, including whitespace and fence delimiters, to
  ;; the dependency itself rather than encoding a second styling policy.
  (let ((buffers (buffer-list)))
    (unwind-protect
        (dolist (fence '("```" "~~~"))
          (let* ((text (concat "# Heading\n\n**bold** and `inline`\n\n"
                               fence "elisp\n(defun sample (arg)\n"
                               "  ;; comment\n  (message \"α\" arg))\n"
                               "\n" fence "\n\nplain\n"))
                 (expected
                  (with-temp-buffer
                    (insert text)
                    (delay-mode-hooks
                      (markdown-mode)
                      (setq-local markdown-fontify-code-blocks-natively t)
                      (font-lock-ensure))
                    (hermes-chat-draft--faces (point-min) (point-max)))))
            (should (equal (hermes-test-draft-face-vector
                            (hermes-chat-draft--fontify text) (length text))
                           (hermes-test-draft-face-vector expected (length text))))))
      (dolist (buffer (seq-difference (buffer-list) buffers))
        (when (string-prefix-p " *markdown-code-fontification:" (buffer-name buffer))
          (kill-buffer buffer))))))

(provide 'hermes-chat-draft-tests)
;;; hermes-chat-draft-tests.el ends here
