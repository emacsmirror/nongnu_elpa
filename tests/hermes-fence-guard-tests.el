;;; hermes-fence-guard-tests.el --- markdown-mode pre-2.8 compatibility  -*- lexical-binding: t; -*-

;;; Commentary:

;; `hermes-chat--fenced-block-at-point' must keep working when the installed
;; markdown-mode lacks `markdown-make-gfm-fence-regex' (added in 2.8, while
;; the Package-Requires floor is 2.6).  Regression for the resume wedge where
;; any fenced transcript killed the `session.resume' callback mid-render.

;;; Code:

(require 'ert)
(require 'hermes-chat)

(ert-deftest hermes-chat-fenced-block-scan-tolerates-pre-2-8-markdown-mode ()
  "Fence scanning falls back to plain regexes without the 2.8 builder."
  (let ((text "text\n```diff\n-old\n+new\n```\nmore\n"))
    ;; Simulate a pre-2.8 markdown-mode: the builder symbol must be UNBOUND,
    ;; because `fboundp' gates the fallback branch.
    (let ((had (fboundp 'markdown-make-gfm-fence-regex))
          (saved (and (fboundp 'markdown-make-gfm-fence-regex)
                      (symbol-function 'markdown-make-gfm-fence-regex))))
      (unwind-protect
          (progn
            (fmakunbound 'markdown-make-gfm-fence-regex)
            (with-temp-buffer
              (insert text)
              (let ((blocks (hermes-chat--diff-blocks text)))
                (should (= 1 (length blocks)))
                ;; START is the zero-based index of the opening fence's first
                ;; backtick; END is exclusive, one past the closing fence.
                (should (equal "```diff"
                               (substring text (nth 0 (car blocks))
                                          (+ 7 (nth 0 (car blocks))))))
                (should (equal "```"
                               (substring text (- (nth 1 (car blocks)) 4)
                                          (- (nth 1 (car blocks)) 1)))))))
        (when had
          (fset 'markdown-make-gfm-fence-regex saved)))))
  ;; A transcript without fences yields no blocks.
  (should (null (hermes-chat--diff-blocks "plain `inline code` only\n"))))

(ert-deftest hermes-chat-fenced-block-fallback-preserves-boundaries ()
  "Fallback closers accept indentation, not shorter or mixed fences."
  (dolist (fence '("```" "~~~~" "````"))
    ;; Markdown 2.6/2.7 only recognize three-backtick opening fences.
    ;; Keep their native opening syntax rather than extending it here.
    (when (or (not (equal fence "````"))
              (string-match-p markdown-regex-gfm-code-block-open
                              "````text"))
      (dolist (fallback '(nil t))
        (let* ((builder (if (eq (aref fence 0) ?`)
                            'markdown-make-gfm-fence-regex
                          'markdown-make-tilde-fence-regex))
               (saved (and (fboundp builder) (symbol-function builder)))
               (body (concat "body\n" (substring fence 1) "\n"
                             (if (eq (aref fence 0) ?`) "~~~~" "````")
                             "\n" fence " trailing\n"))
               (block (concat "  " fence "text\n" body
                              " \t" fence "\t \n")))
          (unwind-protect
              (progn
                (when fallback (fmakunbound builder))
                (with-temp-buffer
                  (insert block "after\n")
                  (goto-char (point-min))
                  (should (equal (hermes-chat--fenced-block-at-point)
                                 (list 0 (length block) body "text")))
                  (should (looking-at "after")))
                (with-temp-buffer
                  (insert fence "text\nbody\n " fence
                          (substring fence 0 1) "\n")
                  (goto-char (point-min))
                  (should (equal (hermes-chat--fenced-block-at-point)
                                 (list 0 (buffer-size) "body\n" "text"))))
                (with-temp-buffer
                  (insert fence "text\n" body)
                  (goto-char (point-min))
                  (should (equal (hermes-chat--fenced-block-at-point)
                                 (list 0 (buffer-size) body "text")))))
            (if saved (fset builder saved) (fmakunbound builder))))))))

(ert-deftest hermes-chat-fenced-block-fallback-keeps-following-diff ()
  "An indented closing fence must not swallow a later diff disclosure."
  (let* ((builder 'markdown-make-gfm-fence-regex)
         (saved (and (fboundp builder) (symbol-function builder)))
         (diff "```diff\n-old\n+new\n  ```\n")
         (text (concat "  ```text\nliteral\n  ```\n" diff "after\n")))
    (unwind-protect
        (progn
          (fmakunbound builder)
          (let ((blocks (hermes-chat--diff-blocks text)))
            (should (= (length blocks) 1))
            (should (equal (substring text (caar blocks) (cadar blocks))
                           diff))))
      (if saved (fset builder saved) (fmakunbound builder)))))

(provide 'hermes-fence-guard-tests)
;;; hermes-fence-guard-tests.el ends here
