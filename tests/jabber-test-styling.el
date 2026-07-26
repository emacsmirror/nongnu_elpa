;;; jabber-test-styling.el --- Tests for jabber-styling  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0393 Message Styling.

;;; Code:

(require 'ert)

(require 'jabber-xml)
(require 'jabber-disco)
(require 'jabber-styling)

;;; Load contract

(ert-deftest jabber-test-styling-loads-org-faces ()
  "Loading styling makes its inherited Org faces available."
  (should (featurep 'org-faces))
  (should (facep 'org-block))
  (should (facep 'org-block-begin-line)))

;;; Group 1: jabber-styling--classify-block

(ert-deftest jabber-test-styling-classify-plain ()
  "Plain text lines classify as plain."
  (should (eq 'plain (jabber-styling--classify-block "Hello world"))))

(ert-deftest jabber-test-styling-classify-quote ()
  "Lines starting with > classify as quote."
  (should (eq 'quote (jabber-styling--classify-block "> quoted text"))))

(ert-deftest jabber-test-styling-classify-pre-open ()
  "Lines starting with ``` with extra text classify as pre-open."
  (should (eq 'pre-open (jabber-styling--classify-block "```python"))))

(ert-deftest jabber-test-styling-classify-pre-close ()
  "Bare ``` line classifies as pre-close."
  (should (eq 'pre-close (jabber-styling--classify-block "```"))))

(ert-deftest jabber-test-styling-classify-pre-close-not-open ()
  "```python should not close a pre block."
  (should (not (eq 'pre-close (jabber-styling--classify-block "```python")))))

;;; Group 2: jabber-styling--parse-spans

(ert-deftest jabber-test-styling-span-bold ()
  "Asterisks produce bold span."
  (let ((spans (jabber-styling--parse-spans "*bold*")))
    (should (equal '((0 6 jabber-styling-bold)) spans))))

(ert-deftest jabber-test-styling-span-italic ()
  "Underscores produce italic span."
  (let ((spans (jabber-styling--parse-spans "_italic_")))
    (should (equal '((0 8 jabber-styling-italic)) spans))))

(ert-deftest jabber-test-styling-span-strike ()
  "Tildes produce strikethrough span."
  (let ((spans (jabber-styling--parse-spans "~strike~")))
    (should (equal '((0 8 jabber-styling-strike)) spans))))

(ert-deftest jabber-test-styling-span-pre ()
  "Backticks produce preformatted span."
  (let ((spans (jabber-styling--parse-spans "`code`")))
    (should (equal '((0 6 jabber-styling-pre)) spans))))

(ert-deftest jabber-test-styling-span-mid-line ()
  "Span in the middle of text after whitespace."
  (let ((spans (jabber-styling--parse-spans "hello *world* end")))
    (should (equal '((6 13 jabber-styling-bold)) spans))))

(ert-deftest jabber-test-styling-span-multiple ()
  "Multiple different spans on one line."
  (let ((spans (jabber-styling--parse-spans "*bold* and _italic_")))
    (should (equal 2 (length spans)))
    (should (equal '(0 6 jabber-styling-bold) (nth 0 spans)))
    (should (equal '(11 19 jabber-styling-italic) (nth 1 spans)))))

(ert-deftest jabber-test-styling-span-lazy-matching ()
  "Lazy matching: *a* b *c* produces two separate bold spans."
  (let ((spans (jabber-styling--parse-spans "*a* b *c*")))
    (should (equal 2 (length spans)))
    (should (equal '(0 3 jabber-styling-bold) (nth 0 spans)))
    (should (equal '(6 9 jabber-styling-bold) (nth 1 spans)))))

(ert-deftest jabber-test-styling-span-strong-plain-star ()
  "Spec example: *strong*plain* produces one bold span."
  (let ((spans (jabber-styling--parse-spans "*strong*plain*")))
    (should (equal 1 (length spans)))
    (should (equal '(0 8 jabber-styling-bold) (car spans)))))

(ert-deftest jabber-test-styling-span-empty-rejected ()
  "Empty span ** is not valid."
  (let ((spans (jabber-styling--parse-spans "**")))
    (should (null spans))))

(ert-deftest jabber-test-styling-span-triple-star ()
  "*** produces no spans (star excluded from content)."
  (let ((spans (jabber-styling--parse-spans "***")))
    (should (null spans))))

(ert-deftest jabber-test-styling-span-quad-star ()
  "**** produces no spans (star excluded from content)."
  (let ((spans (jabber-styling--parse-spans "****")))
    (should (null spans))))

(ert-deftest jabber-test-styling-span-no-close ()
  "Unclosed span produces no styling."
  (let ((spans (jabber-styling--parse-spans "*no close")))
    (should (null spans))))

(ert-deftest jabber-test-styling-span-whitespace-after-open ()
  "Opening directive followed by whitespace is invalid."
  (let ((spans (jabber-styling--parse-spans "* not bold*")))
    (should (null spans))))

(ert-deftest jabber-test-styling-span-whitespace-before-close ()
  "Closing directive preceded by whitespace is invalid."
  (let ((spans (jabber-styling--parse-spans "*not bold *")))
    (should (null spans))))

(ert-deftest jabber-test-styling-span-not-after-nonwhitespace ()
  "Opening directive after non-whitespace char is not valid."
  (let ((spans (jabber-styling--parse-spans "foo*bar*")))
    (should (null spans))))

(ert-deftest jabber-test-styling-span-after-directive ()
  "Opening directive after another opening directive is valid."
  (let ((spans (jabber-styling--parse-spans "*_bold-italic_*")))
    (should (>= (length spans) 1))))

;;; Group 3: jabber-styling--parse-blocks

(ert-deftest jabber-test-styling-blocks-plain ()
  "Single plain line."
  (let ((blocks (jabber-styling--parse-blocks "hello\n")))
    (should (equal '((plain 0 6)) blocks))))

(ert-deftest jabber-test-styling-blocks-quote ()
  "Block quote line."
  (let ((blocks (jabber-styling--parse-blocks "> quoted\n")))
    (should (equal '((quote 0 9)) blocks))))

(ert-deftest jabber-test-styling-blocks-pre ()
  "Preformatted code block with exact ``` closing."
  (let ((blocks (jabber-styling--parse-blocks "```\ncode here\n```\n")))
    (should (equal 1 (length blocks)))
    (should (eq 'pre (caar blocks)))))

(ert-deftest jabber-test-styling-blocks-pre-language-no-close ()
  "```python does not close a pre block opened by ```."
  (let ((blocks (jabber-styling--parse-blocks "```\ncode\n```python\n```\n")))
    ;; Should be one pre block (``` to final ```)
    (should (equal 1 (length blocks)))
    (should (eq 'pre (caar blocks)))))

(ert-deftest jabber-test-styling-blocks-pre-unclosed ()
  "Unclosed preformatted block extends to end."
  (let ((blocks (jabber-styling--parse-blocks "```\ncode\nmore\n")))
    (should (equal 1 (length blocks)))
    (should (eq 'pre (caar blocks)))))

(ert-deftest jabber-test-styling-blocks-mixed ()
  "Mixed block types."
  (let ((blocks (jabber-styling--parse-blocks "> quote\nplain\n")))
    (should (equal 2 (length blocks)))
    (should (eq 'quote (car (nth 0 blocks))))
    (should (eq 'plain (car (nth 1 blocks))))))

;;; Group 4: jabber-styling--strip-quote-prefix

(ert-deftest jabber-test-styling-strip-quote-space ()
  "Strip > followed by space."
  (should (equal "text" (jabber-styling--strip-quote-prefix "> text"))))

(ert-deftest jabber-test-styling-strip-quote-no-space ()
  "Strip > not followed by space."
  (should (equal "text" (jabber-styling--strip-quote-prefix ">text"))))

(ert-deftest jabber-test-styling-strip-quote-bare ()
  "Strip bare >."
  (should (equal "" (jabber-styling--strip-quote-prefix ">"))))

;;; Group 5: jabber-styling--apply-region (integration)

(ert-deftest jabber-test-styling-apply-bold-face ()
  "Bold text gets jabber-styling-bold face."
  (with-temp-buffer
    (insert "hello *world* end")
    (jabber-styling--apply-region (point-min) (point-max))
    (goto-char 7) ;; inside *world*
    (let ((face (get-text-property (point) 'face)))
      (should (memq 'jabber-styling-bold (if (listp face) face (list face)))))))

(ert-deftest jabber-test-styling-apply-quote-face ()
  "Quote lines get jabber-styling-quote face."
  (with-temp-buffer
    (insert "> quoted text\n")
    (jabber-styling--apply-region (point-min) (point-max))
    (goto-char 2)
    (let ((face (get-text-property (point) 'face)))
      (should (memq 'jabber-styling-quote
                    (if (listp face) face (list face)))))))

(ert-deftest jabber-test-styling-apply-pre-block-face ()
  "The code body carries the block background face."
  (with-temp-buffer
    (insert "```emacs-lisp\ncode\n```\n")
    (jabber-styling--apply-region (point-min) (point-max))
    (goto-char 16) ;; inside the code body
    (let ((face (get-text-property (point) 'face)))
      (should (memq 'jabber-styling-pre-block
                    (if (listp face) face (list face)))))))

(ert-deftest jabber-test-styling-no-spans-in-pre ()
  "Spans inside preformatted blocks are not styled."
  (with-temp-buffer
    (insert "```\n*not bold*\n```\n")
    (jabber-styling--apply-region (point-min) (point-max))
    (goto-char 6) ;; inside *not bold*
    (let ((face (get-text-property (point) 'face)))
      (should (not (memq 'jabber-styling-bold
                         (if (listp face) face (list face))))))))

(ert-deftest jabber-test-styling-apply-bold-inside-quote ()
  "Bold spans inside block quotes are styled."
  (with-temp-buffer
    (insert "> *bold* text\n")
    (jabber-styling--apply-region (point-min) (point-max))
    ;; Position 3 is inside *bold* (after "> ")
    (goto-char 3)
    (let ((face (get-text-property (point) 'face)))
      (should (memq 'jabber-styling-bold
                    (if (listp face) face (list face)))))))

(ert-deftest jabber-test-styling-apply-lazy-two-bolds ()
  "Lazy matching produces two bold spans in buffer."
  (with-temp-buffer
    (insert "*a* and *b*")
    (jabber-styling--apply-region (point-min) (point-max))
    ;; Position 2 (inside *a*)
    (goto-char 2)
    (let ((face (get-text-property (point) 'face)))
      (should (memq 'jabber-styling-bold
                    (if (listp face) face (list face)))))
    ;; Position 10 (inside *b*)
    (goto-char 10)
    (let ((face (get-text-property (point) 'face)))
      (should (memq 'jabber-styling-bold
                    (if (listp face) face (list face)))))
    ;; Position 5 (plain " and ") should not be bold
    (goto-char 5)
    (let ((face (get-text-property (point) 'face)))
      (should (not (memq 'jabber-styling-bold
                         (if (listp face) face (list face))))))))

;;; Group 6: jabber-styling--remove-faces

(ert-deftest jabber-test-styling-remove-faces-bold ()
  "Remove-faces strips jabber-styling-bold from a region."
  (with-temp-buffer
    (insert (propertize "bold" 'face 'jabber-styling-bold))
    (jabber-styling--remove-faces (point-min) (point-max))
    (should (null (get-text-property 1 'face)))))

(ert-deftest jabber-test-styling-remove-faces-preserves-other ()
  "Remove-faces preserves non-styling faces."
  (with-temp-buffer
    (insert (propertize "text" 'face '(jabber-styling-bold jabber-chat-text-foreign)))
    (jabber-styling--remove-faces (point-min) (point-max))
    (should (equal 'jabber-chat-text-foreign (get-text-property 1 'face)))))

(ert-deftest jabber-test-styling-remove-faces-multiple ()
  "Remove-faces strips multiple styling faces, keeps the rest."
  (with-temp-buffer
    (insert (propertize "text" 'face '(jabber-styling-bold jabber-styling-italic shadow)))
    (jabber-styling--remove-faces (point-min) (point-max))
    (should (equal 'shadow (get-text-property 1 'face)))))

(ert-deftest jabber-test-styling-remove-faces-no-styling ()
  "Remove-faces is a no-op when no styling faces are present."
  (with-temp-buffer
    (insert (propertize "text" 'face 'shadow))
    (jabber-styling--remove-faces (point-min) (point-max))
    (should (equal 'shadow (get-text-property 1 'face)))))

(ert-deftest jabber-test-styling-remove-then-reapply ()
  "Remove-faces followed by apply-region re-applies styling cleanly."
  (with-temp-buffer
    (insert "*bold* text")
    (jabber-styling--apply-region (point-min) (point-max))
    ;; First verify bold is applied
    (should (memq 'jabber-styling-bold
                  (let ((f (get-text-property 2 'face)))
                    (if (listp f) f (list f)))))
    ;; Remove and re-apply
    (jabber-styling--remove-faces (point-min) (point-max))
    (should (null (get-text-property 2 'face)))
    (jabber-styling--apply-region (point-min) (point-max))
    (should (memq 'jabber-styling-bold
                  (let ((f (get-text-property 2 'face)))
                    (if (listp f) f (list f)))))))

;;; Group 7: pre-span suppression and adjacent spans

(ert-deftest jabber-test-styling-pre-suppresses-bold ()
  "Bold inside a preformatted span is not styled."
  (let ((spans (jabber-styling--parse-spans "`*not bold*`")))
    (should (equal 1 (length spans)))
    (should (eq 'jabber-styling-pre (nth 2 (car spans))))))

(ert-deftest jabber-test-styling-adjacent-spans ()
  "Adjacent spans separated by directive char are both matched."
  (let ((spans (jabber-styling--parse-spans "*a*_b_")))
    (should (equal 2 (length spans)))
    (should (eq 'jabber-styling-bold (nth 2 (nth 0 spans))))
    (should (eq 'jabber-styling-italic (nth 2 (nth 1 spans))))))

;;; Group 8: jabber-styling--fence-lang

(ert-deftest jabber-test-styling-fence-lang-simple ()
  (should (equal "python" (jabber-styling--fence-lang "```python"))))

(ert-deftest jabber-test-styling-fence-lang-none ()
  (should-not (jabber-styling--fence-lang "```")))

(ert-deftest jabber-test-styling-fence-lang-first-token ()
  (should (equal "python" (jabber-styling--fence-lang "```python extra"))))

(ert-deftest jabber-test-styling-fence-lang-downcase ()
  (should (equal "elisp" (jabber-styling--fence-lang "```Elisp"))))

;;; Group 9: jabber-styling--pre-block-parts

(ert-deftest jabber-test-styling-pre-block-parts-basic ()
  (should (equal '("elisp" 9 15)
                 (jabber-styling--pre-block-parts "```elisp\n(foo)\n```"))))

(ert-deftest jabber-test-styling-pre-block-parts-unterminated ()
  "Unterminated blocks extend the code region to end of text."
  (should (equal '("elisp" 9 14)
                 (jabber-styling--pre-block-parts "```elisp\n(foo)"))))

(ert-deftest jabber-test-styling-pre-block-parts-no-lang ()
  (should (equal '(nil 4 6)
                 (jabber-styling--pre-block-parts "```\nx\n```"))))

(ert-deftest jabber-test-styling-pre-block-parts-empty-code ()
  (should (equal '("elisp" 9 9)
                 (jabber-styling--pre-block-parts "```elisp\n```"))))

(ert-deftest jabber-test-styling-pre-block-parts-trailing-newline ()
  "Closing fence line is excluded even with a trailing newline."
  (should (equal '(nil 4 6)
                 (jabber-styling--pre-block-parts "```\nx\n```\n"))))

;;; Group 10: jabber-styling--lang-mode

(ert-deftest jabber-test-styling-lang-mode-direct ()
  (should (eq 'emacs-lisp-mode (jabber-styling--lang-mode "emacs-lisp"))))

(ert-deftest jabber-test-styling-lang-mode-alias ()
  "Aliases in jabber-styling-code-lang-modes resolve first."
  (should (eq 'emacs-lisp-mode (jabber-styling--lang-mode "elisp"))))

(ert-deftest jabber-test-styling-lang-mode-missing ()
  (should-not (jabber-styling--lang-mode "nosuchlang")))

(ert-deftest jabber-test-styling-lang-mode-nil ()
  (should-not (jabber-styling--lang-mode nil)))

(ert-deftest jabber-test-styling-lang-mode-remap ()
  (let ((major-mode-remap-alist '((emacs-lisp-mode . lisp-interaction-mode))))
    (should (eq 'lisp-interaction-mode
                (jabber-styling--lang-mode "emacs-lisp")))))

;;; Group 11: jabber-styling--fontify-code

(defun jabber-test-styling--stretch-faces-at (offset stretches)
  "Return faces from STRETCHES covering OFFSET, flattened to a list."
  (cl-loop for (s e face) in stretches
           when (and (<= s offset) (< offset e))
           append (if (listp face) face (list face))))

(ert-deftest jabber-test-styling-fontify-code-keyword ()
  "The defun keyword gets font-lock-keyword-face."
  (let ((stretches (jabber-styling--fontify-code "(defun f ())"
                                                 'emacs-lisp-mode)))
    (should (memq 'font-lock-keyword-face
                  (jabber-test-styling--stretch-faces-at 1 stretches)))))

(ert-deftest jabber-test-styling-fontify-code-string-face ()
  (let ((stretches (jabber-styling--fontify-code "\"hi\"" 'emacs-lisp-mode)))
    (should (memq 'font-lock-string-face
                  (jabber-test-styling--stretch-faces-at 1 stretches)))))

(ert-deftest jabber-test-styling-fontify-code-bounded ()
  "All stretch offsets fall inside the code string."
  (let* ((code "(defun f () \"doc\" nil)")
         (stretches (jabber-styling--fontify-code code 'emacs-lisp-mode)))
    (should stretches)
    (dolist (stretch stretches)
      (should (<= 0 (nth 0 stretch)))
      (should (< (nth 0 stretch) (nth 1 stretch)))
      (should (<= (nth 1 stretch) (length code))))))

(ert-deftest jabber-test-styling-fontify-code-buffer-reused ()
  "The per-mode work buffer persists and results are stable."
  (let ((first (jabber-styling--fontify-code "(defun f ())" 'emacs-lisp-mode))
        (second (jabber-styling--fontify-code "(defun f ())"
                                              'emacs-lisp-mode)))
    (should (buffer-live-p
             (get-buffer " *jabber-styling-fontify:emacs-lisp-mode*")))
    (should (equal first second))))

;;; Group 12: code block fontification (integration)

(defun jabber-test-styling--faces-at (pos)
  "Return the face property at POS as a list."
  (let ((face (get-text-property pos 'face)))
    (if (listp face) face (list face))))

(ert-deftest jabber-test-styling-apply-code-block ()
  "Lang blocks fontify the body and tint it with the block bg.
The mode's face wins (it precedes the bg), and the body is marked."
  (with-temp-buffer
    (insert "```emacs-lisp\n(defun f ())\n```")
    (jabber-styling--apply-region (point-min) (point-max))
    (let ((faces (jabber-test-styling--faces-at 16))) ; d of defun
      (should (memq 'font-lock-keyword-face faces))
      (should (memq 'jabber-styling-pre-block faces))
      (should (< (seq-position faces 'font-lock-keyword-face)
                 (seq-position faces 'jabber-styling-pre-block))))
    (should (get-text-property 16 'jabber-styling-fontified))))

(ert-deftest jabber-test-styling-pre-block-regions ()
  "Fence lines get the fence face; the body gets the block bg.
Neither face carries the other's, mirroring Org's src blocks."
  (with-temp-buffer
    (insert "```emacs-lisp\n(defun f ())\n```")
    (jabber-styling--apply-region (point-min) (point-max))
    ;; Fence lines: fence face, no block bg.
    (dolist (pos '(2 5 29))
      (should (memq 'jabber-styling-pre-block-fence
                    (jabber-test-styling--faces-at pos)))
      (should-not (memq 'jabber-styling-pre-block
                        (jabber-test-styling--faces-at pos))))
    ;; Body: block bg, no fence face.
    (dolist (pos '(15 16 22 26))
      (should (memq 'jabber-styling-pre-block
                    (jabber-test-styling--faces-at pos)))
      (should-not (memq 'jabber-styling-pre-block-fence
                        (jabber-test-styling--faces-at pos))))))

(ert-deftest jabber-test-styling-apply-code-block-unknown-lang ()
  "Unknown languages: no native fontification, but the body still
gets the block background and the fences the fence face."
  (with-temp-buffer
    (insert "```nosuchlang\nx\n```")
    (jabber-styling--apply-region (point-min) (point-max))
    (should (memq 'jabber-styling-pre-block-fence
                  (jabber-test-styling--faces-at 2)))
    (should (memq 'jabber-styling-pre-block (jabber-test-styling--faces-at 15)))
    (should-not (text-property-any (point-min) (point-max)
                                   'jabber-styling-fontified t))))

(ert-deftest jabber-test-styling-apply-code-block-disabled ()
  (with-temp-buffer
    (insert "```emacs-lisp\n(defun f ())\n```")
    (let ((jabber-styling-fontify-code-blocks nil))
      (jabber-styling--apply-region (point-min) (point-max)))
    (should-not (text-property-any (point-min) (point-max)
                                   'jabber-styling-fontified t))))

(ert-deftest jabber-test-styling-apply-code-block-max-size ()
  "Oversized blocks get the background but no native fontification."
  (with-temp-buffer
    (insert "```emacs-lisp\n(defun f ())\n```")
    (let ((jabber-styling-fontify-max-size 1))
      (jabber-styling--apply-region (point-min) (point-max)))
    (should-not (text-property-any (point-min) (point-max)
                                   'jabber-styling-fontified t))))

(ert-deftest jabber-test-styling-remove-code-fontification ()
  "Removal clears native faces and the marker property."
  (with-temp-buffer
    (insert "```emacs-lisp\n(defun f ())\n```")
    (jabber-styling--apply-region (point-min) (point-max))
    (jabber-styling--remove-code-fontification (point-min) (point-max))
    (should-not (memq 'font-lock-keyword-face
                      (jabber-test-styling--faces-at 16)))
    (should-not (text-property-any (point-min) (point-max)
                                   'jabber-styling-fontified t))))

(ert-deftest jabber-test-styling-code-block-clears-chat-face ()
  "The body drops any preexisting chat face uniformly.
Both a recognized and an unrecognized language lose the
surrounding face, replaced by the block background."
  (dolist (lang '("emacs-lisp" "nosuchlang"))
    (with-temp-buffer
      (insert "```" lang "\n")
      (let ((code-start (point)))
        (insert (propertize "xy" 'face 'jabber-chat-text-local) "\n```")
        (jabber-styling--apply-region (point-min) (point-max))
        (let ((faces (jabber-test-styling--faces-at code-start)))
          (should-not (memq 'jabber-chat-text-local faces))
          (should (memq 'jabber-styling-pre-block faces)))))))

(ert-deftest jabber-test-styling-code-block-disabled-keeps-chat-face ()
  "With fontification disabled the body keeps its chat face,
still tinted by the block background."
  (with-temp-buffer
    (insert "```emacs-lisp\n")
    (let ((code-start (point)))
      (insert (propertize "xy" 'face 'jabber-chat-text-local) "\n```")
      (let ((jabber-styling-fontify-code-blocks nil))
        (jabber-styling--apply-region (point-min) (point-max)))
      (let ((faces (jabber-test-styling--faces-at code-start)))
        (should (memq 'jabber-chat-text-local faces))
        (should (memq 'jabber-styling-pre-block faces))))))

(provide 'jabber-test-styling)
;;; jabber-test-styling.el ends here
