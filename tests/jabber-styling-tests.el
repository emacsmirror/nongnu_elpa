;;; jabber-styling-tests.el --- Tests for XEP-0393 Message Styling  -*- lexical-binding: t; -*-

(require 'ert)

(load (expand-file-name "../lisp/jabber-xml.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/jabber-disco.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/jabber-styling.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;;; Group 1: jabber-styling--classify-block

(ert-deftest jabber-styling-test-classify-plain ()
  "Plain text lines classify as plain."
  (should (eq 'plain (jabber-styling--classify-block "Hello world"))))

(ert-deftest jabber-styling-test-classify-quote ()
  "Lines starting with > classify as quote."
  (should (eq 'quote (jabber-styling--classify-block "> quoted text"))))

(ert-deftest jabber-styling-test-classify-pre-open ()
  "Lines starting with ``` with extra text classify as pre-open."
  (should (eq 'pre-open (jabber-styling--classify-block "```python"))))

(ert-deftest jabber-styling-test-classify-pre-close ()
  "Bare ``` line classifies as pre-close."
  (should (eq 'pre-close (jabber-styling--classify-block "```"))))

(ert-deftest jabber-styling-test-classify-pre-close-not-open ()
  "```python should not close a pre block."
  (should (not (eq 'pre-close (jabber-styling--classify-block "```python")))))

;;; Group 2: jabber-styling--parse-spans

(ert-deftest jabber-styling-test-span-bold ()
  "Asterisks produce bold span."
  (let ((spans (jabber-styling--parse-spans "*bold*")))
    (should (equal '((0 6 jabber-styling-bold)) spans))))

(ert-deftest jabber-styling-test-span-italic ()
  "Underscores produce italic span."
  (let ((spans (jabber-styling--parse-spans "_italic_")))
    (should (equal '((0 8 jabber-styling-italic)) spans))))

(ert-deftest jabber-styling-test-span-strike ()
  "Tildes produce strikethrough span."
  (let ((spans (jabber-styling--parse-spans "~strike~")))
    (should (equal '((0 8 jabber-styling-strike)) spans))))

(ert-deftest jabber-styling-test-span-pre ()
  "Backticks produce preformatted span."
  (let ((spans (jabber-styling--parse-spans "`code`")))
    (should (equal '((0 6 jabber-styling-pre)) spans))))

(ert-deftest jabber-styling-test-span-mid-line ()
  "Span in the middle of text after whitespace."
  (let ((spans (jabber-styling--parse-spans "hello *world* end")))
    (should (equal '((6 13 jabber-styling-bold)) spans))))

(ert-deftest jabber-styling-test-span-multiple ()
  "Multiple different spans on one line."
  (let ((spans (jabber-styling--parse-spans "*bold* and _italic_")))
    (should (equal 2 (length spans)))
    (should (equal '(0 6 jabber-styling-bold) (nth 0 spans)))
    (should (equal '(11 19 jabber-styling-italic) (nth 1 spans)))))

(ert-deftest jabber-styling-test-span-lazy-matching ()
  "Lazy matching: *a* b *c* produces two separate bold spans."
  (let ((spans (jabber-styling--parse-spans "*a* b *c*")))
    (should (equal 2 (length spans)))
    (should (equal '(0 3 jabber-styling-bold) (nth 0 spans)))
    (should (equal '(6 9 jabber-styling-bold) (nth 1 spans)))))

(ert-deftest jabber-styling-test-span-strong-plain-star ()
  "Spec example: *strong*plain* produces one bold span."
  (let ((spans (jabber-styling--parse-spans "*strong*plain*")))
    (should (equal 1 (length spans)))
    (should (equal '(0 8 jabber-styling-bold) (car spans)))))

(ert-deftest jabber-styling-test-span-empty-rejected ()
  "Empty span ** is not valid."
  (let ((spans (jabber-styling--parse-spans "**")))
    (should (null spans))))

(ert-deftest jabber-styling-test-span-triple-star ()
  "*** produces bold with * as content (lazy match, spec ambiguous)."
  (let ((spans (jabber-styling--parse-spans "***")))
    (should (equal '((0 3 jabber-styling-bold)) spans))))

(ert-deftest jabber-styling-test-span-quad-star ()
  "**** produces bold with ** as content (lazy match, spec ambiguous)."
  (let ((spans (jabber-styling--parse-spans "****")))
    (should (equal '((0 3 jabber-styling-bold)) spans))))

(ert-deftest jabber-styling-test-span-no-close ()
  "Unclosed span produces no styling."
  (let ((spans (jabber-styling--parse-spans "*no close")))
    (should (null spans))))

(ert-deftest jabber-styling-test-span-whitespace-after-open ()
  "Opening directive followed by whitespace is invalid."
  (let ((spans (jabber-styling--parse-spans "* not bold*")))
    (should (null spans))))

(ert-deftest jabber-styling-test-span-whitespace-before-close ()
  "Closing directive preceded by whitespace is invalid."
  (let ((spans (jabber-styling--parse-spans "*not bold *")))
    (should (null spans))))

(ert-deftest jabber-styling-test-span-not-after-nonwhitespace ()
  "Opening directive after non-whitespace char is not valid."
  (let ((spans (jabber-styling--parse-spans "foo*bar*")))
    (should (null spans))))

(ert-deftest jabber-styling-test-span-after-directive ()
  "Opening directive after another opening directive is valid."
  (let ((spans (jabber-styling--parse-spans "*_bold-italic_*")))
    (should (>= (length spans) 1))))

;;; Group 3: jabber-styling--parse-blocks

(ert-deftest jabber-styling-test-blocks-plain ()
  "Single plain line."
  (let ((blocks (jabber-styling--parse-blocks "hello\n")))
    (should (equal '((plain 0 6)) blocks))))

(ert-deftest jabber-styling-test-blocks-quote ()
  "Block quote line."
  (let ((blocks (jabber-styling--parse-blocks "> quoted\n")))
    (should (equal '((quote 0 9)) blocks))))

(ert-deftest jabber-styling-test-blocks-pre ()
  "Preformatted code block with exact ``` closing."
  (let ((blocks (jabber-styling--parse-blocks "```\ncode here\n```\n")))
    (should (equal 1 (length blocks)))
    (should (eq 'pre (caar blocks)))))

(ert-deftest jabber-styling-test-blocks-pre-language-no-close ()
  "```python does not close a pre block opened by ```."
  (let ((blocks (jabber-styling--parse-blocks "```\ncode\n```python\n```\n")))
    ;; Should be one pre block (``` to final ```)
    (should (equal 1 (length blocks)))
    (should (eq 'pre (caar blocks)))))

(ert-deftest jabber-styling-test-blocks-pre-unclosed ()
  "Unclosed preformatted block extends to end."
  (let ((blocks (jabber-styling--parse-blocks "```\ncode\nmore\n")))
    (should (equal 1 (length blocks)))
    (should (eq 'pre (caar blocks)))))

(ert-deftest jabber-styling-test-blocks-mixed ()
  "Mixed block types."
  (let ((blocks (jabber-styling--parse-blocks "> quote\nplain\n")))
    (should (equal 2 (length blocks)))
    (should (eq 'quote (car (nth 0 blocks))))
    (should (eq 'plain (car (nth 1 blocks))))))

;;; Group 4: jabber-styling--strip-quote-prefix

(ert-deftest jabber-styling-test-strip-quote-space ()
  "Strip > followed by space."
  (should (equal "text" (jabber-styling--strip-quote-prefix "> text"))))

(ert-deftest jabber-styling-test-strip-quote-no-space ()
  "Strip > not followed by space."
  (should (equal "text" (jabber-styling--strip-quote-prefix ">text"))))

(ert-deftest jabber-styling-test-strip-quote-bare ()
  "Strip bare >."
  (should (equal "" (jabber-styling--strip-quote-prefix ">"))))

;;; Group 5: jabber-styling--apply-region (integration)

(ert-deftest jabber-styling-test-apply-bold-face ()
  "Bold text gets jabber-styling-bold face."
  (with-temp-buffer
    (insert "hello *world* end")
    (jabber-styling--apply-region (point-min) (point-max))
    (goto-char 7) ;; inside *world*
    (let ((face (get-text-property (point) 'face)))
      (should (memq 'jabber-styling-bold (if (listp face) face (list face)))))))

(ert-deftest jabber-styling-test-apply-quote-face ()
  "Quote lines get jabber-styling-quote face."
  (with-temp-buffer
    (insert "> quoted text\n")
    (jabber-styling--apply-region (point-min) (point-max))
    (goto-char 2)
    (let ((face (get-text-property (point) 'face)))
      (should (memq 'jabber-styling-quote
                    (if (listp face) face (list face)))))))

(ert-deftest jabber-styling-test-apply-pre-block-face ()
  "Pre block gets jabber-styling-pre-block face."
  (with-temp-buffer
    (insert "```\ncode\n```\n")
    (jabber-styling--apply-region (point-min) (point-max))
    (goto-char 5) ;; inside code
    (let ((face (get-text-property (point) 'face)))
      (should (memq 'jabber-styling-pre-block
                    (if (listp face) face (list face)))))))

(ert-deftest jabber-styling-test-no-spans-in-pre ()
  "Spans inside preformatted blocks are not styled."
  (with-temp-buffer
    (insert "```\n*not bold*\n```\n")
    (jabber-styling--apply-region (point-min) (point-max))
    (goto-char 6) ;; inside *not bold*
    (let ((face (get-text-property (point) 'face)))
      (should (not (memq 'jabber-styling-bold
                         (if (listp face) face (list face))))))))

(ert-deftest jabber-styling-test-apply-bold-inside-quote ()
  "Bold spans inside block quotes are styled."
  (with-temp-buffer
    (insert "> *bold* text\n")
    (jabber-styling--apply-region (point-min) (point-max))
    ;; Position 3 is inside *bold* (after "> ")
    (goto-char 3)
    (let ((face (get-text-property (point) 'face)))
      (should (memq 'jabber-styling-bold
                    (if (listp face) face (list face)))))))

(ert-deftest jabber-styling-test-apply-lazy-two-bolds ()
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

(ert-deftest jabber-styling-test-remove-faces-bold ()
  "Remove-faces strips jabber-styling-bold from a region."
  (with-temp-buffer
    (insert (propertize "bold" 'face 'jabber-styling-bold))
    (jabber-styling--remove-faces (point-min) (point-max))
    (should (null (get-text-property 1 'face)))))

(ert-deftest jabber-styling-test-remove-faces-preserves-other ()
  "Remove-faces preserves non-styling faces."
  (with-temp-buffer
    (insert (propertize "text" 'face '(jabber-styling-bold jabber-chat-text-foreign)))
    (jabber-styling--remove-faces (point-min) (point-max))
    (should (equal 'jabber-chat-text-foreign (get-text-property 1 'face)))))

(ert-deftest jabber-styling-test-remove-faces-multiple ()
  "Remove-faces strips multiple styling faces, keeps the rest."
  (with-temp-buffer
    (insert (propertize "text" 'face '(jabber-styling-bold jabber-styling-italic shadow)))
    (jabber-styling--remove-faces (point-min) (point-max))
    (should (equal 'shadow (get-text-property 1 'face)))))

(ert-deftest jabber-styling-test-remove-faces-no-styling ()
  "Remove-faces is a no-op when no styling faces are present."
  (with-temp-buffer
    (insert (propertize "text" 'face 'shadow))
    (jabber-styling--remove-faces (point-min) (point-max))
    (should (equal 'shadow (get-text-property 1 'face)))))

(ert-deftest jabber-styling-test-remove-then-reapply ()
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

(provide 'jabber-styling-tests)
;;; jabber-styling-tests.el ends here
