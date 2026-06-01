;;; adoc-mode-test.el --- test suite for adoc-mode.el
;;;
;;; Commentary:
;;
;; Call adoc-test-run to run the test suite
;;
;;; Todo:
;; - test for font lock multiline property
;; - test for presence of adoc-reserved (we do white-box testing here)
;; - test also with multiple versions of Emacs
;; - compare adoc-mode fontification with actual output from AsciiDoc, being
;;   almost the ultimate test for correctness
;;

;;; Code:

;;;; Helpers
(require 'ert)
(require 'adoc-mode)
(require 'cl-lib)

(defun adoctest-log-intervals (prop &optional print-prop)
  "Return string with intervals of property PROP in current buffer.
If PRINT-PROP is non-nil print use that property
in the output instead of PROP."
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (let ((pos (with-current-buffer buf (point-min)))
            (end (with-current-buffer buf (point-max))))
        (while (< pos end)
          (let* ((next (or (next-single-property-change pos prop buf) end))
                 (text (with-current-buffer buf
                         (buffer-substring-no-properties pos next)))
                 (val (get-text-property pos prop buf)))
            (insert (format "#(%S %s %S)\n" text (or print-prop prop) val))
            (setq pos next))))
      (buffer-string))))

;; todo:
;; - auto-create different contexts like
;;   - beginning/end of buffer
;;   - beginning/end of paragraph
;;   - side-to-side yes/no with next same construct
;; checkdoc-params: (NAME ARGS)
(defun adoctest-faces (name &rest args)
  "Test font-lock on concatenation of STRING1 ... STRINGn.

The k-th string STRINGk with k=1,...,n `should' be fontified
with FACEk.
STRING1, FACE1, ..., STRINGn, FACEn are free.

NAME is just for identifying the test.

If instead of two consecutive elements
STRINGk FACEk there is a list LISTk
then this list is spliced into the argument list
and the composed argument list is processed.

\(fn NAME STRING1 FACE1 ... STRINGn FACEn)"
  (let ((not-done t)
        (font-lock-support-mode)
	expander)
    (setq expander
	  (lambda (args)
	    (while args
	      (if (listp (car args))
		  (funcall expander (car args))
		(insert (propertize (car args) 'adoctest (cadr args)))
		(setq args (cdr args)))
	      (setq args (cdr args))
	      )))
    (with-temp-buffer
      ;; setup
      (funcall expander args)

      ;; exercise
      (adoc-mode)
      (font-lock-fontify-region (point-min) (point-max))

      ;; verify
      (goto-char (point-min))
      (ert-info
	  ((format
	    "Expected text:\n%s\nActual text:\n%s\n"
	    (adoctest-log-intervals 'adoctest 'face)
	    (adoctest-log-intervals 'face)
	    ))
	(while not-done
          (let* ((tmp (get-text-property (point) 'adoctest))
		 (tmp2 (get-text-property (point) 'face)))
            (cond
             ((null tmp)) ; nop
             ((eq tmp 'no-face)
              (should (null tmp2)))
             (t
              (if (and (listp tmp2) (not (listp tmp)))
                  (should (and (= 1 (length tmp2)) (equal tmp (car tmp2))))
		(should (equal tmp tmp2)))))
            (if (< (point) (point-max))
		(forward-char 1)
              (setq not-done nil))))))))

(defun adoctest-trans (original-text expected-text transform)
  "Calling TRANSFORM on ORIGINAL-TEXT `should' result in EXPECTED-TEXT.
ORIGINAL-TEXT is put in an temporary buffer and TRANSFORM is
evaluated using `eval'.  The resulting buffer content is compared
to EXPECTED-TEXT.

ORIGINAL-TEXT optionaly may contain the following special
charachters.  Escaping them is not (yet) supported.  They are
removed before TRANSFORM is evaluated.

!  Position of point before TRANSFORM is evaluated

<> Position of mark (<) and point (>) before TRANSFORM is
   evaluatred"
  (if (string-match "[!<>]" original-text)
      ;; original-text has ! markers
      (let ((pos 0)      ; pos in original-text
            (pos-old 0)  ; pos of the last iteration
            (pos-in-new-region-start 0)
            (pos-new-list) ; list of positions in new-original-text
            (new-original-text "")) ; as original-text, but with < > ! stripped
        ;; original-text -> new-original-text by removing ! and remembering their positions
        (while (and (< pos (length original-text))
                    (setq pos (string-match "[!<>]" original-text pos)))
          (setq new-original-text (concat new-original-text (substring original-text pos-old pos)))
          (cond
           ((eq (aref original-text pos) ?<)
            (setq pos-in-new-region-start (length new-original-text)))
           ((eq (aref original-text pos) ?>)
            (setq pos-new-list (cons (cons pos-in-new-region-start (length new-original-text)) pos-new-list)))
           (t
            (setq pos-new-list (cons (length new-original-text) pos-new-list))))
          (setq pos (1+ pos))
          (setq pos-old pos))
        (setq new-original-text (concat new-original-text (substring original-text pos-old pos)))
        ;; run adoctest-trans-inner for each remembered pos
        (while pos-new-list
          (adoctest-trans-inner new-original-text expected-text transform (car pos-new-list))
          (setq pos-new-list (cdr pos-new-list))))
    ;; original-text has no ! markers
    (adoctest-trans-inner original-text expected-text transform)))

(defun adoctest-trans-inner (original-text expected-text transform &optional pos)
  "Todo document adoctest-trans-inner ORIGINAL-TEXT EXPECTED-TEXT TRANSFORM POS."
  (let ((not-done t)
        (font-lock-support-mode))
    (with-temp-buffer
      ;; setup
      (adoc-mode)
      (insert original-text)
      (cond ; 1+: buffer pos starts at 1, but string pos at 0
       ((consp pos)
        (goto-char (1+ (car pos)))
        (set-mark (1+ (cdr pos))))
       (pos
        (goto-char (1+ pos))))
      ;; exercise — pass t as arg when mark is set so tempo uses the region.
      ;; Bind `this-command' and `current-prefix-arg' so that
      ;; `adoc-tempo-on-region' (which inspects these to detect region
      ;; mode) returns the correct value during template expansion.
      (if (consp pos)
          (let ((this-command (car transform))
                (current-prefix-arg t))
            (funcall (car transform) t))
        (eval transform))
      ;; verify
      (should (string-equal (buffer-substring (point-min) (point-max)) expected-text)))))

;; We define our own generic mode for testing code blocks.
;; All other languages except adoc can change fontification without us noticing.
;; Adoc in a code block is a good test case, but it should not be used for the
;; simplest test case. Use `adoctest-lang-mode' instead.
(define-generic-mode adoctest-lang-mode
  '(("//" . nil) ("/*" . "*/")) ;; cpp-like comment syntax
  '("if" "else" "for" "while" "do" "break" "continue" "throw" "catch") ;; some keywords from c/cpp
  nil ;; no additional entries for font-lock-keywords 
  nil ;; no entries for auto-mode-alist
  nil ;; no additional actions
  "Mode for testing code blocks in `adoc-mode'.
Don't use it for anything real.")

(defmacro adoctest-with-uncustomized-vars (vars &rest body)
  "Run BODY without customization of VARS."
  (declare (debug (list body)) (indent 1))
  `(let ,(mapcar
	  (lambda (var)
	    (cons var (get var 'standard-value)))
	  vars)
     ,@body))


;;;; Actual Tests
(ert-deftest adoctest-test-titles-simple-one-line-before ()
  (adoctest-faces "titles-simple-one-line-before"
                  "= " 'adoc-meta-hide-face "document title" 'adoc-title-0-face "\n" nil
                  "\n" nil
                  "== " 'adoc-meta-hide-face "chapter 1" 'adoc-title-1-face "\n" nil
                  "\n" nil
                  "=== " 'adoc-meta-hide-face "chapter 2" 'adoc-title-2-face "\n" nil
                  "\n" nil
                  "==== " 'adoc-meta-hide-face "chapter 3" 'adoc-title-3-face "\n" nil
                  "\n" nil
                  "===== " 'adoc-meta-hide-face "chapter 4" 'adoc-title-4-face "\n" nil
                  "\n" nil
                  "====== " 'adoc-meta-hide-face "chapter 5" 'adoc-title-5-face "\n" nil))

(ert-deftest adoctest-test-titles-simple-one-line-enclosed ()
  (adoctest-faces "titles-simple-one-line-enclosed"
                  "= " 'adoc-meta-hide-face "document title" 'adoc-title-0-face " =" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "== " 'adoc-meta-hide-face "chapter 1" 'adoc-title-1-face " ==" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "=== " 'adoc-meta-hide-face "chapter 2" 'adoc-title-2-face " ===" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "==== " 'adoc-meta-hide-face "chapter 3" 'adoc-title-3-face " ====" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "===== " 'adoc-meta-hide-face "chapter 4" 'adoc-title-4-face " =====" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "====== " 'adoc-meta-hide-face "chapter 5" 'adoc-title-5-face " ======" 'adoc-meta-hide-face "\n" nil))

(ert-deftest adoctest-test-titles-simple-two-line ()
  (let ((adoc-enable-two-line-title t))
    (adoctest-faces "titles-simple-two-line"
                    "document title" 'adoc-title-0-face "\n" nil
                    "==============" 'adoc-meta-hide-face "\n" nil
                    "\n" nil
                    "chapter 1" 'adoc-title-1-face "\n" nil
                    "---------" 'adoc-meta-hide-face "\n" nil
                    "\n" nil
                    "chapter 2" 'adoc-title-2-face "\n" nil
                    "~~~~~~~~~" 'adoc-meta-hide-face "\n" nil
                    "\n" nil
                    "chapter 3" 'adoc-title-3-face "\n" nil
                    "^^^^^^^^^" 'adoc-meta-hide-face "\n" nil
                    "\n" nil
                    "chapter 4" 'adoc-title-4-face "\n" nil
                    "+++++++++" 'adoc-meta-hide-face)))

(ert-deftest adoctest-test-titles-simple-block-title ()
  (adoctest-faces "titles-simple-block-title"
                  "." 'adoc-meta-face "Block title" 'adoc-gen-face))

(ert-deftest adoctest-test-delimited-blocks-simple ()
  (adoctest-faces "delimited-blocks-simple"

                  ;; note that the leading spaces are NOT allowed to have adoc-align face
                  "////////" 'adoc-meta-hide-face "\n" nil
                  " comment line 1\n comment line 2" 'adoc-comment-face "\n" nil
                  "////////" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "++++++++" 'adoc-meta-hide-face "\n" nil
                  " passthrouh line 1\n passthrouh line 2" 'adoc-passthrough-face "\n" nil
                  "++++++++" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "--------" 'adoc-meta-hide-face "\n" nil
                  " listing line 1\n listing line 2" 'adoc-code-face "\n" nil
                  "--------" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "........" 'adoc-meta-hide-face "\n" nil
                  " literal line 1\n literal line 2" 'adoc-verbatim-face "\n" nil
                  "........" 'adoc-meta-hide-face "\n" nil
                  "\n" nil

                  "________" 'adoc-meta-hide-face "\n" nil
                  "quote line 1\nquote line 2" nil "\n" nil
                  "________" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "========" 'adoc-meta-hide-face "\n" nil
                  "example line 1\nexample line 2" nil "\n" nil
                  "========" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "********" 'adoc-meta-hide-face "\n" nil
                  "sidebar line 1\nsidebar line 2" 'adoc-secondary-text-face "\n" nil
                  "********" 'adoc-meta-hide-face "\n"))

;; Don't mistake text between two same delimited blocks as a delimited block,
;; i.e. wrongly treating the end of block 1 as a beginning and wrongly
;; treating the beginning of block 2 as ending.
(ert-deftest adoctest-test-delimited-blocks-special-case ()
  (adoctest-faces "delimited-blocks-special-case"

                  "--------" 'adoc-meta-hide-face "\n" nil
                  "11\n12\n13\n14" 'adoc-code-face "\n" nil
                  "--------" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "lorem" 'no-face "\n" nil
                  "\n" nil
                  "--------" 'adoc-meta-hide-face "\n" nil
                  "21\n22\n23\n24" 'adoc-code-face "\n" nil
                  "--------" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "ipsum" 'no-face "\n" nil))

(ert-deftest adoctest-test-delimited-blocks-empty ()
  (adoctest-faces "delimited-blocks-empty"
                  "////////" 'adoc-meta-hide-face "\n" nil
                  "////////" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "++++++++" 'adoc-meta-hide-face "\n" nil
                  "++++++++" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "--------" 'adoc-meta-hide-face "\n" nil
                  "--------" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "........" 'adoc-meta-hide-face "\n" nil
                  "........" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "________" 'adoc-meta-hide-face "\n" nil
                  "________" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "========" 'adoc-meta-hide-face "\n" nil
                  "========" 'adoc-meta-hide-face "\n" nil
                  "\n" nil
                  "********" 'adoc-meta-hide-face "\n" nil
                  "********" 'adoc-meta-hide-face "\n"))

;; Regression test for https://github.com/bbatsov/adoc-mode/issues/64
(ert-deftest adoctest-test-passthrough-minimal ()
  (adoctest-faces "passthrough-minimal"
                  "++++" 'adoc-meta-hide-face "\n" nil
                  "\\lambda_{T}" 'adoc-passthrough-face "\n" nil
                  "++++" 'adoc-meta-hide-face))

(ert-deftest adoctest-test-open-block ()
  (adoctest-faces "open-block"
                  "--" 'adoc-meta-hide-face "\n" nil
                  "open block line 1\nopen block line 2" nil "\n" nil
                  "--" 'adoc-meta-hide-face))

(ert-deftest adoctest-test-comments ()
  (adoctest-faces "comments"
                  ;; as block macro
                  "// lorem ipsum\n" 'adoc-comment-face
                  "\n" nil
                  ;; as inline macro
                  "lorem ipsum\n" 'no-face
                  "// dolor sit\n" 'adoc-comment-face
                  "amen\n" 'no-face
                  "\n" nil
                  ;; block macro and end of buffer
                  "// lorem ipsum" 'adoc-comment-face
                  ;; as delimited block it's tested in delimited-blocks-simple
                  ))

(ert-deftest adoctest-test-code-blocks ()
  (adoctest-with-uncustomized-vars
      (adoc-fontify-code-blocks-natively
       adoc-code-lang-modes
       adoc-fontify-code-block-default-mode
       adoc-font-lock-extend-after-change-max)
    (let ((source-code
	   (list
	    "if" '(font-lock-keyword-face adoc-native-code-face)
	    "\n" '(adoc-native-code-face)
	    "//" '(font-lock-comment-delimiter-face adoc-native-code-face)
	    "comment" '(font-lock-comment-face adoc-native-code-face)
	    )))
      (adoctest-faces
       "code-block-natively"
       ;; Code block as LISTING
       "\n" nil
       "[source,adoctest-lang]\n----\n" 'adoc-meta-face
       source-code
       "\n" 'adoc-native-code-face
       "----" 'adoc-meta-face
       "\n" nil
       ;; Code blocks without language attribute
       "[source]\n----\n" 'adoc-meta-face
       (apply #'concat (cl-loop for str in source-code by #'cddr collect str)) '(adoc-verbatim-face adoc-code-face)
       "\n" 'adoc-code-face
       "----" 'adoc-meta-face
       "\n" nil
       ;; Code block as OPEN BLOCK
       "\n" nil
       "[source,adoctest-lang]\n--\n" 'adoc-meta-face
       source-code
       "\n" 'adoc-native-code-face
       "--" 'adoc-meta-face
       "\n" nil
       ;; Code block as Literal block
       "[source,adoctest-lang]\n....\n" 'adoc-meta-face
       source-code
       "\n" 'adoc-native-code-face
       "...." 'adoc-meta-face
       "\n" nil
       ;; Test ignored spaces
       "[source,\t adoctest-lang]\t \n....\n" 'adoc-meta-face
       source-code
       "\n" 'adoc-native-code-face
       "...." 'adoc-meta-face
       "\n" nil
       ))))

(ert-deftest adoctest-test-anchors ()
  (adoctest-faces "anchors"
                  ;; block id
                  "[[" 'adoc-meta-face "foo" 'adoc-anchor-face "]]" 'adoc-meta-face "\n" nil
                  "[[" 'adoc-meta-face "foo" 'adoc-anchor-face "," 'adoc-meta-face
                  "bar" 'adoc-secondary-text-face "]]" 'adoc-meta-face "\n" nil

                  ;; special inline syntax: [[id]] [[id,xreftext]]
                  "lorem " 'no-face "[[" 'adoc-meta-face "foo" 'adoc-anchor-face "]]"
                  'adoc-meta-face "ipsum" 'no-face "\n" nil
                  "lorem " 'no-face "[[" 'adoc-meta-face "foo" 'adoc-anchor-face "," 'adoc-meta-face
                  "bla bli bla blu" 'adoc-secondary-text-face "]]" 'adoc-meta-face "ipsum" 'no-face "\n" nil

                  ;; general inline macro syntax
                  "lorem " 'no-face "anchor" 'adoc-command-face ":" 'adoc-meta-face
                  "foo" 'adoc-anchor-face
                  "[]" 'adoc-meta-face "ipsum" 'no-face "\n" nil
                  "lorem " 'no-face "anchor" 'adoc-command-face ":" 'adoc-meta-face
                  "foo" 'adoc-anchor-face
                  "[" 'adoc-meta-face "bla bli bla blu" 'adoc-secondary-text-face "]" 'adoc-meta-face
                  "ipsum" 'no-face "\n" nil

                  ;; biblio
                  "lorem " 'no-face "[[" 'adoc-meta-face "[foo]" 'adoc-gen-face "]]" 'adoc-meta-face
                  " ipsum" 'no-face
                  ))

(ert-deftest adoctest-test-references ()
  (adoctest-faces "references"
                  "lorem " 'no-face "xref" 'adoc-command-face ":" 'adoc-meta-face
                  "foo" 'adoc-reference-face "[]" 'adoc-meta-face "\n" nil
                  "lorem " 'no-face "xref" 'adoc-command-face ":" 'adoc-meta-face
                  "foo" 'adoc-internal-reference-face "[" 'adoc-meta-face
                  "bla bli bla blu" 'adoc-reference-face "]" 'adoc-meta-face "\n" nil

                  ;; caption spawns multiple lines
                  "xref" 'adoc-command-face ":" 'adoc-meta-face
                  "foo" 'adoc-internal-reference-face "[" 'adoc-meta-face
                  "bla\nbli\nbla\nblu" 'adoc-reference-face "]" 'adoc-meta-face "\n" nil
                  ))

(ert-deftest adoctest-test-footnotes ()
  (adoctest-faces "footnotes"
                  ;; simple example
                  "footnote" 'adoc-footnote-marker-face ":" 'adoc-meta-face
                  "[" 'adoc-meta-face "lorem ipsum" 'adoc-footnote-text-face
                  "]" 'adoc-meta-face "\n" nil

                  ;; footnote can be hard up against the preceding word
                  "lorem" 'no-face "footnote" 'adoc-footnote-marker-face ":" 'adoc-meta-face
                  "[" 'adoc-meta-face "ipsum" 'adoc-footnote-text-face
                  "]" 'adoc-meta-face "\n" nil

                  ;; attribute-list is not really an attribute list but normal text,
                  ;; i.e. comma, equal, double quotes are not fontified as meta characters
                  "footnote" 'adoc-footnote-marker-face ":" 'adoc-meta-face
                  "[" 'adoc-meta-face
                  "lorem, ipsum=dolor, sit=\"amen\"" 'adoc-footnote-text-face
                  "]" 'adoc-meta-face "\n" nil

                  ;; multiline attribute list
                  "footnote" 'adoc-footnote-marker-face ":" 'adoc-meta-face
                  "[" 'adoc-meta-face
                  "lorem\nipsum\ndolor\nsit\namen" 'adoc-footnote-text-face
                  "]" 'adoc-meta-face "\n" nil
                  ))

(ert-deftest adoctest-test-footnoterefs ()
  (adoctest-faces "footnoterefs"
                  ;; simple example
                  "footnoteref" 'adoc-footnote-marker-face ":" 'adoc-meta-face
                  "[" 'adoc-meta-face
                  "myid" 'adoc-internal-reference-face
                  "]" 'adoc-meta-face "\n" nil

                  "footnoteref" 'adoc-footnote-marker-face ":" 'adoc-meta-face
                  "[" 'adoc-meta-face
                  "myid" 'adoc-anchor-face
                  "," 'adoc-meta-face
                  "lorem ipsum" 'adoc-footnote-text-face
                  "]" 'adoc-meta-face "\n" nil

                  ;; footnoteref can be hard up against the preceding word
                  "lorem" 'no-face
                  "footnoteref" 'adoc-footnote-marker-face ":" 'adoc-meta-face
                  "[" 'adoc-meta-face
                  "myid" 'adoc-internal-reference-face
                  "]" 'adoc-meta-face "\n" nil

                  "lorem" 'no-face
                  "footnoteref" 'adoc-footnote-marker-face ":" 'adoc-meta-face
                  "[" 'adoc-meta-face
                  "myid" 'adoc-anchor-face
                  "," 'adoc-meta-face
                  "lorem ipsum" 'adoc-footnote-text-face
                  "]" 'adoc-meta-face "\n" nil

                  ;; multiline text
                  "lorem" 'no-face
                  "footnoteref" 'adoc-footnote-marker-face ":" 'adoc-meta-face
                  "[" 'adoc-meta-face
                  "myid" 'adoc-anchor-face
                  "," 'adoc-meta-face
                  "lorem\nipsum\ndolor\nsit" 'adoc-footnote-text-face
                  "]" 'adoc-meta-face "\n" nil
                  ))

(ert-deftest adoctest-test-attribute-list ()
  (adoctest-faces "attribute-list"
                  ;; positional attribute
                  "[" 'adoc-meta-face "hello" 'adoc-value-face "]" 'adoc-meta-face "\n" nil
                  ;; positional attribute containing spaces
                  "[" 'adoc-meta-face "hello world" 'adoc-value-face "]" 'adoc-meta-face "\n" nil
                  ;; positional attribute as string
                  "[\"" 'adoc-meta-face  "hello world" 'adoc-value-face "\"]" 'adoc-meta-face "\n" nil

                  ;; multiple positional attributes
                  "[" 'adoc-meta-face "hello" 'adoc-value-face "," 'adoc-meta-face "world" 'adoc-value-face "]" 'adoc-meta-face "\n" nil

                  ;; multiple positional attributes, however one or both are empty (really empty or only one space)
                  "[" 'adoc-meta-face "hello" 'adoc-value-face ",]" 'adoc-meta-face "\n" nil
                  "[" 'adoc-meta-face "hello" 'adoc-value-face "," 'adoc-meta-face " " 'adoc-value-face "]" 'adoc-meta-face "\n" nil
                  "[," 'adoc-meta-face "hello" 'adoc-value-face "]" 'adoc-meta-face "\n" nil
                  "[" 'adoc-meta-face " " 'adoc-value-face "," 'adoc-meta-face "hello" 'adoc-value-face "]" 'adoc-meta-face "\n" nil
                  "[,]" 'adoc-meta-face "\n" nil
                  "[," 'adoc-meta-face " " 'adoc-value-face "]" 'adoc-meta-face "\n" nil
                  "[" 'adoc-meta-face " " 'adoc-value-face ",]" 'adoc-meta-face "\n" nil
                  "[" 'adoc-meta-face " " 'adoc-value-face "," 'adoc-meta-face " " 'adoc-value-face "]" 'adoc-meta-face "\n" nil

                  ;; zero positional attributes
                  "[]" 'adoc-meta-face "\n" nil
                  "[" 'adoc-meta-face " " 'adoc-value-face "]" 'adoc-meta-face "\n" nil

                  ;; keyword attribute
                  "[" 'adoc-meta-face "salute" 'adoc-attribute-face "=" 'adoc-meta-face "hello" 'adoc-value-face "]" 'adoc-meta-face "\n" nil
                  ;; keyword attribute where value is a string
                  "[" 'adoc-meta-face "salute" 'adoc-attribute-face "=\"" 'adoc-meta-face "hello world" 'adoc-value-face "\"]" 'adoc-meta-face "\n" nil

                  ;; multiple positional attributes, multiple keyword attributes
                  "[" 'adoc-meta-face "lorem" 'adoc-value-face "," 'adoc-meta-face "ipsum" 'adoc-value-face "," 'adoc-meta-face
                  "dolor" 'adoc-attribute-face "=" 'adoc-meta-face "sit" 'adoc-value-face "," 'adoc-meta-face
                  "dolor" 'adoc-attribute-face "=" 'adoc-meta-face "sit" 'adoc-value-face "]" 'adoc-meta-face "\n" nil

                  ;; is , within strings really part of the string and not mistaken as element separator
                  "[\"" 'adoc-meta-face "lorem,ipsum=dolor" 'adoc-value-face "\"]" 'adoc-meta-face "\n" nil
                  ;; does escaping " in strings work
                  "[\"" 'adoc-meta-face "lorem \\\"ipsum\\\" dolor" 'adoc-value-face "\"]" 'adoc-meta-face
                  ))

;; Regression test for https://github.com/bbatsov/adoc-mode/issues/57
(ert-deftest adoctest-test-attribute-reference ()
  (adoctest-faces "attribute-reference"
                  ;; simple attribute reference
                  "{" 'adoc-replacement-face "foo" 'adoc-replacement-face "}" 'adoc-replacement-face "\n" nil
                  ;; hyphenated attribute
                  "{" 'adoc-replacement-face "my-attr" 'adoc-replacement-face "}" 'adoc-replacement-face "\n" nil
                  ;; escaped braces must not hang or get highlighted
                  "\\{not-an-attr\\}" 'no-face "\n" nil))

(ert-deftest adoctest-test-block-macro ()
  (adoctest-faces "block-macro"
                  "lorem" 'adoc-command-face "::" 'adoc-meta-face "ipsum[]" 'adoc-meta-face))

(ert-deftest adoctest-test-quotes-simple ()
  (adoctest-faces "test-quotes-simple"
                  ;; note that in unconstraned quotes cases " ipsum " has spaces around, in
                  ;; constrained quotes case it doesn't
                  "Lorem " nil "``" 'adoc-meta-hide-face "ip" '(adoc-typewriter-face adoc-verbatim-face) "``" 'adoc-meta-hide-face "sum dolor\n" nil
                  "Lorem " nil "`" 'adoc-meta-hide-face "ipsum" '(adoc-typewriter-face adoc-verbatim-face) "`" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "+++" 'adoc-meta-hide-face " ipsum " '(adoc-typewriter-face adoc-verbatim-face) "+++" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "$$" 'adoc-meta-hide-face " ipsum " '(adoc-typewriter-face adoc-verbatim-face) "$$" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "**" 'adoc-meta-hide-face " ipsum " 'adoc-bold-face "**" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "*" 'adoc-meta-hide-face "ipsum" 'adoc-bold-face "*" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "++" 'adoc-meta-hide-face " ipsum " 'adoc-typewriter-face "++" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "+" 'adoc-meta-hide-face "ipsum" 'adoc-typewriter-face "+" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "__" 'adoc-meta-hide-face " ipsum " 'adoc-emphasis-face "__" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "_" 'adoc-meta-hide-face "ipsum" 'adoc-emphasis-face "_" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "##" 'adoc-meta-hide-face " ipsum " 'adoc-highlight-face "##" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "#" 'adoc-meta-hide-face "ipsum" 'adoc-highlight-face "#" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "~" 'adoc-meta-hide-face " ipsum " 'adoc-subscript-face "~" 'adoc-meta-hide-face " dolor\n" nil
                  "Lorem " nil "^" 'adoc-meta-hide-face " ipsum " 'adoc-superscript-face "^" 'adoc-meta-hide-face " dolor"))

(ert-deftest adoctest-test-quotes-medium ()
  (let ((adoc-enable-two-line-title t))
    (adoctest-faces "test-quotes-medium"
                    ;; test wheter constrained/unconstrained quotes can spawn multiple lines
                    "Lorem " 'no-face "*" 'adoc-meta-hide-face "ipsum" 'adoc-bold-face
                    "\n" nil "dolor" 'adoc-bold-face "\n" nil "dolor" 'adoc-bold-face
                    "\n" nil "dolor" 'adoc-bold-face "\n" nil "dolor" 'adoc-bold-face
                    "*" 'adoc-meta-hide-face
                    " sit" 'no-face "\n" nil

                    "Lorem " 'no-face "__" 'adoc-meta-hide-face "ipsum" 'adoc-emphasis-face
                    "\n" nil "dolor" 'adoc-emphasis-face "\n" nil "dolor" 'adoc-emphasis-face
                    "\n" nil "dolor" 'adoc-emphasis-face "\n" nil "dolor" 'adoc-emphasis-face
                    "__" 'adoc-meta-hide-face
                    " sit" 'no-face "\n" nil

                    ;; tests border case that delimiter is at the beginnin/end of an paragraph/line
                    ;; constrained at beginning
                    "*" 'adoc-meta-hide-face "lorem" 'adoc-bold-face "*" 'adoc-meta-hide-face " ipsum\n" 'no-face
                    "\n" nil
                    ;; constrained at end
                    "lorem " 'no-face "*" 'adoc-meta-hide-face "ipsum" 'adoc-bold-face "*" 'adoc-meta-hide-face "\n" nil
                    "\n" nil
                    ;; constrained from beginning to end
                    "*" 'adoc-meta-hide-face "lorem" 'adoc-bold-face "*" 'adoc-meta-hide-face "\n" nil
                    "\n" nil
                    ;; unconstrained at beginning. Note that "** " at the beginning of a line would be a list item.
                    "__" 'adoc-meta-hide-face " lorem " 'adoc-emphasis-face "__" 'adoc-meta-hide-face " ipsum\n" 'no-face
                    "\n" nil
                    ;; unconstrained at end
                    "lorem " 'no-face "__" 'adoc-meta-hide-face " ipsum " 'adoc-emphasis-face "__" 'adoc-meta-hide-face "\n" nil
                    "\n" nil
                    ;; unconstrained from beginning to end
                    "__" 'adoc-meta-hide-face " lorem " 'adoc-emphasis-face "__" 'adoc-meta-hide-face "\n" nil
                    "\n" nil

                    ;; test wheter quotes can nest
                    ;; done by meta-face-cleanup

                    ;; tests that quotes work within titles / labeled lists
                    "== " 'adoc-meta-hide-face "chapter " 'adoc-title-1-face "*" 'adoc-meta-hide-face "1" '(adoc-title-1-face adoc-bold-face) "*" 'adoc-meta-hide-face " ==" 'adoc-meta-hide-face "\n" nil
                    "\n" nil
                    "chapter " 'adoc-title-2-face "_" 'adoc-meta-hide-face "2" '(adoc-title-2-face adoc-emphasis-face) "_" 'adoc-meta-hide-face "\n" nil
                    "~~~~~~~~~~~" 'adoc-meta-hide-face "\n" nil
                    "." 'adoc-meta-face "lorem " 'adoc-gen-face "_" 'adoc-meta-hide-face "ipsum" '(adoc-gen-face adoc-emphasis-face) "_" 'adoc-meta-hide-face "\n" nil
                    "\n" nil
                    "lorem " 'adoc-gen-face "+" 'adoc-meta-hide-face "ipsum" '(adoc-gen-face adoc-typewriter-face) "+" 'adoc-meta-hide-face " sit" 'adoc-gen-face "::" 'adoc-list-face " " 'adoc-align-face
                    )))

;; test border cases where the quote delimiter is at the beginning and/or the
;; end of the buffer
(ert-deftest adoctest-test-quotes-medium-2 ()
  (adoctest-faces "test-quotes-medium-2"
                  "*" 'adoc-meta-hide-face "lorem" 'adoc-bold-face "*" 'adoc-meta-hide-face " ipsum" 'no-face))
(ert-deftest adoctest-test-quotes-medium-3 ()
  (adoctest-faces "test-quotes-medium-3"
                  "lorem " 'no-face "*" 'adoc-meta-hide-face "ipsum" 'adoc-bold-face "*" 'adoc-meta-hide-face))
(ert-deftest adoctest-test-quotes-medium-4 ()
  (adoctest-faces "test-quotes-medium-4"
                  "*" 'adoc-meta-hide-face "lorem" 'adoc-bold-face "*" 'adoc-meta-hide-face))

(ert-deftest adoctest-test-lists-simple ()
  (adoctest-faces "test-lists-simple"
                  " " 'adoc-align-face "-" 'adoc-list-face " " 'adoc-align-face "uo list item\n" nil
                  " " 'adoc-align-face "*" 'adoc-list-face " " 'adoc-align-face "uo list item\n" nil
                  " " 'adoc-align-face "**" 'adoc-list-face " " 'adoc-align-face "uo list item\n" nil
                  " " 'adoc-align-face "***" 'adoc-list-face " " 'adoc-align-face "uo list item\n" nil
                  " " 'adoc-align-face "****" 'adoc-list-face " " 'adoc-align-face "uo list item\n" nil
                  " " 'adoc-align-face "*****" 'adoc-list-face " " 'adoc-align-face "uo list item\n" nil
                  "+" 'adoc-list-face " " 'adoc-align-face "uo list item\n" nil

                  " " 'adoc-align-face "1." 'adoc-list-face " " 'adoc-align-face "o list item\n" nil
                  " " 'adoc-align-face "a." 'adoc-list-face " " 'adoc-align-face "o list item\n" nil
                  " " 'adoc-align-face "B." 'adoc-list-face " " 'adoc-align-face "o list item\n" nil
                  " " 'adoc-align-face "ii)" 'adoc-list-face " " 'adoc-align-face "o list item\n" nil
                  " " 'adoc-align-face "II)" 'adoc-list-face " " 'adoc-align-face "o list item\n" nil

                  " " 'adoc-align-face "." 'adoc-list-face " " 'adoc-align-face "implicitly numbered list item\n" nil
                  " " 'adoc-align-face ".." 'adoc-list-face " " 'adoc-align-face "implicitly numbered list item\n" nil
                  " " 'adoc-align-face "..." 'adoc-list-face " " 'adoc-align-face "implicitly numbered list item\n" nil
                  " " 'adoc-align-face "...." 'adoc-list-face " " 'adoc-align-face "implicitly numbered list item\n" nil
                  " " 'adoc-align-face "....." 'adoc-list-face " " 'adoc-align-face "implicitly numbered list item\n" nil
                  "<1>" 'adoc-list-face " " 'adoc-align-face "callout\n" nil
                  "1>" 'adoc-list-face " " 'adoc-align-face "callout\n" nil

                  " " 'adoc-align-face "term" 'adoc-gen-face "::" 'adoc-list-face " " 'adoc-align-face "lorem ipsum\n" nil
                  " " 'adoc-align-face "term" 'adoc-gen-face ";;" 'adoc-list-face " " 'adoc-align-face "lorem ipsum\n" nil
                  " " 'adoc-align-face "term" 'adoc-gen-face ":::" 'adoc-list-face " " 'adoc-align-face "lorem ipsum\n" nil
                  " " 'adoc-align-face "term" 'adoc-gen-face "::::" 'adoc-list-face " " 'adoc-align-face "lorem ipsum\n" nil
                  " " 'adoc-align-face "question" 'adoc-gen-face "??" 'adoc-list-face "\n" nil
                  "glossary" 'adoc-gen-face ":-" 'adoc-list-face "\n" nil

                  "-" 'adoc-list-face " " 'adoc-align-face "uo list item\n" nil
                  "+" 'adoc-meta-face "\n" nil
                  "2nd list paragraph\n" nil ))

(ert-deftest adoctest-test-lists-medium ()
  (adoctest-faces "test-lists-medium"
                  ;; white box test: labeled list item font lock keyword is implemented
                  ;; specially in that it puts adoc-reserved text-property on the preceding
                  ;; newline. However it shall deal with the situation that there is no
                  ;; preceding newline, because we're at the beginning of the buffer
                  "lorem" 'adoc-gen-face "::" 'adoc-list-face " " nil "ipsum" 'no-face))

(ert-deftest adoctest-test-inline-macros ()
  (adoctest-faces "inline-macros"
                  "commandname" 'adoc-command-face ":target[" 'adoc-meta-face "attribute list" 'adoc-value-face "]" 'adoc-meta-face))

(ert-deftest adoctest-test-asciidoctor-inline-macros ()
  ;; kbd macro
  (adoctest-faces "asciidoctor-kbd-macro"
                  "Press " nil "kbd" 'adoc-command-face ":[" 'adoc-meta-face "Ctrl+C" 'adoc-value-face "]" 'adoc-meta-face)
  ;; btn macro
  (adoctest-faces "asciidoctor-btn-macro"
                  "Click " nil "btn" 'adoc-command-face ":[" 'adoc-meta-face "OK" 'adoc-value-face "]" 'adoc-meta-face)
  ;; pass macro
  (adoctest-faces "asciidoctor-pass-macro"
                  "Use " nil "pass" 'adoc-command-face ":[" 'adoc-meta-face "raw content" 'adoc-value-face "]" 'adoc-meta-face))

(ert-deftest adoctest-test-meta-face-cleanup ()
  ;; begin with a few simple explicit cases which are easier to debug in case of troubles

  ;; 1) test that meta characters always only have a single meta and don't get
  ;;    adoc-bold/-emphasis/... face just because they are within a
  ;;    bold/emphasis/... construct.
  ;; 2) test that nested quotes really apply the faces of both quotes to the inner text
  (adoctest-faces "meta-face-cleanup-1"
                  "*" 'adoc-meta-hide-face "lorem " 'adoc-bold-face
                  "_" 'adoc-meta-hide-face "ipsum" '(adoc-bold-face adoc-emphasis-face) "_" 'adoc-meta-hide-face
                  " dolor" 'adoc-bold-face "*" 'adoc-meta-hide-face "\n" nil)
  (adoctest-faces "meta-face-cleanup-2"
                  "_" 'adoc-meta-hide-face "lorem " 'adoc-emphasis-face
                  "*" 'adoc-meta-hide-face "ipsum" '(adoc-bold-face adoc-emphasis-face) "*" 'adoc-meta-hide-face
                  " dolor" 'adoc-emphasis-face "_" 'adoc-meta-hide-face)

  ;; now test all possible cases
  ;; mmm, that is all possible cases inbetween constrained/unconstrained quotes

  ;; .... todo
  )

(ert-deftest adoctest-test-role-faces ()
  "Role-aware fontification of [.role]#text# / [role]#text# spans.
The role face is layered on top of the surrounding quote's default
face (matching how Asciidoctor's stylesheet renders the role class
on top of the `<mark>' default)."
  (adoctest-faces "role-line-through"
                  ;; Asciidoctor preferred syntax: leading dot
                  "Lorem " nil
                  "[.line-through]" 'adoc-meta-face
                  "#" 'adoc-meta-hide-face
                  "ipsum" '(adoc-strike-through-face adoc-highlight-face)
                  "#" 'adoc-meta-hide-face " dolor" nil)
  (adoctest-faces "role-line-through-legacy"
                  ;; Legacy syntax without leading dot
                  "Lorem " nil
                  "[line-through]" 'adoc-meta-face
                  "#" 'adoc-meta-hide-face
                  "ipsum" '(adoc-strike-through-face adoc-highlight-face)
                  "#" 'adoc-meta-hide-face " dolor" nil)
  (adoctest-faces "role-underline"
                  "Lorem " nil
                  "[.underline]" 'adoc-meta-face
                  "#" 'adoc-meta-hide-face
                  "ipsum" '(adoc-underline-face adoc-highlight-face)
                  "#" 'adoc-meta-hide-face " dolor" nil)
  (adoctest-faces "role-overline"
                  "Lorem " nil
                  "[.overline]" 'adoc-meta-face
                  "#" 'adoc-meta-hide-face
                  "ipsum" '(adoc-overline-face adoc-highlight-face)
                  "#" 'adoc-meta-hide-face " dolor" nil)
  (adoctest-faces "role-with-id"
                  ;; [.role#id] should still dispatch on role
                  "Lorem " nil
                  "[.line-through#myid]" 'adoc-meta-face
                  "#" 'adoc-meta-hide-face
                  "ipsum" '(adoc-strike-through-face adoc-highlight-face)
                  "#" 'adoc-meta-hide-face " dolor" nil)
  ;; Role combines with the surrounding quote's default face
  (adoctest-faces "role-combines-with-bold"
                  "Lorem " nil
                  "[.line-through]" 'adoc-meta-face
                  "*" 'adoc-meta-hide-face
                  "ipsum" '(adoc-strike-through-face adoc-bold-face)
                  "*" 'adoc-meta-hide-face " dolor" nil)
  ;; Unknown role: text-face falls back to the default
  (adoctest-faces "role-unknown"
                  "Lorem " nil
                  "[.frobnicate]" 'adoc-meta-face
                  "#" 'adoc-meta-hide-face "ipsum" 'adoc-highlight-face
                  "#" 'adoc-meta-hide-face " dolor" nil))

(ert-deftest adoctest-test-url ()
  (adoctest-faces "url"
                  ;; url inline macro with attriblist
                  "foo " nil
                  "http://www.lorem.com/ipsum.html" 'adoc-url-face
                  "[" 'adoc-meta-face "sit amet" 'adoc-reference-face "]" 'adoc-meta-face
                  " bar \n" nil
                  ;; link text contains newlines and commas
                  "http://www.lorem.com/ipsum.html" 'adoc-url-face
                  "[" 'adoc-meta-face
                  "sit,\namet,\nconsectetur" 'adoc-reference-face
                  "]" 'adoc-meta-face
                  " bar \n" nil
                  ;; url inline macro withOUT attriblist
                  "http://www.lorem.com/ipsum.html" 'adoc-url-face
                  "[]" 'adoc-meta-face
                  " bar \n" nil
                  ;; plain url
                  "http://www.lorem.com/ipsum.html" 'adoc-url-face
                  " foo " nil "joe.bloggs@foobar.com" 'adoc-url-face ))

(ert-deftest adoctest-test-url-enclosing-quote ()
  (adoctest-faces "url-enclosing-quote"
                  ;; spaces between __ and url seem really to be needed also in asciidoc
                  "foo " nil "__" 'adoc-meta-hide-face " " nil
                  "http://www.lorem.com/ipsum.html" '(adoc-emphasis-face adoc-url-face)
                  " " nil "__" 'adoc-meta-hide-face

                  "\nfoo " nil
                  "**" 'adoc-meta-hide-face " " nil
                  "joe.bloggs@foobar.com" '(adoc-bold-face adoc-url-face)
                  " " nil "**" 'adoc-meta-hide-face ))

;; inline substitutions only within the block they belong to. I.e. don't cross
;; block boundaries.
(ert-deftest adoctest-test-inline-subst-boundaries ()
  (let ((adoc-enable-two-line-title t))
  (adoctest-faces "inline-subst-boundaries"

                  ;; 1) don't cross title boundaries.
                  ;; 2) don't cross paragraph boundaries.
                  ;; 3) verify that the (un)constrained quotes would work however
                  "== " 'adoc-meta-hide-face "chapter ** 1" 'adoc-title-1-face "\n" nil
                  "lorem ** ipsum\n" 'no-face
                  "\n" nil
                  "lorem " 'no-face "**" 'adoc-meta-hide-face " ipsum " 'adoc-bold-face "**" 'adoc-meta-hide-face "\n" nil
                  "\n" nil

                  "== " 'adoc-meta-hide-face "chapter __ 1" 'adoc-title-1-face " ==" 'adoc-meta-hide-face "\n" nil
                  "lorem __ ipsum\n" 'no-face
                  "\n" nil
                  "lorem " 'no-face "__" 'adoc-meta-hide-face " ipsum " 'adoc-emphasis-face "__" 'adoc-meta-hide-face "\n" nil
                  "\n" nil

                  "chapter ++ 1" 'adoc-title-1-face "\n" nil
                  "------------" 'adoc-meta-hide-face "\n" nil
                  "lorem ++ ipsum\n" 'no-face
                  "\n" nil
                  "lorem " 'no-face "++" 'adoc-meta-hide-face " ipsum " 'adoc-typewriter-face "++" 'adoc-meta-hide-face "\n" nil
                  "\n" nil

                  "." 'adoc-meta-face "block ^title" 'adoc-gen-face "\n" nil
                  "lorem^ ipsum\n" 'no-face
                  "\n" nil
                  "lorem " 'no-face "^" 'adoc-meta-hide-face " ipsum " 'adoc-superscript-face "^" 'adoc-meta-hide-face "\n" nil
                  "\n" nil

                  ;; Being able to use a ** that potentially could be mistaken as an end
                  ;; delimiter as start delimiter
                  "== " 'adoc-meta-hide-face "chapter ** 1" 'adoc-title-1-face "\n" nil
                  "lorem " 'no-face "**" 'adoc-meta-hide-face " ipsum " 'adoc-bold-face "**" 'adoc-meta-hide-face "\n" nil
                  "\n" nil

                  ;; don't cross list item boundaries
                  "-" 'adoc-list-face " " nil "lorem ** ipsum\n" 'no-face
                  "-" 'adoc-list-face " " nil "dolor ** sit\n" 'no-face
                  ;; test that a quote within the list element works
                  "-" 'adoc-list-face " " nil "dolor " 'no-face "**" 'adoc-meta-hide-face " sit " 'adoc-bold-face "**" 'adoc-meta-hide-face "\n" nil
                  ;; dont mistake '**' list elements for quote starters/enders
                  "**" 'adoc-list-face " " nil "lorem ** ipsum\n" 'no-face
                  "**" 'adoc-list-face " " nil "dolor ** sit\n" 'no-face
                  "**" 'adoc-list-face " " nil "dolor ** sit\n" 'no-face
                  ;; don't cross list item boundaries in the case of labeled lists
                  "lorem ** ipsum " 'adoc-gen-face "::" 'adoc-list-face " " nil "sit ** dolor\n" 'no-face
                  "lorem ** ipsum " 'adoc-gen-face "::" 'adoc-list-face " " nil "sit ** dolor" 'no-face)))

(ert-deftest adoctest-test-promote-title ()
  (adoctest-trans "= foo" "== foo" '(adoc-promote-title 1))
  ;; One-line titles span six levels (0-5), so level 5 wraps to level 0.
  (adoctest-trans "====== foo" "= foo" '(adoc-promote-title 1))
  (adoctest-trans "== foo" "==== foo" '(adoc-promote-title 2))

  (adoctest-trans "= foo =" "== foo ==" '(adoc-promote-title 1))
  (adoctest-trans "====== foo ======" "= foo =" '(adoc-promote-title 1))
  (adoctest-trans "== foo ==" "==== foo ====" '(adoc-promote-title 2))

  ;; Two-line titles span five levels (0-4), so promoting level 4 wraps to 0.
  (adoctest-trans "foo!\n===!" "foo\n---" '(adoc-promote-title 1))
  (adoctest-trans "foo!\n+++!" "foo\n===" '(adoc-promote-title 1))
  (adoctest-trans "foo!\n---!" "foo\n^^^" '(adoc-promote-title 2)))

;; since it's a whitebox test we know demote and promote only differ by inverse
;; arg. So demote doesn't need to be throuhly tested again
(ert-deftest adoctest-test-demote-title ()
  (adoctest-trans "= foo" "====== foo" '(adoc-demote-title 1))
  (adoctest-trans "= foo =" "====== foo ======" '(adoc-demote-title 1))
  (adoctest-trans "foo!\n===!" "foo\n+++" '(adoc-demote-title 1)))

;; todo: test after transition point is still on title lines
(ert-deftest adoctest-test-toggle-title-type ()
  (adoctest-trans "= one" "one\n===" '(adoc-toggle-title-type))
  (adoctest-trans "two!\n===!" "= two" '(adoc-toggle-title-type))
  (adoctest-trans "= three!\nbar" "three\n=====\nbar" '(adoc-toggle-title-type))
  (adoctest-trans "four!\n====!\nbar" "= four\nbar" '(adoc-toggle-title-type))
  (adoctest-trans "= five" "= five =" '(adoc-toggle-title-type t))
  (adoctest-trans "= six =" "= six" '(adoc-toggle-title-type t)))

(ert-deftest adoctest-test-adjust-title-del ()
  (adoctest-trans "lorem!\n===!" "lorem\n=====" '(adoc-adjust-title-del))
  (adoctest-trans "lorem!\n========!" "lorem\n=====" '(adoc-adjust-title-del))
  (adoctest-trans "lorem!\n=====!" "lorem\n=====" '(adoc-adjust-title-del)))

;;;; Heading navigation

(defun adoctest-insert-nav-doc ()
  "Insert a small multi-level document used by the navigation tests."
  (insert "= Doc Title\n\n"
          "intro\n\n"
          "== Section A\n\n"
          "text a\n\n"
          "=== Sub A1\n\n"
          "text\n\n"
          "=== Sub A2\n\n"
          "more\n\n"
          "== Section B\n\n"
          "end\n"))

(defun adoctest-goto-heading (text)
  "Move point to the beginning of the line containing TEXT."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(ert-deftest adoctest-test-next-visible-heading ()
  (with-temp-buffer
    (adoc-mode)
    (adoctest-insert-nav-doc)
    (goto-char (point-min))
    (adoc-next-visible-heading 1)
    (should (looking-at-p "== Section A$"))
    (adoc-next-visible-heading 1)
    (should (looking-at-p "=== Sub A1$"))
    ;; a count argument moves several headings at once
    (adoc-next-visible-heading 2)
    (should (looking-at-p "== Section B$"))
    ;; nothing past the last heading: point stays put, signals a user-error
    (let ((pos (point)))
      (should-error (adoc-next-visible-heading 1) :type 'user-error)
      (should (= (point) pos)))))

(ert-deftest adoctest-test-previous-visible-heading ()
  (with-temp-buffer
    (adoc-mode)
    (adoctest-insert-nav-doc)
    (adoctest-goto-heading "Section B")
    (adoc-previous-visible-heading 1)
    (should (looking-at-p "=== Sub A2$"))
    (adoc-previous-visible-heading 2)
    (should (looking-at-p "== Section A$"))
    ;; from within a section body it climbs to that section's heading
    (adoctest-goto-heading "text a")
    (adoc-previous-visible-heading 1)
    (should (looking-at-p "== Section A$"))
    ;; a negative argument moves forward
    (adoc-previous-visible-heading -1)
    (should (looking-at-p "=== Sub A1$"))))

(ert-deftest adoctest-test-forward-same-level ()
  (with-temp-buffer
    (adoc-mode)
    (adoctest-insert-nav-doc)
    ;; level-1 -> level-1, skipping the level-2 subsections in between
    (adoctest-goto-heading "Section A")
    (adoc-forward-same-level 1)
    (should (looking-at-p "== Section B$"))
    ;; level-2 -> level-2
    (adoctest-goto-heading "Sub A1")
    (adoc-forward-same-level 1)
    (should (looking-at-p "=== Sub A2$"))
    ;; a shallower heading stops the search; point is restored
    (adoctest-goto-heading "Sub A2")
    (let ((pos (point)))
      (should-error (adoc-forward-same-level 1) :type 'user-error)
      (should (= (point) pos)))))

(ert-deftest adoctest-test-backward-same-level ()
  (with-temp-buffer
    (adoc-mode)
    (adoctest-insert-nav-doc)
    (adoctest-goto-heading "Section B")
    (adoc-backward-same-level 1)
    (should (looking-at-p "== Section A$"))
    (adoctest-goto-heading "Sub A2")
    (adoc-backward-same-level 1)
    (should (looking-at-p "=== Sub A1$"))
    ;; a shallower heading stops the search; point is restored
    (adoctest-goto-heading "Sub A1")
    (let ((pos (point)))
      (should-error (adoc-backward-same-level 1) :type 'user-error)
      (should (= (point) pos)))))

(ert-deftest adoctest-test-up-heading ()
  (with-temp-buffer
    (adoc-mode)
    (adoctest-insert-nav-doc)
    (adoctest-goto-heading "Sub A1")
    (adoc-up-heading 1)
    (should (looking-at-p "== Section A$"))
    ;; two levels up from a level-2 heading reaches the document title
    (adoctest-goto-heading "Sub A2")
    (adoc-up-heading 2)
    (should (looking-at-p "= Doc Title$"))
    ;; nothing above the document title: point is restored
    (adoctest-goto-heading "Doc Title")
    (let ((pos (point)))
      (should-error (adoc-up-heading 1) :type 'user-error)
      (should (= (point) pos)))))

(ert-deftest adoctest-test-nav-two-line-titles ()
  (with-temp-buffer
    (adoc-mode)
    (insert "Doc Title\n=========\n\n"
            "intro\n\n"
            "Section A\n---------\n\n"
            "text\n\n"
            "Section B\n---------\n\n"
            "end\n")
    (goto-char (point-min))
    (adoc-next-visible-heading 1)
    (should (looking-at-p "Section A$"))
    (adoc-forward-same-level 1)
    (should (looking-at-p "Section B$"))
    (adoc-up-heading 1)
    (should (looking-at-p "Doc Title$"))))

;;;; Outline cycling

(defun adoctest-cycle-buffer ()
  "Set up a temp buffer with a small document for cycling tests."
  (adoc-mode)
  (insert "= Top\n\n== A\n\nbody a\n\n== B\n\nbody b\n")
  (font-lock-ensure)
  (goto-char (point-min)))

(ert-deftest adoctest-test-cycle-subtree ()
  (with-temp-buffer
    (adoctest-cycle-buffer)
    (let ((body (save-excursion (search-forward "body a") (point))))
      (search-forward "== A") (beginning-of-line)
      (should-not (outline-invisible-p body))
      ;; on a heading, cycling folds the subtree, then reveals it again
      (adoc-cycle)
      (should (outline-invisible-p body))
      (adoc-cycle)
      (should-not (outline-invisible-p body)))))

(ert-deftest adoctest-test-cycle-off-heading-does-not-fold ()
  (with-temp-buffer
    (adoctest-cycle-buffer)
    (let ((body (save-excursion (search-forward "body a") (point))))
      ;; point on an ordinary line: cycling must not fold anything
      (goto-char body)
      (adoc-cycle)
      (should-not (outline-invisible-p body)))))

(ert-deftest adoctest-test-cycle-buffer ()
  (with-temp-buffer
    (adoctest-cycle-buffer)
    (let ((body (save-excursion (search-forward "body a") (point))))
      (should-not (outline-invisible-p body))
      ;; a prefix argument cycles the whole buffer: overview hides bodies
      (adoc-cycle '(4))
      (should (outline-invisible-p body))
      ;; ... and the dedicated command eventually shows everything again
      (adoc-cycle-buffer)
      (adoc-cycle-buffer)
      (should-not (outline-invisible-p body)))))

;;;; Text styling

(ert-deftest adoctest-test-insert-markup-word ()
  ;; with no region, the word at point is wrapped
  (adoctest-trans "foo!" "*foo*" '(adoc-insert-bold))
  (adoctest-trans "foo!" "_foo_" '(adoc-insert-italic))
  (adoctest-trans "foo!" "`foo`" '(adoc-insert-monospace))
  (adoctest-trans "foo!" "#foo#" '(adoc-insert-highlight))
  (adoctest-trans "foo!" "^foo^" '(adoc-insert-superscript))
  (adoctest-trans "foo!" "~foo~" '(adoc-insert-subscript)))

(ert-deftest adoctest-test-insert-markup-empty ()
  ;; with neither region nor word, insert an empty pair
  (adoctest-trans "!" "**" '(adoc-insert-bold)))

(ert-deftest adoctest-test-insert-bold-region ()
  (with-temp-buffer
    (adoc-mode)
    (insert "foo bar baz")
    (goto-char 5) (set-mark 8) (activate-mark) ; region "bar"
    (adoc-insert-bold)
    (should (string-equal (buffer-string) "foo *bar* baz"))))

(ert-deftest adoctest-test-insert-bold-toggle-off ()
  ;; delimiters just outside the region are removed
  (with-temp-buffer
    (adoc-mode)
    (insert "foo *bar* baz")
    (goto-char 6) (set-mark 9) (activate-mark) ; region "bar"
    (adoc-insert-bold)
    (should (string-equal (buffer-string) "foo bar baz")))
  ;; delimiters inside the region are removed too
  (with-temp-buffer
    (adoc-mode)
    (insert "foo *bar* baz")
    (goto-char 5) (set-mark 10) (activate-mark) ; region "*bar*"
    (adoc-insert-bold)
    (should (string-equal (buffer-string) "foo bar baz"))))

(ert-deftest adoctest-test-insert-link ()
  (with-temp-buffer
    (adoc-mode)
    (adoc-insert-link "https://example.com" "Example")
    (should (string-equal (buffer-string) "https://example.com[Example]")))
  ;; an empty label yields a bare URL
  (with-temp-buffer
    (adoc-mode)
    (adoc-insert-link "https://example.com" "")
    (should (string-equal (buffer-string) "https://example.com")))
  ;; an active region is replaced by the link
  (with-temp-buffer
    (adoc-mode)
    (insert "see here now")
    (goto-char 5) (set-mark 9) (activate-mark) ; region "here"
    (adoc-insert-link "https://x.com" "here")
    (should (string-equal (buffer-string) "see https://x.com[here] now"))))

;;;; Escaped formatting

(ert-deftest adoctest-test-escaped-formatting ()
  ;; constrained: escaped markup is literal, the backslash is de-emphasised
  (adoctest-faces "escaped-bold"
                  "\\" 'adoc-meta-hide-face "*foo*" 'no-face)
  ;; unconstrained escape (previously leaked an inner constrained match)
  (adoctest-faces "escaped-unconstrained-bold"
                  "\\" 'adoc-meta-hide-face "**foo**" 'no-face)
  (adoctest-faces "escaped-monospace"
                  "\\" 'adoc-meta-hide-face "`foo`" 'no-face)
  ;; a backslash before a non-formatting char is left untouched
  (adoctest-faces "non-escape-backslash" "a\\b" 'no-face)
  ;; normal markup after an escaped span still fontifies
  (adoctest-faces "escape-then-bold"
                  "\\" 'adoc-meta-hide-face "*x* " 'no-face
                  "*" 'adoc-meta-hide-face "y" 'adoc-bold-face "*" 'adoc-meta-hide-face)
  ;; a lone dollar is not markup, so a literal \$ is left untouched
  (adoctest-faces "escaped-dollar" "x \\$5 y" 'no-face)
  ;; a triple-delimiter passthrough is escaped in full
  (adoctest-faces "escaped-passthrough"
                  "\\" 'adoc-meta-hide-face "+++p+++" 'no-face))

;;;; Filling

(ert-deftest adoctest-test-fill-preserves-hard-break ()
  (with-temp-buffer
    (adoc-mode)
    (setq fill-column 40)
    (insert "alpha beta gamma +\ndelta epsilon\neta theta iota\n")
    (goto-char (point-min))
    (fill-paragraph)
    (should (string-equal (buffer-string)
                          "alpha beta gamma +\ndelta epsilon eta theta iota\n"))))

(ert-deftest adoctest-test-fill-joins-plain-lines ()
  (with-temp-buffer
    (adoc-mode)
    (setq fill-column 40)
    (insert "word1 word2 word3\nword4 word5\n")
    (goto-char (point-min))
    (fill-paragraph)
    (should (string-equal (buffer-string) "word1 word2 word3 word4 word5\n"))))

(ert-deftest adoctest-test-fill-list-item-indents-continuation ()
  (with-temp-buffer
    (adoc-mode)
    (setq fill-column 40)
    (insert "* long list item that should wrap nicely when filled to a narrow width indeed yes\n")
    (goto-char (point-min))
    (fill-paragraph)
    (should (string-equal
             (buffer-string)
             "* long list item that should wrap nicely\n  when filled to a narrow width indeed\n  yes\n"))))

(ert-deftest adoctest-test-fill-clears-stale-hard-break ()
  ;; once the user removes a ` +', a refill must merge the lines again
  (with-temp-buffer
    (adoc-mode)
    (setq fill-column 30)
    (insert "alpha beta gamma delta epsilon zeta eta theta iota +\nkappa lambda mu nu\n")
    (goto-char (point-min))
    (fill-paragraph)
    (goto-char (point-min))
    (re-search-forward " \\+$")
    (delete-char -2)                    ; drop the trailing " +"
    (goto-char (point-min))
    (fill-paragraph)
    (should-not (string-match-p "iota +\n" (buffer-string)))
    (should (string-match-p "iota kappa" (buffer-string)))))

;;;; List editing

(ert-deftest adoctest-test-list-promote-demote-unordered ()
  ;; promote nests deeper, demote nests shallower
  (adoctest-trans "* foo!" "** foo" '(adoc-promote 1))
  (adoctest-trans "** foo!" "* foo" '(adoc-demote 1))
  ;; level 1 (*) demotes to level 0 (-), and back
  (adoctest-trans "* foo!" "- foo" '(adoc-demote 1))
  (adoctest-trans "- foo!" "* foo" '(adoc-promote 1))
  ;; clamped at the extremes
  (adoctest-trans "- foo!" "- foo" '(adoc-demote 1))
  (adoctest-trans "***** foo!" "***** foo" '(adoc-promote 1))
  ;; leading indentation is preserved
  (adoctest-trans "  ** foo!" "  *** foo" '(adoc-promote 1))
  ;; a numeric argument changes several levels at once
  (adoctest-trans "* foo!" "*** foo" '(adoc-promote 2)))

(ert-deftest adoctest-test-list-promote-demote-numbered ()
  (adoctest-trans ". foo!" ".. foo" '(adoc-promote 1))
  (adoctest-trans ".. foo!" ". foo" '(adoc-demote 1))
  (adoctest-trans ". foo!" ". foo" '(adoc-demote 1)))

(ert-deftest adoctest-test-list-promote-explicit-numbered-errors ()
  (with-temp-buffer
    (adoc-mode)
    (insert "1. foo")
    (goto-char (point-min))
    (should-error (adoc-promote 1) :type 'user-error)
    ;; buffer is left untouched
    (should (string-equal (buffer-string) "1. foo"))))

(ert-deftest adoctest-test-insert-list-item ()
  (adoctest-trans "* foo!" "* foo\n* " '(adoc-insert-list-item))
  (adoctest-trans "- foo!" "- foo\n- " '(adoc-insert-list-item))
  (adoctest-trans ".. bar!" ".. bar\n.. " '(adoc-insert-list-item))
  ;; explicitly-numbered items get an incremented marker
  (adoctest-trans "1. foo!" "1. foo\n2. " '(adoc-insert-list-item))
  (adoctest-trans "a. foo!" "a. foo\nb. " '(adoc-insert-list-item))
  (adoctest-trans "9. foo!" "9. foo\n10. " '(adoc-insert-list-item))
  ;; roman numerals and the last letter of the alphabet are copied verbatim
  (adoctest-trans "i) foo!" "i) foo\ni) " '(adoc-insert-list-item))
  (adoctest-trans "z. foo!" "z. foo\nz. " '(adoc-insert-list-item))
  ;; indentation is preserved
  (adoctest-trans "  ** foo!" "  ** foo\n  ** " '(adoc-insert-list-item)))

(ert-deftest adoctest-test-insert-list-item-not-in-list ()
  (with-temp-buffer
    (adoc-mode)
    (insert "just some prose")
    (goto-char (point-min))
    (should-error (adoc-insert-list-item) :type 'user-error)
    (should (string-equal (buffer-string) "just some prose"))))

(ert-deftest adoctest-test-move-list-item-down ()
  (adoctest-trans "* a!\n* b\n" "* b\n* a\n" '(adoc-move-list-item-down))
  ;; a missing final newline must not merge the two items into one line
  (adoctest-trans "* a!\n* b" "* b\n* a\n" '(adoc-move-list-item-down))
  ;; the item carries its nested sub-items with it
  (adoctest-trans "* one!\n** sub\n* two\n" "* two\n* one\n** sub\n"
                  '(adoc-move-list-item-down)))

(ert-deftest adoctest-test-move-list-item-up ()
  (adoctest-trans "* a\n* b!\n" "* b\n* a\n" '(adoc-move-list-item-up))
  ;; a missing final newline must not merge the two items into one line
  (adoctest-trans ". one\n. two!" ". two\n. one\n" '(adoc-move-list-item-up))
  (adoctest-trans "* one\n** sub\n* two!\n" "* two\n* one\n** sub\n"
                  '(adoc-move-list-item-up)))

(ert-deftest adoctest-test-move-list-item-distinct-lists ()
  ;; a same-level item of a different list type is not a sibling
  (with-temp-buffer
    (adoc-mode)
    (insert "1. alpha\n- bullet\n2. beta\n")
    (goto-char (point-min))
    (should-error (adoc-move-list-item-down) :type 'user-error)
    (should (string-equal (buffer-string) "1. alpha\n- bullet\n2. beta\n"))))

(ert-deftest adoctest-test-move-list-item-no-sibling ()
  (with-temp-buffer
    (adoc-mode)
    (insert "* only\n")
    (goto-char (point-min))
    (should-error (adoc-move-list-item-down) :type 'user-error)
    (should (string-equal (buffer-string) "* only\n")))
  ;; a shallower neighbour is not a sibling
  (with-temp-buffer
    (adoc-mode)
    (insert "** a\n* b\n")
    (goto-char (point-min))
    (should-error (adoc-move-list-item-down) :type 'user-error)
    (should (string-equal (buffer-string) "** a\n* b\n"))))

(ert-deftest adoctest-test-renumber-list ()
  (adoctest-trans "1. a!\n1. b\n1. c\n" "1. a\n2. b\n3. c\n" '(adoc-renumber-list))
  ;; works from any item in the run, and preserves the first item's start value
  (adoctest-trans "1. a\n5. b!\n9. c\n" "1. a\n2. b\n3. c\n" '(adoc-renumber-list))
  (adoctest-trans "3. a!\n1. b\n" "3. a\n4. b\n" '(adoc-renumber-list))
  ;; alphabetic lists
  (adoctest-trans "a. x!\na. y\n" "a. x\nb. y\n" '(adoc-renumber-list))
  ;; a missing final newline must terminate (not loop forever)
  (adoctest-trans "1. a!\n9. b" "1. a\n2. b" '(adoc-renumber-list)))

(ert-deftest adoctest-test-renumber-list-alpha-overflow ()
  (with-temp-buffer
    (adoc-mode)
    (dotimes (_ 27) (insert "a. item\n"))
    (goto-char (point-min))
    (should-error (adoc-renumber-list) :type 'user-error)))

(ert-deftest adoctest-test-renumber-list-errors ()
  ;; roman lists are left untouched
  (with-temp-buffer
    (adoc-mode)
    (insert "i) a\nv) b\n")
    (goto-char (point-min))
    (should-error (adoc-renumber-list) :type 'user-error)
    (should (string-equal (buffer-string) "i) a\nv) b\n")))
  ;; not on an explicitly-numbered list
  (with-temp-buffer
    (adoc-mode)
    (insert "* a\n* b\n")
    (goto-char (point-min))
    (should-error (adoc-renumber-list) :type 'user-error)
    (should (string-equal (buffer-string) "* a\n* b\n"))))

(ert-deftest adoctest-test-xref-at-point-1 ()
  (unwind-protect
      (progn
        (set-buffer (get-buffer-create "adoc-test"))
        (insert "lorem xref:bogous1[] ipsum xref:foo[bla\nbli] dolor xref:bogous2[]")
        (re-search-backward "bli")      ; move point within ref
        (should (equal (adoc-xref-id-at-point) "foo")))
    (kill-buffer "adoc-test")))

(ert-deftest adoctest-test-xref-at-point-2 ()
  (unwind-protect
      (progn
        (set-buffer (get-buffer-create "adoc-test"))
        (insert "lorem <<bogous1,caption>> ipsum <<foo,bla\nbli>> dolor <<bogous2>>")
        (re-search-backward "bli") ; move point within ref
        (should (equal (adoc-xref-id-at-point) "foo")))
    (kill-buffer "adoc-test")))

(ert-deftest adoctest-test-goto-ref-label ()
  (unwind-protect
      (progn
        (set-buffer (get-buffer-create "adoc-test"))
        (insert "[[foo]]\n"                ;1
                "lorem ipsum\n"            ;2
                "[[bar]]\n"                ;3
                "dolor [[geil]]sit amen\n" ;4
                "anchor:cool[]\n")         ;5
        (adoc-goto-ref-label "cool")
        (should (equal (line-number-at-pos) 5))
        (adoc-goto-ref-label "geil")
        (should (equal (line-number-at-pos) 4))
        (adoc-goto-ref-label "bar")
        (should (equal (line-number-at-pos) 3)))
    (kill-buffer "adoc-test")))

(defun adoctest-template (template expected)
  "Todo document adoctest-template TEMPLATE EXPECTED."
  (let ((buf-name (concat "adoctest-" (symbol-name template))))
    (unwind-protect
        (progn
          ;; setup
          (set-buffer (get-buffer-create buf-name))
          (delete-region (point-min) (point-max))
          (funcall template)
          (should (equal (buffer-substring-no-properties (point-min) (point-max)) expected)))
      ;; tear-down
      (kill-buffer buf-name))))

(ert-deftest adoctest-test-make-two-line-title-underline ()
  (should (equal (adoc-make-two-line-title-underline 0 6)
                 "======"))
  (should (equal (adoc-make-two-line-title-underline 2)
                 "~~~~")))

(ert-deftest adoctest-test-indent-by-example ()
  (let ((tab-width 2)
        (indent-tabs-mode nil))
    (adoctest-trans "" " x"   '(adoc-insert-indented "x" 1))
    (adoctest-trans "" "   x" '(adoc-insert-indented "x" 2)))

  (let ((tab-width 3)
        (indent-tabs-mode t))
    (adoctest-trans "" "  x"   '(adoc-insert-indented "x" 1))
    (adoctest-trans "" "\t  x" '(adoc-insert-indented "x" 2))))

(ert-deftest adoctest-test-imenu-create-index ()
  (unwind-protect
      (progn
        (set-buffer (get-buffer-create "adoc-test"))
        (insert "= document title\n"
                "== chapter 1\n"
                "[IMPORTANT]\n"
                ".Important announcement\n"
                "====\n"
                "This should not be a title\n"
                "====\n"
                "=== sub chapter 1.1\n"
                "[source,rust]\n"
                "----\n"
                "// here is a comment\n"
                "----\n"
                "chapter 2\n"
                "----------\n"
                "[latexmath#einstein]\n"
                "++++\n"
                "\begin{equation}\n"
                "e = mc^{2}\n"
                "\end{equation}\n"
                "++++\n"
                "sub chapter 2.1\n"
                "~~~~~~~~~~~~~~\n")
        (should
         (equal
          (adoc-imenu-create-index)
          (list
           (cons "document title" 1)
           (cons "chapter 1" 18)
           (cons "sub chapter 1.1" 104)
           (cons "chapter 2" 169)
           (cons "sub chapter 2.1" 262)))))
    (kill-buffer "adoc-test")))

(ert-deftest adoctest-test-imenu-create-nested-index ()
  (unwind-protect
      (progn
        (set-buffer (get-buffer-create "adoc-test"))
        (insert "= document title\n"
                "== chapter 1\n"
                "=== sub chapter 1.1\n"
                "== chapter 2\n"
                "=== sub chapter 2.1\n"
                "=== sub chapter 2.2\n")
        (should
         (equal
          (adoc-imenu-create-nested-index)
          '(("document title"
             (nil . 1)
             ("chapter 1"
              (nil . 18)
              ("sub chapter 1.1" . 31))
             ("chapter 2"
              (nil . 51)
              ("sub chapter 2.1" . 64)
              ("sub chapter 2.2" . 84)))))))
    (kill-buffer "adoc-test")))

(ert-deftest adoctest-update-title-faces ()
  (unwind-protect
      (let ((adoc-title-scaling t)
            (adoc-title-scaling-values '(2.5 2.0 1.6 1.3 1.1 1.0)))
        (adoc-update-title-faces)
        (should (= 2.5 (face-attribute 'adoc-title-0-face :height)))
        (should (= 1.1 (face-attribute 'adoc-title-4-face :height)))
        (setq adoc-title-scaling nil)
        (adoc-update-title-faces)
        (should (= 1.0 (face-attribute 'adoc-title-0-face :height)))
        (should (= 1.0 (face-attribute 'adoc-title-5-face :height))))
    ;; restore defaults so subsequent tests see the documented heights
    (let ((adoc-title-scaling t)
          (adoc-title-scaling-values '(2.0 1.8 1.6 1.4 1.2 1.0)))
      (adoc-update-title-faces))))

(ert-deftest adoctest-adoc-kw-replacement ()
  (unwind-protect
      (progn
	(set-buffer (get-buffer-create "adoc-test"))
	(erase-buffer)
	(adoc-mode)
	(let ((adoc-insert-replacement t))
	  (adoc-calc)
	  (insert "(C)")
	  (font-lock-flush)
	  (font-lock-ensure)
	  (should (string-equal (overlay-get (car (overlays-in (point) (point-max))) 'after-string) "©"))
	  )
	)
    (adoc-calc)
    (kill-buffer "adoc-test")))

;; purpose
;; - ensure that the latest version, i.e. the one currently in buffer(s), of
;;   adoc-mode and adoc-mode-test is used for the test
;; - test that adoc-mode and adoc-mode-test are compileble & loadable
;; - ensure no *.elc are lying around because they are 'dangerous' when the
;;   corresponding .el is edited regulary; dangerous because it's not unlikely
;;   that the .el is newer than the .elc, but load-library takes the outdated
;;   .elc.
;;
;; todo: also test for warnings
(defun adoctest-save-compile-load ()
  "Todo document adoctest-save-compile-load."
  (unwind-protect
      (progn
        (let ((buf-adoc-mode (find-buffer-visiting "adoc-mode.el"))
              (buf-adoc-mode-test (find-buffer-visiting "adoc-mode-test.el")))

          ;; adoc-mode
          (cond
           ((null buf-adoc-mode))       ;nop
           ((bufferp buf-adoc-mode) (save-buffer buf-adoc-mode))
           (t (error "Multiple buffer are visiting adoc-mode.el.  Save them first")))
          (or (byte-compile-file (locate-library "adoc-mode.el" t)) (error "Compile error"))
          (or (load "adoc-mode.el" nil nil t) (error "Load error"))

          ;; adoc-mode-test
          (cond
           ((null buf-adoc-mode-test))  ;nop
           ((bufferp buf-adoc-mode-test) (save-buffer buf-adoc-mode-test))
           (t (error "Multiple buffer are visiting adoc-mode-test.el.  Save them first")))
          (or (byte-compile-file (locate-library "adoc-mode-test.el" t)) (error "Compile error"))
          (or (load "adoc-mode-test.el" nil nil t) (error "Load error"))))

    (when (file-exists-p "adoc-mode.elc")
      (delete-file "adoc-mode.elc"))
    (when (file-exists-p "adoc-mode-test.elc")
      (delete-file "adoc-mode-test.elc"))))

(defun adoc-test-run()
  (interactive)

  ;; ensure that a failed test can be re-run
  (when (get-buffer "*ert*")
    (kill-buffer "*ert*"))

  ;; ensure no no-longer test defuns exist, which would otherwise be executed
  (mapatoms
   (lambda (x) (if (string-match "^adoctest-test-" (symbol-name x))
                   (unintern x nil))))

  (adoctest-save-compile-load)

  ;; todo: execute tests in an smart order: the basic/simple tests first, so
  ;; when a complicated test fails one knows that the simple things do work
  (ert-run-tests-interactively "^adoctest-test-"))

(provide 'adoc-mode-test)

;;; adoc-mode-test.el ends here
