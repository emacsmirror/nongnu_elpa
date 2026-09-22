;;; hermes-chat-format-tests.el --- Chat topic tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-chat-format)
(require 'delsel)
(require 'hermes-test-helpers)

(ert-deftest hermes-chat-status-helpers-classify-parity-states ()
  (dolist (case '(("in_progress" "Running" "·" shadow t nil)
                  ("busy" "Running" "·" shadow t nil)
                  ("approval-requested" "Approval requested" "·"
                   shadow t nil)
                  ("queued" "Queued" "·" shadow t nil)
                  ("succeeded" "Ready" "✓" success nil t)
                  ("interrupted" "Interrupted" "!" error nil t)
                  ("cancelled" "Cancelled" "!" error nil t)
                  ("closed" "Disconnected" "!" warning nil t)))
    (pcase-let ((`(,status ,label ,icon ,face ,active ,finished) case))
      (should (equal (hermes-chat--header-status-label status) label))
      (should (equal (hermes-chat--status-icon status) icon))
      (should (eq (hermes-chat--status-face status) face))
      (should (eq (hermes-chat--header-status-face status)
                  (cond ((equal label "Running") 'font-lock-keyword-face)
                        ((equal label "Approval requested") 'warning)
                        (t face))))
      (should (eq (not (null (hermes-chat--active-status-p status))) active))
      (should (eq (not (null (hermes-chat--finished-status-p status)))
                  finished)))))

(ert-deftest hermes-chat-markdown-keeps-markup-visible ()
  "Markdown markers keep their faces but are never hidden, for easy copying."
  (let ((s (hermes-chat--fontify-markdown-string "say *hello* and `code`")))
    (should (string-match-p "\\*hello\\*" s))
    (should (string-match-p "`code`" s))
    (dotimes (i (length s))
      (should-not (get-text-property i 'invisible s)))))

(ert-deftest hermes-chat-markdown-marks-original-tables ()
  "Recognizing tables does not align or otherwise change their source."
  (let* ((raw "| A | B |\n|---|---|\n| one | two |\n")
         (text (hermes-chat--fontify-markdown-string raw)))
    (should (equal (substring-no-properties text) raw))
    (should (equal (get-text-property 0 'hermes-chat-table text) raw))))

(ert-deftest hermes-chat-table-wrap-preserves-literals ()
  "Wrapping preserves every character, native face and combining sequence."
  (dolist (text (list "path\\ followed by text" "a\\|b **bold** `code`"
                      "界é界é" (make-string 160 ?x)))
    (dolist (width '(2 3 7 22))
      (let ((lines (hermes-chat--table-cell-lines
                    (propertize text 'face 'bold) width)))
        (should (equal (apply #'concat lines) text))
        (dolist (line lines)
          (should (<= (string-width line) width))
          (when (> (length line) 0)
            (should (eq (get-text-property 0 'face line) 'bold))
            (should-not (= (aref line 0) #x301))))))))

(ert-deftest hermes-chat-table-grid-bounded-with-native-cells ()
  "Long tokens, escaped pipes and Unicode remain visible in bounded grids."
  (let ((source (concat "| Kind | Description |\n|---|---|\n"
                        "| 界é **bold** | path\\ followed a\\|b `x|y` "
                        (make-string 160 ?x) " |\n")))
    (dolist (width '(12 28 78))
      (let ((grid (hermes-chat--format-table source width)))
        (dolist (line (split-string grid "\n" t))
          (should (<= (string-width line) width)))
        (should-not (text-property-not-all 0 (length grid) 'display nil grid))
        (should-not (text-property-not-all 0 (length grid) 'invisible nil grid))
        (should (eq 'fixed-pitch (get-text-property 0 'face grid)))))
    ;; The native parser keeps escaped pipes and fontified code spans together.
    (let* ((line (hermes-chat--fontify-markdown-string
                  "| a\\|b | `x|y` |\n"))
           (cells (markdown--table-line-to-columns (string-trim-right line))))
      (should (equal cells '("a\\|b" "`x|y`"))))))

(ert-deftest hermes-chat-table-degenerate-and-optional-trailing-pipes ()
  "Degenerate tables stay literal; missing closing pipes never lose cells."
  (dolist (raw '("|---|---|\n" "|\n"))
    (should (equal (substring-no-properties (hermes-chat--format-table raw 28)) raw)))
  (let* ((raw "| A | B\n|---|---|\n| x | y\n")
         (grid (hermes-chat--format-table raw 28)))
    (dolist (cell '("A" "B" "x" "y"))
      (should (string-match-p cell grid)))))

(ert-deftest hermes-chat-table-narrow-column-panels ()
  "Narrow windows retain every header and data cell in column panels."
  (let* ((source "| A | B | C | D |\n|---|---|---|---|\n| a | b | c | d |\n")
         (grid (hermes-chat--format-table source 12)))
    (dolist (cell '("A" "B" "C" "D" "a" "b" "c" "d"))
      (should (string-match-p cell grid)))
    (dolist (line (split-string grid "\n" t))
      (should (<= (string-width line) 12)))))

(ert-deftest hermes-chat-table-ragged-panels-preserve-all-cells ()
  "Missing cells render empty, including entire rows and header panel slices."
  (let ((rows '(("A" "B" "C") ("x") nil ("y" "z" "q" "r")))
        (padded '(("A" "B" "C" "") ("x" "" "" "")
                  ("" "" "" "") ("y" "z" "q" "r"))))
    (dolist (width '(6 12 80))
      (should (equal (hermes-chat--table-grid rows width)
                     (hermes-chat--table-grid padded width)))))
  (dolist (body '("| x |\n" "|\n| x |\n| y | z | q | r |\n"))
    (let* ((raw (concat "| A | B | C |\n|---|---|---|\n" body))
           (grid (hermes-chat--format-table raw 12)))
      (dolist (cell (if (string-match-p "y" body)
                        '("A" "B" "C" "x" "y" "z" "q" "r")
                      '("A" "B" "C" "x")))
        (should (string-match-p (regexp-quote cell) grid)))
      (dolist (line (split-string grid "\n" t))
        (should (<= (string-width line) 12))))))

(ert-deftest hermes-chat-markdown-unclosed-fenced-tables-stay-inline ()
  "Settled replies retain table-like code in either unclosed fence."
  (dolist (fence '("```" "~~~"))
    (let* ((raw (concat fence "\n| A | B |\n|---|---|\n| one | two |\n"))
           (formatted (hermes-chat--fontify-markdown-string raw)))
      (should (equal (substring-no-properties formatted) raw))
      (should-not (text-property-not-all
                   0 (length formatted) 'hermes-chat-table nil formatted))
      (hermes-test-with-chat-buffer
       (hermes-chat--insert-entry
        (hermes-chat--make-entry 'assistant raw 'done))
       (should (string-match-p (regexp-quote raw) (buffer-string)))
       (should-not (next-button (point-min)))))))

(ert-deftest hermes-chat-markdown-leaves-fenced-tables-alone ()
  "A table inside either closed fence stays inline, unlike a table after it."
  (dolist (fence '("```" "~~~"))
    (let* ((table "| short | x |\n|---|---|\n| much longer cell | y |\n")
           (raw (concat fence "\n" table fence "\n"))
           (s (hermes-chat--fontify-markdown-string raw)))
      (should (equal (substring-no-properties s) raw))
      (should-not (text-property-not-all 0 (length s) 'hermes-chat-table nil s))
      (with-temp-buffer
        (hermes-chat--insert-markdown (concat raw "\n" table))
        (should (string-prefix-p raw (buffer-string)))
        (let ((button (next-button (point-min))))
          (should button)
          (should (> (button-start button) (length raw)))
          (should (equal (button-get button 'hermes-chat-table) table))
          (should-not (next-button (button-end button))))))))

(ert-deftest hermes-chat-sanitize-fragment-state-is-explicit ()
  "ANSI fragments are pure values that can be carried by independent streams."
  (let* ((first (hermes-chat--sanitize-content-with-fragment
                 "left\e[38;2;255" nil))
         (other (hermes-chat--sanitize-content-with-fragment "right" nil))
         (continued (hermes-chat--sanitize-content-with-fragment
                     ";0;0m!" (cdr first))))
    (should (equal (car first) "left"))
    (should (equal (car other) "right"))
    (should-not (cdr other))
    (should (equal (car continued) "!"))
    (should-not (cdr continued))))

(ert-deftest hermes-chat-thinking-activity-is-neutral ()
  "Provider notices are activity, not evidence of reasoning."
  (dolist (content '("(◔_◔) pondering..." "reasoning" "Rate limited; waiting"))
    (should (equal (hermes-chat--thinking-activity content) "Working")))
  (should-not (hermes-chat--thinking-activity ""))
  (should-not (hermes-chat--thinking-activity nil)))

(ert-deftest hermes-chat-format-tool-event-keeps-detail-and-emoji ()
  "Tool lines keep the command/skill detail and carry the tool emoji."
  (should (equal (hermes-chat--format-tool-event
                  '(:type tool :name "terminal" :status "running"
                          :context "make test"))
                 "💻 terminal: make test"))
  (should (equal (hermes-chat--format-tool-event
                  '(:type tool :name "terminal" :status "completed"
                          :args ((command . "make test")) :duration 0.2))
                 "💻 terminal: make test  0.2s"))
  (should (equal (hermes-chat--format-tool-event
                  '(:type tool :name "skill_view" :status "completed"
                          :args ((name . "elisp-review")) :duration 0.1))
                 "📚 skill_view: elisp-review  0.1s"))
  (should (string-prefix-p "⚡ mystery"
                           (hermes-chat--format-tool-event
                            '(:type tool :name "mystery" :status "running")))))

(ert-deftest hermes-chat-format-context ()
  "Context usage renders only abbreviated used and limit tokens."
  (should (equal (hermes-chat--format-context '(:used 45000 :max 200000 :percent 22))
                  "45k/200k"))
  (should-not (hermes-chat--format-context '(:used 0 :max 0 :percent 0)))
  (should-not (hermes-chat--format-context nil)))

(ert-deftest hermes-chat-extract-embedded-images-lifts-data-url ()
  "Embedded data:image URLs leave cleaned prose and an image list."
  (let* ((png (concat "data:image/png;base64," (make-string 80 ?A)))
         (result (hermes-chat--extract-embedded-images
                  (format "see this %s please" png))))
    (should (equal (car result) "see this  please"))
    (should (equal (cdr result) (list png)))))

(ert-deftest hermes-chat-extract-embedded-images-rejects-oversize-boundedly ()
  "Oversized image data is rejected with bounded validation work."
  (let* ((hermes-chat--max-embedded-image-base64 80)
         (text (concat "data:image/png;base64," (make-string 100000 ?A)))
         (real-bounded-run
          (symbol-function 'hermes-chat--bounded-valid-run-length))
         limits
         result)
    (cl-letf (((symbol-function 'hermes-chat--bounded-valid-run-length)
               (lambda (&rest args)
                 (push (nth 3 args) limits)
                 (apply real-bounded-run args))))
      (setq result (hermes-chat--extract-embedded-images text)))
    (should (equal (car result) text))
    (should-not (cdr result))
    (should (equal (nreverse limits)
                   (list hermes-chat--max-embedded-image-mime-length
                         hermes-chat--max-embedded-image-base64)))))

(ert-deftest hermes-chat-extract-embedded-images-ignores-short-payload ()
  "Short base64 payloads are left in prose."
  (let* ((short "data:image/png;base64,AAAA")
         (text (format "keep %s text" short))
         (result (hermes-chat--extract-embedded-images text)))
    (should (equal (car result) text))
    (should-not (cdr result))))

(ert-deftest hermes-chat-format-sanitize-osc-and-lone-escape-streams ()
  "Independent escape streams retain Unicode and discard only controls."
  (let* ((osc (hermes-chat--sanitize-content-with-fragment "α\e]0;title" nil))
         (esc (hermes-chat--sanitize-content-with-fragment "β\e" nil)))
    (should (equal osc '("α" . "\e]0;title")))
    (should (equal esc '("β" . "\e")))
    (should (equal (hermes-chat--sanitize-content-with-fragment
                    "\a界\0\177\u0085\t\n" (cdr osc))
                   '("界\t\n")))
    (should (equal (hermes-chat--sanitize-content-with-fragment
                    "[31mγ" (cdr esc)) '("γ")))
    (should (equal (hermes-chat--sanitize-content-with-fragment
                    "\e\\δ" (cdr osc)) '("δ")))))

(ert-deftest hermes-chat-format-diff-hunk-counts-and-rollback ()
  "Consume declared hunks; reject malformed, overrunning and no-change bodies."
  (with-temp-buffer
    (insert "prefix\n@@ -1,2 +1,2 @@\n keep\n-old\n+new\ntrailer\n")
    (goto-char (point-min))
    (forward-line 1)
    (should (hermes-chat--consume-unified-diff-hunk))
    (should (looking-at "trailer")))
  (dolist (body '("@@ nonsense\n-old\n+new\n"
                  "@@ -1 +1 @@\n-old\n-old again\n+new\n"
                  "@@ -1 +1 @@\n keep\n"
                  "@@ -1,2 +1,2 @@\n-old\n+new\nnot a body line\n"))
    (with-temp-buffer
      (insert "prefix\n" body)
      (goto-char (point-min))
      (forward-line 1)
      (let ((start (point)))
        (should-not (hermes-chat--consume-unified-diff-hunk))
        (should (= (point) start)))))
  (let* ((diff "@@ -1 +1 @@\n-old\n+new\n")
         (text (concat "Intro\n" diff "End\n"))
         (blocks (hermes-chat--diff-blocks text)))
    (should (equal blocks (list (list 6 (+ 6 (length diff)) diff))))))

(provide 'hermes-chat-format-tests)
;;; hermes-chat-format-tests.el ends here
