;;; Emit mutation sites for a lisp file, one per line:
;;;   OFFSET|LENGTH|REPLACEMENT|DESCRIPTION
;;; Offsets are character positions (0-based) into the file's text.
;;; Sites are found by parsing, so a replacement always covers a whole sexp
;;; and the mutant stays balanced.

(defun sweep-site (start end replacement description)
  (princ (format "%d|%d|%s|%s\n" (1- start) (- end start) replacement description)))

(defun sweep-guard-sites (keyword replacement)
  "Emit a site for the condition of each (KEYWORD COND ...) in the buffer."
  (goto-char (point-min))
  (while (re-search-forward (format "(%s\\_>" (regexp-quote keyword)) nil t)
    (unless (nth 8 (syntax-ppss))          ; not in a string or comment
      (let ((start (progn (skip-chars-forward " \t\n") (point))))
        (condition-case nil
            (let ((end (save-excursion (forward-sexp) (point))))
              (unless (equal (buffer-substring-no-properties start end) replacement)
                (sweep-site start end replacement
                            (format "%s condition -> %s" keyword replacement))))
          (error nil))))))

(defun sweep-number-sites ()
  "Emit a site for each integer literal, replaced by its successor."
  (goto-char (point-min))
  (while (re-search-forward "\\_<-?[0-9]+\\_>" nil t)
    (unless (nth 8 (syntax-ppss))
      (let* ((text (match-string-no-properties 0))
             (n (string-to-number text)))
        ;; leave alone the numbers that are almost always structural
        (unless (member text '("0" "1"))
          (sweep-site (match-beginning 0) (match-end 0)
                      (number-to-string (1+ n))
                      (format "number %s -> %s" text (1+ n))))))))

(defun sweep-call-sites (names)
  "Emit a site for each call to one of NAMES, replaced by nil."
  (dolist (name names)
    (goto-char (point-min))
    (while (re-search-forward (format "(%s\\_>" (regexp-quote name)) nil t)
      (unless (nth 8 (syntax-ppss))
        (condition-case nil
            (let* ((start (progn (goto-char (match-beginning 0)) (point)))
                   (end (save-excursion (forward-sexp) (point))))
              (goto-char end)
              (sweep-site start end "nil" (format "%s call -> nil" name)))
          (error nil))))))

(let ((file (car command-line-args-left)))
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (sweep-guard-sites "when" "t")
    (sweep-guard-sites "unless" "nil")
    (sweep-guard-sites "if" "t")
    (sweep-number-sites)
    (sweep-call-sites '("setq" "push" "delete-region" "insert"))))
