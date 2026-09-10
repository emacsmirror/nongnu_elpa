;;; vm-misc-test.el --- Tests for vm-misc.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM utility functions in vm-misc.el

;;; Code:

(require 'vm-test-init)
(require 'vm-misc)

;;; vm-parse tests

(ert-deftest vm-misc-test-parse-simple ()
  "Test vm-parse with simple colon-separated string."
  (should (equal (vm-parse "a:b:c" "\\([^:]+\\):?" 1)
                 '("a" "b" "c"))))

(ert-deftest vm-misc-test-parse-empty-string ()
  "Test vm-parse with empty string."
  (should (equal (vm-parse "" "\\([^:]+\\):?" 1)
                 nil)))

(ert-deftest vm-misc-test-parse-single-element ()
  "Test vm-parse with single element (no delimiter)."
  (should (equal (vm-parse "abc" "\\([^:]+\\):?" 1)
                 '("abc"))))

(ert-deftest vm-misc-test-parse-with-limit ()
  "Test vm-parse with match limit."
  (should (equal (vm-parse "a:b:c:d" "\\([^:]+\\):?" 1 2)
                 '("a" "b" "c:d"))))

;;; vm-replace-in-string tests

(ert-deftest vm-misc-test-replace-in-string-simple ()
  "Test vm-replace-in-string with simple replacement."
  (should (equal (vm-replace-in-string "hello world" "world" "emacs")
                 "hello emacs")))

(ert-deftest vm-misc-test-replace-in-string-regex ()
  "Test vm-replace-in-string with regex pattern."
  (should (equal (vm-replace-in-string "foo123bar" "[0-9]+" "X")
                 "fooXbar")))

(ert-deftest vm-misc-test-replace-in-string-multiple ()
  "Test vm-replace-in-string replacing multiple occurrences."
  (should (equal (vm-replace-in-string "a-b-c" "-" "_")
                 "a_b_c")))

(ert-deftest vm-misc-test-replace-in-string-no-match ()
  "Test vm-replace-in-string when pattern doesn't match."
  (should (equal (vm-replace-in-string "hello" "xyz" "abc")
                 "hello")))

(ert-deftest vm-misc-test-replace-in-string-empty ()
  "Test vm-replace-in-string with empty string."
  (should (equal (vm-replace-in-string "" "x" "y")
                 "")))

;;; vm-string-assoc tests

(ert-deftest vm-misc-test-string-assoc-found ()
  "Test vm-string-assoc finding element."
  (let ((alist '(("UTF-8" . utf-8) ("ISO-8859-1" . latin-1))))
    (should (equal (vm-string-assoc "UTF-8" alist) '("UTF-8" . utf-8)))))

(ert-deftest vm-misc-test-string-assoc-case-insensitive ()
  "Test vm-string-assoc is case-insensitive."
  (let ((alist '(("UTF-8" . utf-8) ("ISO-8859-1" . latin-1))))
    (should (equal (vm-string-assoc "utf-8" alist) '("UTF-8" . utf-8)))))

(ert-deftest vm-misc-test-string-assoc-not-found ()
  "Test vm-string-assoc when element not found."
  (let ((alist '(("UTF-8" . utf-8))))
    (should (null (vm-string-assoc "ASCII" alist)))))

;;; vm-string-member tests

(ert-deftest vm-misc-test-string-member-found ()
  "Test vm-string-member finding element."
  (should (vm-string-member "hello" '("hello" "world"))))

(ert-deftest vm-misc-test-string-member-case-insensitive ()
  "Test vm-string-member is case-insensitive."
  (should (vm-string-member "HELLO" '("hello" "world"))))

(ert-deftest vm-misc-test-string-member-not-found ()
  "Test vm-string-member when element not found."
  (should (null (vm-string-member "foo" '("hello" "world")))))

;;; vm-string-equal-ignore-case tests

(ert-deftest vm-misc-test-string-equal-ignore-case-same ()
  "Test vm-string-equal-ignore-case with identical strings."
  (should (vm-string-equal-ignore-case "hello" "hello")))

(ert-deftest vm-misc-test-string-equal-ignore-case-different-case ()
  "Test vm-string-equal-ignore-case with different case."
  (should (vm-string-equal-ignore-case "Hello" "HELLO"))
  (should (vm-string-equal-ignore-case "HELLO" "hello")))

(ert-deftest vm-misc-test-string-equal-ignore-case-different ()
  "Test vm-string-equal-ignore-case with different strings."
  (should-not (vm-string-equal-ignore-case "hello" "world")))

;;; vm-abs tests

(ert-deftest vm-misc-test-abs-positive ()
  "Test vm-abs with positive number."
  (should (= (vm-abs 5) 5)))

(ert-deftest vm-misc-test-abs-negative ()
  "Test vm-abs with negative number."
  (should (= (vm-abs -5) 5)))

(ert-deftest vm-misc-test-abs-zero ()
  "Test vm-abs with zero."
  (should (= (vm-abs 0) 0)))

;;; vm-last tests

(ert-deftest vm-misc-test-last ()
  "Test vm-last returns last cons cell."
  (let ((list '(a b c)))
    (should (equal (vm-last list) '(c)))))

(ert-deftest vm-misc-test-last-single ()
  "Test vm-last with single element list."
  (should (equal (vm-last '(a)) '(a))))

(ert-deftest vm-misc-test-last-nil ()
  "Test vm-last with nil."
  (should (null (vm-last nil))))

;;; vm-last-elem tests

(ert-deftest vm-misc-test-last-elem ()
  "Test vm-last-elem returns last element."
  (should (eq (vm-last-elem '(a b c)) 'c)))

(ert-deftest vm-misc-test-last-elem-single ()
  "Test vm-last-elem with single element."
  (should (eq (vm-last-elem '(a)) 'a)))

(ert-deftest vm-misc-test-last-elem-nil ()
  "Test vm-last-elem with nil."
  (should (null (vm-last-elem nil))))

;;; vm-vector-to-list tests

(ert-deftest vm-misc-test-vector-to-list ()
  "Test vm-vector-to-list conversion."
  (should (equal (vm-vector-to-list [a b c]) '(a b c))))

(ert-deftest vm-misc-test-vector-to-list-empty ()
  "Test vm-vector-to-list with empty vector."
  (should (equal (vm-vector-to-list []) nil)))

;;; vm-zip-lists tests

(ert-deftest vm-misc-test-zip-lists ()
  "Test vm-zip-lists interleaves two lists."
  (should (equal (vm-zip-lists '(a b) '(1 2)) '(a 1 b 2))))

(ert-deftest vm-misc-test-zip-lists-empty ()
  "Test vm-zip-lists with empty lists."
  (should (equal (vm-zip-lists nil nil) nil)))

(ert-deftest vm-misc-test-zip-lists-unequal-error ()
  "Test vm-zip-lists signals error for unequal length lists."
  (should-error (vm-zip-lists '(a b) '(1))))

;;; vm-elems tests

(ert-deftest vm-misc-test-elems ()
  "Test vm-elems selects first N elements."
  (should (equal (vm-elems 2 '(a b c d)) '(a b))))

(ert-deftest vm-misc-test-elems-more-than-length ()
  "Test vm-elems when N > list length."
  (should (equal (vm-elems 10 '(a b c)) '(a b c))))

(ert-deftest vm-misc-test-elems-zero ()
  "Test vm-elems with zero."
  (should (equal (vm-elems 0 '(a b c)) nil)))

;;; vm-find tests

(ert-deftest vm-misc-test-find ()
  "Test vm-find returns position of matching element."
  (should (= (vm-find '(1 2 3 4) (lambda (x) (= x 3))) 2)))

(ert-deftest vm-misc-test-find-not-found ()
  "Test vm-find returns nil when not found."
  (should (null (vm-find '(1 2 3) (lambda (x) (= x 5))))))

(ert-deftest vm-misc-test-find-first ()
  "Test vm-find returns first match position."
  (should (= (vm-find '(1 2 2 3) (lambda (x) (= x 2))) 1)))

;;; vm-delete tests

(ert-deftest vm-misc-test-delete ()
  "Test vm-delete removes matching elements."
  (should (equal (vm-delete (lambda (x) (= 0 (% x 2))) (list 1 2 3 4 5)) '(1 3 5))))

(ert-deftest vm-misc-test-delete-retain ()
  "Test vm-delete with retain flag keeps matching elements."
  (should (equal (vm-delete (lambda (x) (= 0 (% x 2))) (list 1 2 3 4 5) t) '(2 4))))

(ert-deftest vm-misc-test-delete-none ()
  "Test vm-delete when nothing matches."
  (should (equal (vm-delete (lambda (x) (= 0 (% x 2))) (list 1 3 5)) '(1 3 5))))

;;; vm-delete-non-matching-strings tests

(ert-deftest vm-misc-test-delete-non-matching-strings ()
  "Test vm-delete-non-matching-strings keeps matching strings."
  (should (equal (vm-delete-non-matching-strings "^a" '("apple" "banana" "apricot"))
                 '("apple" "apricot"))))

;;; vm-coding-system-p tests

(ert-deftest vm-misc-test-coding-system-p-valid ()
  "Test vm-coding-system-p with valid coding system."
  (should (vm-coding-system-p 'utf-8)))

(ert-deftest vm-misc-test-coding-system-p-invalid ()
  "Test vm-coding-system-p with invalid coding system."
  (should-not (vm-coding-system-p 'nonexistent-coding-system-12345)))

;;; vm-nonneg-string tests

(ert-deftest vm-misc-test-nonneg-string-positive ()
  "Test vm-nonneg-string with positive number."
  (should (equal (vm-nonneg-string 42) "42")))

(ert-deftest vm-misc-test-nonneg-string-zero ()
  "Test vm-nonneg-string with zero."
  (should (equal (vm-nonneg-string 0) "0")))

(ert-deftest vm-misc-test-nonneg-string-negative ()
  "Test vm-nonneg-string with negative number returns ?."
  (should (equal (vm-nonneg-string -1) "?")))

;;; Temporary directory macro tests

(ert-deftest vm-misc-test-with-temp-dir ()
  "Test vm-test-with-temp-dir creates and cleans up directory."
  (let (temp-dir-path)
    (vm-test-with-temp-dir
      (setq temp-dir-path default-directory)
      (should (file-directory-p temp-dir-path))
      ;; Create a file to ensure cleanup works
      (write-region "test" nil "test-file.txt"))
    ;; Directory should be cleaned up
    (should-not (file-exists-p temp-dir-path))))

;;; Temporary buffer macro tests

(ert-deftest vm-misc-test-with-temp-buffer ()
  "Test vm-test-with-temp-buffer creates buffer with content."
  (should (equal (vm-test-with-temp-buffer "hello world"
                   (buffer-string))
                 "hello world")))

(ert-deftest vm-misc-test-with-temp-buffer-point ()
  "Test vm-test-with-temp-buffer positions point at beginning."
  (should (= (vm-test-with-temp-buffer "hello"
               (point))
             1)))

;;; vm-copy tests

(ert-deftest vm-misc-test-copy-list ()
  "Test vm-copy with a list."
  (let* ((original '(a b c))
         (copy (vm-copy original)))
    (should (equal original copy))
    (should-not (eq original copy))))

(ert-deftest vm-misc-test-copy-nested-list ()
  "Test vm-copy with nested lists."
  (let* ((original '((a b) (c d)))
         (copy (vm-copy original)))
    (should (equal original copy))
    (should-not (eq original copy))
    (should-not (eq (car original) (car copy)))))

(ert-deftest vm-misc-test-copy-vector ()
  "Test vm-copy with a vector."
  (let* ((original [a b c])
         (copy (vm-copy original)))
    (should (equal original copy))
    (should-not (eq original copy))))

(ert-deftest vm-misc-test-copy-string ()
  "Test vm-copy with a string."
  (let* ((original "hello")
         (copy (vm-copy original)))
    (should (equal original copy))
    (should-not (eq original copy))))

(ert-deftest vm-misc-test-copy-atom ()
  "Test vm-copy with atoms returns same object."
  (should (eq (vm-copy 42) 42))
  (should (eq (vm-copy 'symbol) 'symbol)))

;;; vm-delqual tests

(ert-deftest vm-misc-test-delqual-basic ()
  "Test vm-delqual removes equal element."
  (should (equal (vm-delqual "b" (list "a" "b" "c")) '("a" "c"))))

(ert-deftest vm-misc-test-delqual-multiple ()
  "Test vm-delqual removes multiple occurrences."
  (should (equal (vm-delqual "b" (list "a" "b" "b" "c")) '("a" "c"))))

(ert-deftest vm-misc-test-delqual-first ()
  "Test vm-delqual removes first element."
  (should (equal (vm-delqual "a" (list "a" "b" "c")) '("b" "c"))))

(ert-deftest vm-misc-test-delqual-not-found ()
  "Test vm-delqual when element not in list."
  (should (equal (vm-delqual "x" (list "a" "b" "c")) '("a" "b" "c"))))

;;; vm-find-all tests

(ert-deftest vm-misc-test-find-all-basic ()
  "Test vm-find-all returns all matching elements."
  (should (equal (vm-find-all '(1 2 3 4 5 6) (lambda (x) (= 0 (% x 2))))
                 '(2 4 6))))

(ert-deftest vm-misc-test-find-all-none ()
  "Test vm-find-all when nothing matches."
  (should (null (vm-find-all '(1 3 5) (lambda (x) (= 0 (% x 2)))))))

(ert-deftest vm-misc-test-find-all-all ()
  "Test vm-find-all when all match."
  (should (equal (vm-find-all '(2 4 6) (lambda (x) (= 0 (% x 2))))
                 '(2 4 6))))

;;; vm-elems-of tests

(ert-deftest vm-misc-test-elems-of-basic ()
  "Test vm-elems-of returns unique elements."
  (should (equal (sort (vm-elems-of '(a b a c b c)) #'string<)
                 '(a b c))))

(ert-deftest vm-misc-test-elems-of-no-duplicates ()
  "Test vm-elems-of with no duplicates."
  (should (equal (vm-elems-of '(a b c)) '(a b c))))

(ert-deftest vm-misc-test-elems-of-empty ()
  "Test vm-elems-of with empty list."
  (should (null (vm-elems-of nil))))

;;; vm-for-all tests

(ert-deftest vm-misc-test-for-all-true ()
  "Test vm-for-all when all satisfy predicate."
  (should (vm-for-all '(2 4 6) (lambda (x) (= 0 (% x 2))))))

(ert-deftest vm-misc-test-for-all-false ()
  "Test vm-for-all when some don't satisfy predicate."
  (should-not (vm-for-all '(1 2 4 6) (lambda (x) (= 0 (% x 2))))))

(ert-deftest vm-misc-test-for-all-empty ()
  "Test vm-for-all with empty list returns t."
  (should (vm-for-all nil (lambda (x) (= 0 (% x 2))))))

;;; vm-mapvector tests

(ert-deftest vm-misc-test-mapvector-basic ()
  "Test vm-mapvector applies function to vector elements."
  (should (equal (vm-mapvector #'1+ [1 2 3]) [2 3 4])))

(ert-deftest vm-misc-test-mapvector-empty ()
  "Test vm-mapvector with empty vector."
  (should (equal (vm-mapvector #'1+ []) [])))

(ert-deftest vm-misc-test-mapvector-strings ()
  "Test vm-mapvector with string function."
  (should (equal (vm-mapvector #'upcase ["a" "b" "c"]) ["A" "B" "C"])))

;;; vm-mapcar tests

(ert-deftest vm-misc-test-mapcar-two-lists ()
  "Test vm-mapcar with two lists."
  (should (equal (vm-mapcar #'+ '(1 2 3) '(10 20 30)) '(11 22 33))))

(ert-deftest vm-misc-test-mapcar-three-lists ()
  "Test vm-mapcar with three lists."
  (should (equal (vm-mapcar #'+ '(1 2) '(10 20) '(100 200)) '(111 222))))

(ert-deftest vm-misc-test-mapcar-single-list ()
  "Test vm-mapcar with single list."
  (should (equal (vm-mapcar #'1+ '(1 2 3)) '(2 3 4))))

;;; vm-zip-vectors tests

(ert-deftest vm-misc-test-zip-vectors-basic ()
  "Test vm-zip-vectors interleaves two vectors."
  (should (equal (vm-zip-vectors [a b] [1 2]) [a 1 b 2])))

(ert-deftest vm-misc-test-zip-vectors-empty ()
  "Test vm-zip-vectors with empty vectors."
  (should (equal (vm-zip-vectors [] []) [])))

(ert-deftest vm-misc-test-zip-vectors-unequal-error ()
  "Test vm-zip-vectors errors on unequal lengths."
  (should-error (vm-zip-vectors [a b] [1])))

;;; vm-extend-vector tests

(ert-deftest vm-misc-test-extend-vector-basic ()
  "Test vm-extend-vector extends to given length."
  (let ((result (vm-extend-vector [a b] 4)))
    (should (equal (length result) 4))
    (should (equal (aref result 0) 'a))
    (should (equal (aref result 1) 'b))
    (should (null (aref result 2)))
    (should (null (aref result 3)))))

(ert-deftest vm-misc-test-extend-vector-with-fill ()
  "Test vm-extend-vector with fill value."
  (let ((result (vm-extend-vector [a b] 4 'x)))
    (should (equal (aref result 2) 'x))
    (should (equal (aref result 3) 'x))))

(ert-deftest vm-misc-test-extend-vector-no-change ()
  "Test vm-extend-vector when already long enough."
  (should (equal (vm-extend-vector [a b c] 2) [a b c])))

;;; vm-url-decode-string tests

(ert-deftest vm-misc-test-url-decode-basic ()
  "Test vm-url-decode-string with basic encoding."
  (should (equal (vm-url-decode-string "hello%20world") "hello world")))

(ert-deftest vm-misc-test-url-decode-special-chars ()
  "Test vm-url-decode-string with special characters."
  (should (equal (vm-url-decode-string "a%2Fb%3Fc") "a/b?c")))

(ert-deftest vm-misc-test-url-decode-no-encoding ()
  "Test vm-url-decode-string with no encoded chars."
  (should (equal (vm-url-decode-string "hello") "hello")))

(ert-deftest vm-misc-test-url-decode-case-insensitive ()
  "Test vm-url-decode-string handles case-insensitive hex."
  (should (equal (vm-url-decode-string "hello%2bworld") "hello+world"))
  (should (equal (vm-url-decode-string "hello%2Bworld") "hello+world")))

;;; vm-obarray-to-string-list tests

(ert-deftest vm-misc-test-obarray-to-string-list ()
  "Test vm-obarray-to-string-list converts obarray to list."
  (let ((ob (make-vector 7 0)))
    (intern "foo" ob)
    (intern "bar" ob)
    (intern "baz" ob)
    (let ((result (vm-obarray-to-string-list ob)))
      (should (= (length result) 3))
      (should (member "foo" result))
      (member "bar" result)
      (should (member "baz" result)))))

;;; vm-obarray-empty-p tests

(ert-deftest vm-misc-test-obarray-empty-p ()
  "`vm-obarray-empty-p' answers what `null' cannot ask.
An obarray used as a set is a vector, so it is true whether anything has been
interned in it or not.  Testing one with `null' is what #572 was."
  (let ((ob (make-vector 7 0)))
    (should (vm-obarray-empty-p ob))
    ;; The thing that made the bug: it is not nil when empty.
    (should ob)
    (intern "foo" ob)
    (should-not (vm-obarray-empty-p ob))
    ;; Interning the same name again does not make it emptier or fuller.
    (intern "foo" ob)
    (should-not (vm-obarray-empty-p ob))
    (unintern "foo" ob)
    (should (vm-obarray-empty-p ob))))

;;; vm-time-difference tests

(ert-deftest vm-misc-test-time-difference-basic ()
  "Test vm-time-difference calculates time difference."
  (let ((t1 '(0 10 0))    ; 10 seconds
        (t2 '(0 5 0)))    ; 5 seconds
    (should (= (vm-time-difference t1 t2) 5))))

(ert-deftest vm-misc-test-time-difference-with-high ()
  "Test vm-time-difference with high-order bits."
  (let ((t1 '(1 0 0))     ; 65536 seconds
        (t2 '(0 0 0)))    ; 0 seconds
    (should (= (vm-time-difference t1 t2) 65536))))

;;; vm-match-data tests

(ert-deftest vm-misc-test-match-data ()
  "Test vm-match-data returns match data list."
  (string-match "\\(a\\)\\(b\\)" "ab")
  (let ((md (vm-match-data)))
    (should (listp md))
    (should (= (length md) 6))))

;;; vm-error-free-call tests

(ert-deftest vm-misc-test-error-free-call-success ()
  "Test vm-error-free-call with successful call."
  (should (= (vm-error-free-call #'+ 1 2) 3)))

(ert-deftest vm-misc-test-error-free-call-error ()
  "Test vm-error-free-call swallows error."
  (should (null (vm-error-free-call #'/ 1 0))))

;;; vm-symbol-lists-intersect-p tests

(ert-deftest vm-misc-test-symbol-lists-intersect-yes ()
  "Test vm-symbol-lists-intersect-p when lists intersect."
  (should (vm-symbol-lists-intersect-p '(a b c) '(c d e))))

(ert-deftest vm-misc-test-symbol-lists-intersect-no ()
  "Test vm-symbol-lists-intersect-p when lists don't intersect."
  (should-not (vm-symbol-lists-intersect-p '(a b c) '(d e f))))

(ert-deftest vm-misc-test-symbol-lists-intersect-empty ()
  "Test vm-symbol-lists-intersect-p with empty list."
  (should-not (vm-symbol-lists-intersect-p nil '(a b c))))

;;; vm-delete-all-match test (new - complements existing delete tests)

(ert-deftest vm-misc-test-delete-all-match ()
  "Test vm-delete when everything matches."
  (should (equal (vm-delete #'numberp '(1 2 3)) nil)))

;;; vm-delete-non-matching-strings-none test (new)

(ert-deftest vm-misc-test-delete-non-matching-strings-none ()
  "Test vm-delete-non-matching-strings when none match."
  (should (equal (vm-delete-non-matching-strings "^z" '("apple" "banana"))
                 nil)))

;;; vm-delete-duplicates tests

(ert-deftest vm-misc-test-delete-duplicates-basic ()
  "Test vm-delete-duplicates removes duplicates."
  (let ((result (vm-delete-duplicates '("a" "b" "a" "c" "b"))))
    (should (member "a" result))
    (should (member "b" result))
    (should (member "c" result))
    (should (= (length result) 3))))

(ert-deftest vm-misc-test-delete-duplicates-all ()
  "Test vm-delete-duplicates with all flag."
  ;; With all=t, removes all occurrences of duplicated items
  (let ((result (vm-delete-duplicates '("a" "b" "a" "c") t)))
    (should (member "b" result))
    (should (member "c" result))
    (should-not (member "a" result))))

;;; vm-delete-directory-file-names tests

(ert-deftest vm-misc-test-delete-directory-file-names ()
  "Test vm-delete-directory-file-names removes . and .."
  (let ((result (vm-delete-directory-file-names '("." ".." "file1" "file2"))))
    (should (equal result '("file1" "file2")))))

;;; vm-delete-backup-file-names tests

(ert-deftest vm-misc-test-delete-backup-file-names ()
  "Test vm-delete-backup-file-names removes backup files."
  (let ((result (vm-delete-backup-file-names '("file.txt" "file.txt~" "other.el"))))
    (should (equal result '("file.txt" "other.el")))))

;;; vm-delete-auto-save-file-names tests

(ert-deftest vm-misc-test-delete-auto-save-file-names ()
  "Test vm-delete-auto-save-file-names removes auto-save files."
  (let ((result (vm-delete-auto-save-file-names '("file.txt" "#file.txt#" "other"))))
    (should (equal result '("file.txt" "other")))))

;;; vm-mapc tests

(ert-deftest vm-misc-test-mapc-basic ()
  "Test vm-mapc iterates over lists."
  (let ((result nil))
    (vm-mapc (lambda (x y) (push (list x y) result))
             '(1 2 3) '(a b c))
    (should (equal (reverse result) '((1 a) (2 b) (3 c))))))

;;; vm-buffer-string-no-properties tests

(ert-deftest vm-misc-test-buffer-string-no-properties ()
  "Test vm-buffer-string-no-properties returns string without properties."
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 1 6 'face 'bold)
    (let ((result (vm-buffer-string-no-properties)))
      (should (equal result "hello world"))
      ;; Should have no text properties
      (should (null (text-properties-at 0 result))))))

;;; vm-substring-no-properties tests

(ert-deftest vm-misc-test-substring-no-properties ()
  "Test vm-substring-no-properties returns substring without properties."
  (let* ((str (propertize "hello world" 'face 'bold))
         (result (vm-substring-no-properties str 0 5)))
    (should (equal result "hello"))
    (should (null (text-properties-at 0 result)))))

;;; vm-char-to-int tests (XEmacs compatibility)

(ert-deftest vm-misc-test-char-to-int-exists ()
  "Test vm-char-to-int function exists."
  (should (fboundp 'vm-char-to-int)))

;;; vm-locate-executable-file tests

(ert-deftest vm-misc-test-locate-executable-file-searches-exec-path ()
  "A program is looked for on `exec-path', and nil comes back when there is
none.  VM asks this before offering an external viewer, so a wrong answer
either loses a viewer or runs nothing."
  (let ((dir (file-name-as-directory (make-temp-file "vm-misc-test" t))))
    (unwind-protect
        (let ((program (expand-file-name "vm-misc-test-program" dir)))
          (write-region "#!/bin/sh
exit 0
" nil program nil 'quiet)
          (set-file-modes program #o755)
          (let ((exec-path (list dir)))
            (should (equal (vm-locate-executable-file "vm-misc-test-program")
                           program))
            (should-not (vm-locate-executable-file "vm-no-such-program-here")))
          ;; not on the path, not found
          (let ((exec-path nil))
            (should-not (vm-locate-executable-file "vm-misc-test-program"))))
      (delete-directory dir t))))

;;; vm-run-command tests

;;; vm-octal tests

(ert-deftest vm-misc-test-octal-reads-its-argument-as-octal ()
  "`vm-octal' takes a decimal-looking integer and reads its digits as octal.
It is how the folder permission defaults are written: (vm-octal 600) is the
0600 a reader expects, not six hundred."
  (should (= 384 (vm-octal 600)))            ; 0600
  (should (= 511 (vm-octal 777)))            ; 0777
  (should (= 420 (vm-octal 644)))            ; 0644
  (should (= 0 (vm-octal 0)))
  (should (= 8 (vm-octal 10)))
  ;; and it refuses what is not octal rather than returning a wrong number
  (should-error (vm-octal 8) :type 'error)
  (should-error (vm-octal 649) :type 'error))

(ert-deftest vm-misc-test-octal-is-what-the-defaults-use ()
  "The permission defaults come out as the octal they are written as."
  (should (= 384 (default-value 'vm-default-folder-permission-bits))))

;;; vm-generate-new-buffer tests

(ert-deftest vm-misc-test-generate-new-multibyte-buffer ()
  "Test vm-generate-new-multibyte-buffer creates buffer."
  (let ((buf (vm-generate-new-multibyte-buffer "test-multi")))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (string-match "test-multi" (buffer-name buf))))
      (kill-buffer buf))))

(ert-deftest vm-misc-test-generate-new-unibyte-buffer ()
  "Test vm-generate-new-unibyte-buffer creates buffer."
  (let ((buf (vm-generate-new-unibyte-buffer "test-uni")))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (string-match "test-uni" (buffer-name buf))))
      (kill-buffer buf))))

;;; vm-make-work-buffer tests

(ert-deftest vm-misc-test-make-work-buffer ()
  "Test vm-make-work-buffer creates temporary buffer."
  (let ((buf (vm-make-work-buffer)))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (string-match "vm-work" (buffer-name buf))))
      (kill-buffer buf))))

(ert-deftest vm-misc-test-make-multibyte-work-buffer ()
  "Test vm-make-multibyte-work-buffer creates temporary buffer."
  (let ((buf (vm-make-multibyte-work-buffer)))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (string-match "vm-work" (buffer-name buf))))
      (kill-buffer buf))))

;;; vm-with-string-as-temp-buffer tests

(ert-deftest vm-misc-test-with-string-as-temp-buffer-returns-the-result ()
  "The string is worked on in a buffer of its own and the result returned."
  (should (equal "HELLO"
                 (vm-with-string-as-temp-buffer
                  "hello" (lambda () (upcase-region (point-min) (point-max))))))
  ;; the work buffer does not outlive the call
  (let ((before (length (buffer-list))))
    (vm-with-string-as-temp-buffer "x" #'ignore)
    (should (= before (length (buffer-list)))))
  ;; and it is multibyte, so a non-ASCII string is not mangled
  (should (equal "café" (vm-with-string-as-temp-buffer "café" #'ignore))))

;;; vm-md5-string tests

(ert-deftest vm-misc-test-md5-string ()
  "Test vm-md5-string returns MD5 hash."
  (let ((hash (vm-md5-string "hello")))
    (should (stringp hash))
    ;; MD5 is 32 hex chars
    (should (= (length hash) 32))
    ;; Should be consistent
    (should (equal hash (vm-md5-string "hello")))))

;;; vm-xor-string tests

(ert-deftest vm-misc-test-xor-string-is-its-own-inverse ()
  "`vm-xor-string' xored twice with the same key gives the original back.
It is used on the password VM keeps in memory, so the round trip is the whole
of its job."
  (require 'vm-crypto)
  (let* ((text "hunter2!") (key "abcdefgh"))
    (should (equal text (vm-xor-string (vm-xor-string text key) key)))
    ;; and the once-xored form is not the text
    (should-not (equal text (vm-xor-string text key))))
  ;; equal lengths are required rather than quietly truncated
  (should-error (vm-xor-string "abc" "ab") :type 'error))

(ert-deftest vm-misc-test-char-to-int-does-not-name-a-misspelt-feature ()
  "REGRESSION: `vm-char-to-int' asked for `xeamcs'.
The XEmacs branch of that alias could therefore never be taken.  That branch
is gone with XEmacs support, but the point stands: a misspelt feature name
is silent, so this checks every one VM asks about."
  (let ((known '(berkeley-db gtk window-system vm-pgg vm-epg))
        (unknown nil))
    (dolist (file (directory-files vm-test-lisp-dir t "\\.el\\'"))
      (unless (string-match-p "vm-autoloads\\|vm-cus-load" file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "(featurep '\\([a-z0-9-]+\\)" nil t)
            (let ((f (intern (match-string 1))))
              (unless (memq f known)
                (push (cons (file-name-nondirectory file) f) unknown)))))))
    (should (equal nil unknown))))

;;; vm-insert-region-from-buffer tests

(ert-deftest vm-misc-test-insert-region-from-buffer-full ()
  "Test vm-insert-region-from-buffer copies entire buffer."
  (let ((src-buf (generate-new-buffer "src"))
        (dst-buf (generate-new-buffer "dst")))
    (unwind-protect
        (progn
          (with-current-buffer src-buf
            (insert "source content"))
          (with-current-buffer dst-buf
            (vm-insert-region-from-buffer src-buf))
          (should (equal (with-current-buffer dst-buf (buffer-string))
                         "source content")))
      (kill-buffer src-buf)
      (kill-buffer dst-buf))))

(ert-deftest vm-misc-test-insert-region-from-buffer-partial ()
  "Test vm-insert-region-from-buffer copies specified region."
  (let ((src-buf (generate-new-buffer "src"))
        (dst-buf (generate-new-buffer "dst")))
    (unwind-protect
        (progn
          (with-current-buffer src-buf
            (insert "0123456789"))
          (with-current-buffer dst-buf
            (vm-insert-region-from-buffer src-buf 4 7))
          (should (equal (with-current-buffer dst-buf (buffer-string))
                         "345")))
      (kill-buffer src-buf)
      (kill-buffer dst-buf))))

;;; vm-extent/overlay tests

(ert-deftest vm-misc-test-make-extent ()
  "Test vm-make-extent creates overlay."
  (with-temp-buffer
    (insert "hello world")
    (let ((ext (vm-make-extent 1 6)))
      (should ext)
      (should (= (vm-extent-start-position ext) 1))
      (should (= (vm-extent-end-position ext) 6))
      (vm-delete-extent ext))))

(ert-deftest vm-misc-test-set-extent-property ()
  "Test vm-set-extent-property sets property."
  (with-temp-buffer
    (insert "hello world")
    (let ((ext (vm-make-extent 1 6)))
      (vm-set-extent-property ext 'face 'bold)
      (should (eq (vm-extent-property ext 'face) 'bold))
      (vm-delete-extent ext))))

(ert-deftest vm-misc-test-extent-object ()
  "Test vm-extent-object returns buffer."
  (with-temp-buffer
    (insert "hello world")
    (let ((ext (vm-make-extent 1 6)))
      (should (eq (vm-extent-object ext) (current-buffer)))
      (vm-delete-extent ext))))

(ert-deftest vm-misc-test-set-extent-endpoints ()
  "Test vm-set-extent-endpoints moves overlay."
  (with-temp-buffer
    (insert "hello world")
    (let ((ext (vm-make-extent 1 6)))
      (vm-set-extent-endpoints ext 7 12)
      (should (= (vm-extent-start-position ext) 7))
      (should (= (vm-extent-end-position ext) 12))
      (vm-delete-extent ext))))

(ert-deftest vm-misc-test-map-extents ()
  "Test vm-map-extents iterates over extents."
  (with-temp-buffer
    (insert "hello world")
    (let ((ext1 (vm-make-extent 1 6))
          (ext2 (vm-make-extent 7 12))
          (count 0))
      (vm-map-extents (lambda (ext _)
                        (when (memq ext (list ext1 ext2))
                          (setq count (1+ count)))))
      (should (>= count 2))
      (vm-delete-extent ext1)
      (vm-delete-extent ext2))))

;;; vm-set-region-face tests

(ert-deftest vm-misc-test-set-region-face ()
  "Test vm-set-region-face applies face to region."
  (with-temp-buffer
    (insert "hello world")
    (vm-set-region-face 1 6 'bold)
    ;; Should have created an overlay with face
    (let ((overlays (overlays-at 3)))
      (should overlays)
      (should (eq (overlay-get (car overlays) 'face) 'bold)))))

;;; vm-default-buffer-substring-no-properties tests

(ert-deftest vm-misc-test-default-buffer-substring-no-properties ()
  "Test vm-default-buffer-substring-no-properties removes properties."
  (with-temp-buffer
    (insert (propertize "hello" 'face 'bold))
    (insert " world")
    (let ((result (vm-default-buffer-substring-no-properties 1 6)))
      (should (equal result "hello"))
      (should (null (text-properties-at 0 result))))))

;;; vm-call-process tests

(ert-deftest vm-misc-test-call-process-stdout ()
  "Test vm-call-process captures stdout to current buffer."
  (with-temp-buffer
    (vm-call-process "echo" nil t '("hello world"))
    (should (equal (string-trim (buffer-string)) "hello world"))))

(ert-deftest vm-misc-test-call-process-exit-status-success ()
  "Test vm-call-process returns exit status 0 on success."
  (with-temp-buffer
    (should (= (vm-call-process "true" nil t nil) 0))))

(ert-deftest vm-misc-test-call-process-exit-status-failure ()
  "Test vm-call-process returns non-zero exit status on failure."
  (with-temp-buffer
    (should (= (vm-call-process "false" nil t nil) 1))))

(ert-deftest vm-misc-test-call-process-with-args ()
  "Test vm-call-process passes arguments correctly."
  (with-temp-buffer
    (vm-call-process "printf" nil t '("%s-%s" "foo" "bar"))
    (should (equal (buffer-string) "foo-bar"))))

(ert-deftest vm-misc-test-call-process-stderr-separate ()
  "Test vm-call-process keeps stderr separate from stdout."
  (with-temp-buffer
    ;; Run a command that outputs to both stdout and stderr
    ;; sh -c 'echo stdout; echo stderr >&2'
    (vm-call-process "sh" nil t '("-c" "echo stdout; echo stderr >&2"))
    ;; Buffer should only contain stdout
    (should (equal (string-trim (buffer-string)) "stdout"))))

(ert-deftest vm-misc-test-call-process-stderr-to-messages ()
  "Test vm-call-process reports stderr via message."
  (with-temp-buffer
    (let ((messages nil))
      ;; Capture messages
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (vm-call-process "sh" nil t '("-c" "echo stderr >&2")))
      ;; Should have captured a message with stderr content
      (should (cl-some (lambda (msg) (string-match "stderr" msg)) messages)))))

(ert-deftest vm-misc-test-call-process-infile ()
  "Test vm-call-process with input file."
  (let ((tempfile (make-temp-file "vm-test")))
    (unwind-protect
        (progn
          (with-temp-file tempfile
            (insert "input data"))
          (with-temp-buffer
            (vm-call-process "cat" tempfile t nil)
            (should (equal (buffer-string) "input data"))))
      (delete-file tempfile))))

(ert-deftest vm-misc-test-call-process-to-named-buffer ()
  "Test vm-call-process with named buffer as destination."
  (let ((buf (generate-new-buffer " *test-output*")))
    (unwind-protect
        (progn
          (vm-call-process "echo" nil buf '("test output"))
          (should (equal (string-trim (with-current-buffer buf
                                        (buffer-string)))
                         "test output")))
      (kill-buffer buf))))

(ert-deftest vm-misc-test-call-process-binary-safe ()
  "Test vm-call-process handles binary data correctly."
  (with-temp-buffer
    ;; Output some bytes including null
    (vm-call-process "printf" nil t '("A\\0B\\0C"))
    (should (equal (buffer-string) "A\0B\0C"))))

;;; auth-source tests

(defmacro vm-misc-test-with-authinfo (lines &rest body)
  "Run BODY with `auth-sources' pointing at a temp authinfo of LINES."
  (declare (indent 1))
  `(let ((file (make-temp-file "vm-authinfo")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,lines))
           (let ((auth-sources (list file))
                 (auth-source-do-cache nil))
             (auth-source-forget-all-cached)
             ,@body))
       (delete-file file)
       (auth-source-forget-all-cached))))

(ert-deftest vm-misc-test-auth-source-password-found ()
  "Test that a password is read from auth-source by host, port and user."
  (vm-misc-test-with-authinfo
      "machine mail.example.com login user port 143 password s3cret\n"
    (should (equal (vm-auth-source-password '("mail.example.com") 143 "user")
                   "s3cret"))))

(ert-deftest vm-misc-test-auth-source-password-string-port ()
  "Test that a service-name port works as well as a numeric one.
POP passes the port through as a string when it is not all digits."
  (vm-misc-test-with-authinfo
      "machine pop.example.com login user port pop3 password s3cret\n"
    (should (equal (vm-auth-source-password '("pop.example.com") "pop3" "user")
                   "s3cret"))))

(ert-deftest vm-misc-test-auth-source-password-second-host ()
  "Test that the second name is tried when the first does not match.
VM looks up both the account name and the real host name."
  (vm-misc-test-with-authinfo
      "machine mail.example.com login user port 143 password s3cret\n"
    (should (equal (vm-auth-source-password
                    '("work-account" "mail.example.com") 143 "user")
                   "s3cret"))))

(ert-deftest vm-misc-test-auth-source-password-account-name-wins ()
  "Test that the account name is preferred over the host name."
  (vm-misc-test-with-authinfo
      (concat "machine work-account login user port 143 password by-account\n"
              "machine mail.example.com login user port 143 password by-host\n")
    (should (equal (vm-auth-source-password
                    '("work-account" "mail.example.com") 143 "user")
                   "by-account"))))

(ert-deftest vm-misc-test-auth-source-password-wrong-user ()
  "Test that an entry for a different user is not used."
  (vm-misc-test-with-authinfo
      "machine mail.example.com login someone-else port 143 password s3cret\n"
    (should (null (vm-auth-source-password '("mail.example.com") 143 "user")))))

(ert-deftest vm-misc-test-auth-source-password-nil-user ()
  "Test that a nil user yields nothing rather than someone else's password.
`auth-source-search' reads a nil :user as no constraint rather than as
a wildcard to match, so it returns whichever entry for the host comes
first -- which would be another account's password."
  (vm-misc-test-with-authinfo
      (concat "machine mail.example.com login alice port 143 password alice-pw\n"
              "machine mail.example.com login bob port 143 password bob-pw\n")
    ;; each named user still gets their own
    (should (equal (vm-auth-source-password '("mail.example.com") 143 "alice")
                   "alice-pw"))
    (should (equal (vm-auth-source-password '("mail.example.com") 143 "bob")
                   "bob-pw"))
    ;; and an unnamed one gets nobody's
    (should (null (vm-auth-source-password '("mail.example.com") 143 nil)))))

(ert-deftest vm-misc-test-auth-source-password-no-match ()
  "Test that nil is returned when nothing matches."
  (vm-misc-test-with-authinfo
      "machine other.example.com login user port 143 password s3cret\n"
    (should (null (vm-auth-source-password '("mail.example.com") 143 "user")))))

(ert-deftest vm-misc-test-auth-source-password-nil-hosts-skipped ()
  "Test that a nil host in the list is ignored, not searched for.
`vm-imap-account-name-for-spec' returns nil when the spec is not in
`vm-imap-account-alist'."
  (vm-misc-test-with-authinfo
      "machine mail.example.com login user port 143 password s3cret\n"
    (should (equal (vm-auth-source-password '(nil "mail.example.com") 143 "user")
                   "s3cret"))
    (should (null (vm-auth-source-password '(nil) 143 "user")))))


;;; how much VM says (issue #508)

(defun vm-misc-test--messages-at (verbosity thunk)
  "Return the messages THUNK emits with `vm-verbosity' set to VERBOSITY."
  (let ((vm-verbosity verbosity)
        (vm-verbal-time 0)
        (said nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args)
                 (push (if (car args) (apply #'format args) "") said)
                 (car said))))
      (funcall thunk))
    (nreverse said)))

(ert-deftest vm-misc-test-verbosity-defaults-to-the-documented-normal-level ()
  "REGRESSION: `vm-verbosity' defaults to the level it calls normal.
Issue #508: the default was 8 while the variable's own documentation called 5
the normal level, so VM was three levels chattier than it said.  Saving a dozen
messages to an IMAP folder reported \"Checking IMAP connection to ...\" a dozen
times, that message being level 7."
  (require 'vm-vars)
  (should (= 5 (default-value 'vm-verbosity)))
  ;; the documented normal level, so what is documented has to say 5
  (should (string-match-p "5 - normal level"
                          (documentation-property 'vm-verbosity
                                                  'variable-documentation))))

(ert-deftest vm-misc-test-inform-shows-its-level-or-lower ()
  "`vm-inform' speaks when its level is at or below `vm-verbosity'.
The direction is worth pinning: a larger `vm-verbosity' means more output, not
less, and the boundary is inclusive."
  (require 'vm-misc)
  (should (equal '("five") (vm-misc-test--messages-at
                            5 (lambda () (vm-inform 5 "five")))))
  (should (equal nil (vm-misc-test--messages-at
                      5 (lambda () (vm-inform 6 "six")))))
  (should (equal '("six") (vm-misc-test--messages-at
                           6 (lambda () (vm-inform 6 "six")))))
  ;; and at the default, the message from #508 is silent while a command result
  ;; is not
  (should (equal nil (vm-misc-test--messages-at
                      (default-value 'vm-verbosity)
                      (lambda () (vm-inform 7 "Checking IMAP connection")))))
  (should (equal '("3 messages saved")
                 (vm-misc-test--messages-at
                  (default-value 'vm-verbosity)
                  (lambda () (vm-inform 5 "3 messages saved"))))))

(ert-deftest vm-misc-test-warnings-survive-the-default-verbosity ()
  "No warning sits above the default verbosity, so none is silenced by it.
`vm-warn' gates on `vm-verbosity' just as `vm-inform' does, which makes
lowering the default a way to lose warnings as well as chatter.  Checked
against the source rather than by calling them, so that a new warning added
above the default fails here instead of going unseen in the field."
  (let ((default (default-value 'vm-verbosity))
        (too-quiet nil))
    (dolist (file (directory-files
                   (expand-file-name "../lisp" vm-test-dir) t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(vm-warn +\\([0-9]+\\)" nil t)
          (when (> (string-to-number (match-string 1)) default)
            (push (format "%s:%d level %s"
                          (file-name-nondirectory file)
                          (line-number-at-pos (match-beginning 0))
                          (match-string 1))
                  too-quiet)))))
    (should (equal nil too-quiet))))

(ert-deftest vm-misc-test-command-results-are-all-at-the-normal-level ()
  "Every \"N messages ...\" report is at level 5, so a command says what it did.
Two in `vm-save-message' were at 7 and so went silent at the default (#508),
while the same report from every other command -- deleted, undeleted, flagged,
marked, archived, pruned -- was at 5.  That was an inconsistency rather than a
decision, and this keeps it from coming back."
  (let ((odd nil))
    (dolist (file (directory-files
                   (expand-file-name "../lisp" vm-test-dir) t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(vm-inform +\\([0-9]+\\) +\"%d message" nil t)
          (unless (= 5 (string-to-number (match-string 1)))
            (push (format "%s:%d level %s"
                          (file-name-nondirectory file)
                          (line-number-at-pos (match-beginning 0))
                          (match-string 1))
                  odd)))))
    (should (equal nil odd))))

;;; Taking a structured header apart

;; The tests of this function that existed were in vm-mime-test.el and were
;; all Content-Type: a semicolon, a quoted parameter value, nothing else.
;; What an address header brings -- a comma inside a quoted phrase, a
;; comment in parentheses, a backslash escape, a header that stops in the
;; middle of one -- was untested.

(defun vm-misc-test--parse (string &optional keep-quotes)
  "Parse STRING as a comma-separated structured header."
  (vm-parse-structured-header string ?, keep-quotes))

(ert-deftest vm-misc-test-a-structured-header-splits-on-its-separator ()
  "An address header is its addresses, and an empty one between two
commas is not an address."
  (should (equal (vm-misc-test--parse "alice@x, bob@y, carol@z")
                 '("alice@x" "bob@y" "carol@z")))
  (should (equal (vm-misc-test--parse "alice@x,, bob@y")
                 '("alice@x" "bob@y"))))

(ert-deftest vm-misc-test-a-separator-in-quotes-is-not-a-separator ()
  "`\"Smith, John\" <js@x>' is one address, not two: the comma is inside
the quoted phrase.  Splitting it would send the mail to `Smith'."
  (should (equal (vm-misc-test--parse "\"Smith, John\" <js@x>, alice@y")
                 '("Smith, John<js@x>" "alice@y"))))

(ert-deftest vm-misc-test-quotes-come-off-unless-they-are-wanted ()
  "The quotes around a phrase are punctuation, so they are removed, and an
escaped quote inside it is not the end of the phrase.  KEEP-QUOTES asks for
the phrase as it was written, which is what re-emitting a header needs."
  (should (equal (vm-misc-test--parse "\"a\\\"b\" <c@x>") '("a\"b<c@x>")))
  (should (equal (vm-misc-test--parse "\"a\\\"b\" <c@x>" t)
                 '("\"a\"b\"<c@x>")))
  (should (equal (vm-misc-test--parse "\"a\", \"b\"") '("a" "b"))))

(ert-deftest vm-misc-test-a-comment-is-not-part-of-the-address ()
  "A comment in parentheses is dropped, including one with parentheses of
its own -- an address is what is left when the comments are taken out."
  (should (equal (vm-misc-test--parse "alice@x (Alice Smith)") '("alice@x")))
  (should (equal (vm-misc-test--parse "a@x (out (in) still) b") '("a@xb")))
  (should (equal (vm-misc-test--parse "a@x (smiley \\) here) b") '("a@xb"))))

(ert-deftest vm-misc-test-space-between-the-pieces-is-not-kept ()
  "Space outside quotes is not part of what it separates: the pieces are
run together, which is what makes `John Smith <j@x>' a single token."
  (should (equal (vm-misc-test--parse "John Smith <j@x>") '("JohnSmith<j@x>")))
  (should (equal (vm-misc-test--parse "  alice@x  ,  bob@y  ")
                 '("alice@x" "bob@y"))))

(ert-deftest vm-misc-test-a-header-that-stops-in-the-middle-still-parses ()
  "A header cut short -- an unclosed quote, an unclosed comment, a
backslash with nothing after it -- gives back what there was rather than
running off the end of the buffer.  VM parses headers as they arrive, and
a truncated one must not hang the folder."
  (should (equal (vm-misc-test--parse "\"unterminated <a@x>")
                 '("unterminated<a@x>")))
  (should (equal (vm-misc-test--parse "a@x (unterminated") '("a@xunterminated")))
  (should (equal (vm-misc-test--parse "trailing\\") '("trailing\\")))
  (should (equal (vm-misc-test--parse "\"quoted trailing\\") '("quoted trailing"))))

(ert-deftest vm-misc-test-without-a-separator-it-is-all-one-token ()
  "With no separator character the whole header is one item, with the
quotes and comments still taken out.  That is how VM reads a header it has
no list syntax for."
  (should (equal (vm-parse-structured-header "alice@x, bob@y (both)")
                 '("alice@x,bob@y"))))

;;; Pausing to be read (emacs-vm/vm#473)

(ert-deftest vm-misc-test-a-pause-can-be-typed-through ()
  "`vm-pause' waits with `sit-for', so a reader who has read the message
carries on rather than waiting out the rest of it.  `sleep-for' does not
return early for anything, and every message pause in the IMAP and POP code
was one until this was written."
  (let (waited)
    (cl-letf (((symbol-function 'sit-for) (lambda (n) (setq waited n) t))
              ((symbol-function 'sleep-for)
               (lambda (&rest _) (error "vm-pause used sleep-for"))))
      (vm-pause 2)
      (should (equal waited 2)))))

(ert-deftest vm-misc-test-a-pause-of-nothing-does-not-wait ()
  "Zero seconds is no pause at all, which is what `vm-verbal-time' is by
default: every `vm-inform' would otherwise stop for it."
  (let ((waited nil))
    (cl-letf (((symbol-function 'sit-for) (lambda (n) (setq waited n) t)))
      (vm-pause 0)
      (vm-pause nil)
      (should-not waited))))

(ert-deftest vm-misc-test-informing-pauses-for-the-verbal-time ()
  "`vm-inform' shows the message and pauses for `vm-verbal-time', which is
how a reader is given time to see it."
  (let ((vm-verbosity 5)
        (vm-verbal-time 3)
        waited)
    (cl-letf (((symbol-function 'message) (lambda (&rest args) (car args)))
              ((symbol-function 'sit-for) (lambda (n) (setq waited n) t)))
      (vm-inform 5 "something happened")
      (should (equal waited 3)))))

;;; The log (vm-log-level)

(defmacro vm-misc-test--with-log (&rest body)
  "Run BODY with a log buffer of its own, and answer with what it holds."
  (declare (indent 0) (debug t))
  `(let ((vm-log-buffer-name " *vm-misc-test-log*")
         (vm-last-message-time nil))
     (unwind-protect
         (progn ,@body
                (if (get-buffer vm-log-buffer-name)
                    (with-current-buffer vm-log-buffer-name (buffer-string))
                  ""))
       (when (get-buffer vm-log-buffer-name)
         (kill-buffer vm-log-buffer-name)))))

(ert-deftest vm-misc-test-what-is-shown-carries-no-timing ()
  "The message in the echo area says what it always did.  The time goes to
the log, where there is room for it and where it can be read afterwards."
  (let ((vm-log-level nil)
        (said nil))
    (let ((log (vm-misc-test--with-log
                 (setq said (vm-misc-test--messages-at
                             5 (lambda () (vm-inform 5 "five")))))))
      (should (equal said '("five")))
      ;; and the line that does carry it is in the log
      (should (string-match-p "\\[5\\] five" log)))))

(ert-deftest vm-misc-test-the-log-says-when-and-how-long ()
  "The first line is the clock time; each after it adds the real and CPU
seconds since the line before.  Which step of a slow operation the time went
to is the question the log answers."
  (let ((vm-verbosity 5)
        (vm-log-level 10))
    (let ((log (vm-misc-test--with-log
                 (cl-letf (((symbol-function 'message) #'ignore)
                           ((symbol-function 'sit-for) (lambda (&rest _) t)))
                   (vm-inform 5 "first")
                   (vm-inform 5 "second")))))
      (let ((lines (split-string log "\n" t)))
        (should (equal (length lines) 2))
        (should (string-match-p "\\`[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9]\\{3\\} \\[5\\] first\\'"
                                (nth 0 lines)))
        (should (string-match-p "\\`[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9]\\{3\\} \\+[0-9.]+s \\+[0-9.]+cpu \\[5\\] second\\'"
                                (nth 1 lines)))))))

(ert-deftest vm-misc-test-everything-shown-is-logged ()
  "What VM says goes in the log whether `vm-log-level' asks for it or not.
A message that was shown and not recorded is one nobody can go back to, and
going back to it is what the log is for."
  (let ((vm-verbosity 5)
        (vm-log-level nil))
    (let ((log (vm-misc-test--with-log
                 (vm-misc-test--messages-at
                  5 (lambda () (vm-inform 5 "something happened"))))))
      (should (string-match-p "\\[5\\] something happened" log)))))

(ert-deftest vm-misc-test-the-log-is-bounded ()
  "The log runs for as long as Emacs does, so it is trimmed from the front:
what a reader wants is the end."
  (let ((vm-verbosity 5)
        (vm-log-max-lines 10))
    (let ((log (vm-misc-test--with-log
                 (vm-misc-test--messages-at
                  5 (lambda ()
                      (dotimes (i 40) (vm-inform 5 "line %d" i)))))))
      (let ((lines (split-string log "\n" t)))
        (should (<= (length lines) 21))
        ;; the end is what is kept
        (should (string-match-p "line 39" (car (last lines))))
        (should-not (string-match-p "line 0\\'" log))))))

(ert-deftest vm-misc-test-the-log-keeps-what-verbosity-hides ()
  "`vm-log-level' records a message the minibuffer never sees, which is how
to keep the detail of a slow operation without the churn.  The two are
independent: nothing is shown, and the line is still there."
  (let ((vm-verbosity 5)
        (vm-log-level 10)
        (vm-verbal-time 0)
        (said nil))
    (let ((log (vm-misc-test--with-log
                 (cl-letf (((symbol-function 'message)
                            (lambda (&rest args) (push (apply #'format args) said))))
                   (should-not (vm-inform 9 "a detail worth keeping"))))))
      (should-not said)
      (should (string-match-p "\\[9\\] a detail worth keeping" log))
      (should (string-match-p "\\`[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." log)))))

(ert-deftest vm-misc-test-the-log-keeps-warnings-too ()
  "A warning is recorded on the same terms, so a run that ends in one has it
in the same place as what led up to it."
  (let ((vm-verbosity 5)
        (vm-log-level 10)
        (vm-current-warning nil))
    (let ((log (vm-misc-test--with-log
                (cl-letf (((symbol-function 'message) #'ignore)
                          ((symbol-function 'sit-for) (lambda (&rest _) t)))
                  (vm-warn 1 0 "something is wrong")))))
      (should (string-match-p "\\[1\\] something is wrong" log)))))

(ert-deftest vm-misc-test-an-unrecorded-message-does-not-move-the-interval ()
  "A message that is neither shown nor recorded costs nothing and is not
timed: the next interval is measured from the last message that was."
  (let ((vm-verbosity 5)
        (vm-log-level nil)
        (vm-last-message-time nil))
    (vm-misc-test--messages-at 5 (lambda () (vm-inform 9 "not shown")))
    (should-not vm-last-message-time)))

(ert-deftest vm-misc-test-showing-an-empty-log-says-so ()
  "`vm-show-log' before VM has said anything says that, rather than showing
an empty buffer that says nothing about why."
  (let ((vm-log-level nil)
        (vm-verbosity 5)
        (said nil))
    (vm-misc-test--with-log
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest args) (push (apply #'format args) said)))
                ((symbol-function 'display-buffer)
                 (lambda (&rest _) (error "there was nothing to show"))))
        (vm-show-log)))
    (should (equal (length said) 1))
    (should (string-match-p "not said anything" (car said)))))

(ert-deftest vm-misc-test-imagemagick-7-is-not-asked-for-convert ()
  "REGRESSION: VM does not ask ImageMagick 7 for its deprecated command.

Version 7 warns on every run of `magick convert':

    WARNING: The convert command is deprecated in IMv7, use \"magick\"
    instead of \"convert\" or \"magick convert\"

VM prepended `convert' to the arguments whenever the program was `magick', so
every image it displayed printed that.  `magick' takes the same arguments on
its own.  `identify' is a different matter: version 7 has it as a subcommand
and does not deprecate it."
  (let ((vm-imagemagick-program "/usr/bin/magick")
        (vm-imagemagick-convert-program nil)
        (vm-imagemagick-identify-program nil)
        (called nil))
    (cl-letf (((symbol-function 'vm-call-process)
               (lambda (program _infile _buffer args)
                 (setq called (cons program args))
                 0)))
      (vm-imagemagick-call-convert nil nil '("-resize" "50%"))
      (should (equal called '("/usr/bin/magick" "-resize" "50%")))
      (should-not (member "convert" called))
      ;; and the shell form, which the asynchronous path writes into a script
      (should (equal (vm-imagemagick-convert-shell-command) "/usr/bin/magick"))
      ;; identify still names itself
      (vm-imagemagick-call-identify nil nil '("file.png"))
      (should (equal called '("/usr/bin/magick" "identify" "file.png"))))))

(ert-deftest vm-misc-test-imagemagick-6-still-runs-convert ()
  "Version 6 has no `magick': the program is `convert' and takes the same
arguments, so nothing is prepended to those either."
  (let ((vm-imagemagick-program nil)
        (vm-imagemagick-convert-program "/usr/bin/convert")
        (vm-imagemagick-identify-program "/usr/bin/identify")
        (called nil))
    (cl-letf (((symbol-function 'vm-call-process)
               (lambda (program _infile _buffer args)
                 (setq called (cons program args))
                 0)))
      (vm-imagemagick-call-convert nil nil '("-resize" "50%"))
      (should (equal called '("/usr/bin/convert" "-resize" "50%")))
      (should (equal (vm-imagemagick-convert-shell-command) "/usr/bin/convert")))))

;;; vm-load-features: warning about what is not there

(defmacro vm-misc-test--loading (bindings &rest body)
  "Run BODY with what `message' was told bound to `said', newest last.
BINDINGS are let bindings on top of that."
  (declare (indent 1) (debug t))
  `(let ((said nil) ,@bindings)
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest args)
                  (setq said (append said (list (apply #'format format args)))))))
       ,@body)
     said))

(ert-deftest vm-misc-test-load-features-is-quiet-in-batch ()
  "A batch Emacs is not warned about a feature that is not there.
The warning is for a reader who asked for a feature and is not getting it, and
a batch Emacs has nobody to read it: eighteen lines of it came out of `make',
where four WARNINGs in the middle of a build read as a broken build."
  (let ((said (vm-misc-test--loading ((noninteractive t))
                (vm-load-features '(vm-test-no-such-feature)))))
    (should-not said)))

(ert-deftest vm-misc-test-load-features-warns-a-reader-who-is-there ()
  "Outside batch it still says so, which is who the warning is for."
  (let ((said (vm-misc-test--loading ((noninteractive nil))
                (vm-load-features '(vm-test-no-such-feature)))))
    (should (equal (length said) 2))
    (should (string-match-p "Could not load feature vm-test-no-such-feature"
                            (car said)))
    (should (string-match-p "may not work correctly" (cadr said)))))

(ert-deftest vm-misc-test-load-features-honours-silent ()
  "SILENT still silences it where there is a reader, which is what the call
sites pass while a file is being compiled."
  (let ((said (vm-misc-test--loading ((noninteractive nil))
                (vm-load-features '(vm-test-no-such-feature) t))))
    (should-not said)))

(ert-deftest vm-misc-test-load-features-answers-what-loaded ()
  "The return value is the features that did load, warning or no warning."
  (should-not (let ((noninteractive t))
                (vm-load-features '(vm-test-no-such-feature))))
  (should (equal (let ((noninteractive t))
                   (vm-load-features '(subr-x vm-test-no-such-feature)))
                 '(subr-x))))


;;; Filling a paragraph the converter indented (issue #540)

(ert-deftest vm-misc-test-a-paragraph-indented-past-the-column-is-left-alone ()
  "REGRESSION: filling leaves a paragraph whose prefix is wider than the column.
Issue #540.  `vm-forward-paragraph' reads a paragraph\\='s indentation as its
prefix, and quoted HTML converted to a page 100000 columns wide arrived
indented by some 900 columns.  Filling that to 70 could only put one word on
each line, which is what the reply held."
  (with-temp-buffer
    (insert "> " (make-string 906 ?\s)
            "Mark Diekhans commented on a discussion on the issue:\n")
    (let ((vm-paragraph-fill-column 70)
          (vm-word-wrap-paragraphs nil))
      (vm-fill-paragraphs-containing-long-lines 70 (point-min) (point-max)))
    (goto-char (point-min))
    (should (looking-at (concat "> +Mark Diekhans commented")))
    ;; one line still, rather than one word to a line
    (should (equal (count-lines (point-min) (point-max)) 1))))

(ert-deftest vm-misc-test-an-ordinary-quoted-paragraph-is-still-filled ()
  "The other side of the guard: a prefix that leaves room is filled as before."
  (with-temp-buffer
    (insert "> " (mapconcat (lambda (i) (format "word%d" i))
                            (number-sequence 1 40) " ")
            "\n")
    (let ((vm-paragraph-fill-column 70)
          (vm-word-wrap-paragraphs nil))
      (vm-fill-paragraphs-containing-long-lines 70 (point-min) (point-max)))
    (should (> (count-lines (point-min) (point-max)) 1))
    (goto-char (point-min))
    (while (not (eobp))
      (should (<= (- (line-end-position) (line-beginning-position)) 70))
      (should (looking-at "> "))
      (forward-line 1))))

(ert-deftest vm-misc-test-fill-prefix-leaves-room-p-answers-both-ways ()
  "`vm-fill-prefix-leaves-room-p' compares the prefix with the column."
  (should (let ((fill-prefix nil) (fill-column 70))
            (vm-fill-prefix-leaves-room-p)))
  (should (let ((fill-prefix "> ") (fill-column 70))
            (vm-fill-prefix-leaves-room-p)))
  (should-not (let ((fill-prefix (make-string 70 ?\s)) (fill-column 70))
                (vm-fill-prefix-leaves-room-p)))
  (should-not (let ((fill-prefix (concat "> " (make-string 906 ?\s)))
                    (fill-column 70))
                (vm-fill-prefix-leaves-room-p))))

;;; Compiled files older than their sources (emacs-vm/vm#791)

(defmacro vm-misc-test--with-a-lisp-dir (spec &rest body)
  "Run BODY with a directory of fake VM files on `load-path'.
SPEC is (DIR-VAR).  BODY makes the files it wants; `features' is bound, so
what it pretends to have loaded does not outlive the test."
  (declare (indent 1) (debug t))
  `(let* ((,(car spec) (file-name-as-directory (make-temp-file "vm-stale" t)))
          (load-path (cons ,(car spec) load-path))
          (features features))
     (unwind-protect (progn ,@body)
       (delete-directory ,(car spec) t))))

(defun vm-misc-test--fake-file (dir name elc-age)
  "Write DIR/NAME.el and its .elc, the .elc ELC-AGE seconds older.
Answers nothing; adds NAME to `features' so VM counts it as loaded."
  (let ((el (expand-file-name (concat name ".el") dir))
        (elc (expand-file-name (concat name ".elc") dir)))
    (write-region (format ";;; %s\n(provide '%s)\n" name name) nil el nil 'quiet)
    (write-region "" nil elc nil 'quiet)
    (let ((when (- (float-time (file-attribute-modification-time
                                (file-attributes el)))
                   elc-age)))
      (set-file-times elc (seconds-to-time when)))
    (push (intern name) features)))

(ert-deftest vm-misc-test-a-stale-compiled-file-is-noticed ()
  "A VM .elc older than its .el is reported by name.
Emacs loads the compiled file in preference to the newer source, and VM's
files inline one another's defsubsts, so a stale one runs code that no longer
matches the rest of VM.  That is how a vm-folder.elc compiled before #453
answered `(setting-constant nil)' from inside `vm-build-message-list'."
  (vm-misc-test--with-a-lisp-dir (dir)
    (vm-misc-test--fake-file dir "vm-pretend-stale" 60)
    (should (member "vm-pretend-stale" (vm-stale-compiled-files)))))

(ert-deftest vm-misc-test-a-current-compiled-file-is-not-reported ()
  "A .elc at least as new as its .el is not reported."
  (vm-misc-test--with-a-lisp-dir (dir)
    (vm-misc-test--fake-file dir "vm-pretend-fresh" -60)
    (should-not (member "vm-pretend-fresh" (vm-stale-compiled-files)))))

(ert-deftest vm-misc-test-a-file-with-no-elc-is-not-reported ()
  "A file running interpreted, with no .elc at all, is nothing to report."
  (vm-misc-test--with-a-lisp-dir (dir)
    (write-region ";;; x\n(provide 'vm-pretend-plain)\n" nil
                  (expand-file-name "vm-pretend-plain.el" dir) nil 'quiet)
    (push 'vm-pretend-plain features)
    (should-not (member "vm-pretend-plain" (vm-stale-compiled-files)))))

(ert-deftest vm-misc-test-the-stale-warning-names-the-files ()
  "The warning says which files and what to do about them."
  (vm-misc-test--with-a-lisp-dir (dir)
    (vm-misc-test--fake-file dir "vm-pretend-stale" 60)
    (let (said)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (setq said message))))
        (vm-warn-about-stale-compiled-files))
      (should said)
      (should (string-match-p "vm-pretend-stale" said))
      (should (string-match-p "byte-recompile-directory" said)))))

;;; The version a file was compiled against (emacs-vm/vm#791)

(ert-deftest vm-misc-test-a-clean-build-reports-no-mismatch ()
  "The VM running this suite was built in one piece, and says so.
The premise of every other test here: were this to fail, the tree under test
would be one nobody should trust the rest of the run on."
  (should (equal nil vm-version-mismatched-files))
  (should (equal nil (vm-stale-compiled-files))))

(ert-deftest vm-misc-test-the-version-stamp-names-a-build ()
  "`vm-version-stamp' answers the release and the commit, and is stable."
  (let ((stamp (vm-version-stamp)))
    (should (stringp stamp))
    (should (string-match-p "/" stamp))
    (should (equal stamp (vm-version-stamp)))))

(ert-deftest vm-misc-test-the-assertion-fires-on-another-version ()
  "`vm-assert-version' expanded under one version notes it under another.
Expanded here rather than compiled, which is the same thing: the macro bakes
in whatever `vm-version-stamp' said when it ran, and the form left behind
compares that with what says it later."
  (let ((vm-version-mismatched-files nil)
        (form (cl-letf (((symbol-function 'vm-version-stamp)
                         (lambda () "8.3.2/deadbeef")))
                (macroexpand '(vm-assert-version)))))
    ;; under the version it was expanded with, nothing to say
    (cl-letf (((symbol-function 'vm-version-stamp)
               (lambda () "8.3.2/deadbeef")))
      (eval form t))
    (should (equal nil vm-version-mismatched-files))
    ;; under another, it notes the file and what it was built against
    (cl-letf (((symbol-function 'vm-version-stamp)
               (lambda () "8.3.3/cafe")))
      (eval form t))
    (should (equal 1 (length vm-version-mismatched-files)))
    (should (equal "8.3.2/deadbeef" (cdar vm-version-mismatched-files)))))

(ert-deftest vm-misc-test-the-warning-reports-a-version-mismatch ()
  "The warning names the file, the version it was built against, and this one.
This is the half that works on an installed tree: `make install' copies the
.elc after the .el, so there the .elc is always the newer of the two and no
comparison of timestamps can say anything."
  (let ((vm-version-mismatched-files '(("vm-folder.el" . "8.3.2/deadbeef")))
        said)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _) (setq said message)))
              ((symbol-function 'vm-stale-compiled-files) (lambda () nil)))
      (vm-warn-about-stale-compiled-files))
    (should said)
    (should (string-match-p "vm-folder\\.el" said))
    (should (string-match-p "8\\.3\\.2/deadbeef" said))
    (should (string-match-p "byte-recompile-directory" said))))

(ert-deftest vm-misc-test-every-compiled-vm-file-carries-the-assertion ()
  "Every VM file that is byte-compiled says which VM built it.
A file without the form is one whose staleness nothing would notice, which
is how #791 went unexplained.  The four left out are named here with why."
  (let ((exempt '("vm-autoloads.el"      ; generated
                  "vm-cus-load.el"       ; generated, never compiled
                  "vm-version-conf.el"   ; generated, and what the stamp reads
                  "vm-macro.el"          ; defines the macro
                  "vm-build.el"))        ; run by the build, not by VM
        missing)
    (dolist (file (directory-files vm-test-lisp-dir t "\\`vm-.*\\.el\\'"))
      (let ((name (file-name-nondirectory file)))
        (unless (member name exempt)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (unless (re-search-forward "^(vm-assert-version)$" nil t)
              (push name missing))))))
    (should (equal nil (sort missing #'string<)))))

;;; Word-wrapping long lines without the longlines package (emacs-vm/vm#817)
;;
;; `vm-word-wrap-paragraphs' used longlines.el, obsolete since Emacs 24.4,
;; which warned as it loaded.  These cover the replacement, and the fill
;; column that the longlines path ignored.

(defconst vm-misc-test--wrap-sample
  (concat "A short line.\n"
          "This is one very long line that runs well past forty columns"
          " and needs to be wrapped somewhere.\n"
          "> quoted short\n"
          "> a quoted line that is also far too long to fit inside forty"
          " columns of screen\n"
          "\n"
          "Final short line.\n")
  "Short lines, long lines, and a quoted block, which is the case that matters.")

(defun vm-misc-test--wrapped (width column &optional word-wrap)
  "The sample filled with WIDTH and COLUMN, word-wrapped if WORD-WRAP."
  (with-temp-buffer
    (insert vm-misc-test--wrap-sample)
    (let ((vm-paragraph-fill-column column)
          (vm-word-wrap-paragraphs word-wrap)
          (vm-message-pointer nil))
      (vm-fill-paragraphs-containing-long-lines width (point-min) (point-max)))
    (buffer-string)))

(defun vm-misc-test--longest-line (text)
  "The length of the longest line in TEXT."
  (apply #'max 0 (mapcar #'length (split-string text "\n"))))

(ert-deftest vm-misc-test-word-wrapping-keeps-every-line-break ()
  "REGRESSION: word-wrapping does not join lines, where filling does.
That is the whole point of `vm-word-wrap-paragraphs'.  Filling joins the
lines of a paragraph before breaking them again, so a short line followed
by a long one becomes one paragraph, and two quoted lines become one.  VM's
own filling does keep the quote prefix on the lines it makes, `fill-region'
on its own being worse than that; what it cannot keep is where the breaks
were."
  (let ((wrapped (vm-misc-test--wrapped 40 40 t))
        (filled (vm-misc-test--wrapped 40 40 nil)))
    ;; word-wrapped: the short line is still a line, and the quoted short
    ;; line is still its own
    (should (string-prefix-p "A short line.\n" wrapped))
    (should (string-match-p "\n> quoted short\n" wrapped))
    ;; every quote marker at the start of a line, in both
    (should-not (string-match-p "[^\n]>" wrapped))
    (should-not (string-match-p "[^\n]>" filled))
    ;; filled: both of those breaks are gone
    (should-not (string-prefix-p "A short line.\n" filled))
    (should-not (string-match-p "\n> quoted short\n" filled))))

(ert-deftest vm-misc-test-word-wrapping-honours-the-fill-column ()
  "REGRESSION: the column wrapped to is `vm-paragraph-fill-column'.
The longlines path read `vm-fill-paragraphs-containing-long-lines' instead,
which is the threshold and not the column, and ignored the width its caller
passed.  So anyone setting `vm-fill-long-lines-in-reply-column' with
`vm-word-wrap-paragraphs-in-reply' on had it silently ignored: measured, a
column of 30 produced lines of 59 (emacs-vm/vm#817)."
  (should (<= (vm-misc-test--longest-line (vm-misc-test--wrapped 40 30 t)) 30))
  (should (<= (vm-misc-test--longest-line (vm-misc-test--wrapped 40 50 t)) 50))
  ;; and the threshold is separate from the column: nothing here is over 200
  ;; columns, so nothing is touched however narrow the column
  (should (equal vm-misc-test--wrap-sample
                 (vm-misc-test--wrapped 200 30 t))))

(ert-deftest vm-misc-test-word-wrapping-does-not-load-longlines ()
  "REGRESSION: no obsolete package is required.
The deprecation warning on emacs-vm/vm#817 was `Package longlines is
deprecated', printed by the `require' inside the old implementation.  It
was a run-time require, so no lint saw it, and no test ran that path."
  (should-not (featurep 'longlines))
  (vm-misc-test--wrapped 40 40 t)
  (should-not (featurep 'longlines)))

(ert-deftest vm-misc-test-word-wrapping-leaves-a-long-word-whole ()
  "A word longer than the column is not broken: a URL has to survive."
  (let ((url "https://example.com/a/very/long/path/that/has/no/spaces/in/it/at/all"))
    (with-temp-buffer
      (insert "See " url " for more.\n")
      (let ((vm-paragraph-fill-column 40)
            (vm-word-wrap-paragraphs t)
            (vm-message-pointer nil))
        (vm-fill-paragraphs-containing-long-lines 40 (point-min) (point-max)))
      (should (string-match-p (regexp-quote url) (buffer-string)))
      ;; on a line of its own, unbroken
      (should (member url (split-string (buffer-string) "\n"))))))

(ert-deftest vm-misc-test-word-wrapping-leaves-short-lines-alone ()
  "A line at or under the threshold is untouched, and so is an empty region."
  (with-temp-buffer
    (insert (make-string 40 ?x) "\n")
    (let ((vm-paragraph-fill-column 40)
          (vm-word-wrap-paragraphs t)
          (vm-message-pointer nil))
      (vm-fill-paragraphs-containing-long-lines 40 (point-min) (point-max)))
    (should (equal (concat (make-string 40 ?x) "\n") (buffer-string))))
  (with-temp-buffer
    (let ((vm-paragraph-fill-column 40)
          (vm-word-wrap-paragraphs t)
          (vm-message-pointer nil))
      (vm-fill-paragraphs-containing-long-lines 40 (point-min) (point-max)))
    (should (equal "" (buffer-string)))))

(ert-deftest vm-misc-test-word-wrapping-leaves-no-trailing-whitespace ()
  "Nothing is left at the end of a wrapped line.
longlines put a space there to mark its own soft breaks, which VM never
unwrapped.  Under RFC 3676 a trailing space is a soft line break, so
leaving them would make VM's wrapping mean something to a recipient
reading format=flowed."
  (let ((wrapped (vm-misc-test--wrapped 40 40 t)))
    (should (> (length (split-string wrapped "\n")) 6)) ; it did wrap
    (dolist (line (split-string wrapped "\n"))
      (should-not (string-match-p "[ \t]\\'" line)))))

;;; Parsing a date out of a header (emacs-vm/vm#831)

(ert-deftest vm-misc-test-parse-date-reads-a-conforming-date ()
  "`vm-parse-date' answers the six fields of an RFC 822 date."
  (should (equal (vm-parse-date "Mon, 5 Jan 2026 12:00:00 -0800")
                 ["Mon" "5" "Jan" "2026" "12:00:00" "-0800"]))
  (should (equal (vm-parse-date "Tue, 6 Feb 2026 01:02:03 +0530")
                 ["Tue" "6" "Feb" "2026" "01:02:03" "+0530"])))

(ert-deftest vm-misc-test-parse-date-takes-no-comma-for-a-timezone-sign ()
  "REGRESSION: only `+' and `-' introduce a numeric timezone.

The class was written `[+---]', three hyphens to get a literal one, which
Emacs reads as `+' followed by the range `+' to `-'.  That range holds the
comma, so a date whose year followed a comma was read as a timezone:
`January 5,2026 12:00:00' gave a timezone of \",2026\" (emacs-vm/vm#831)."
  (let ((parsed (vm-parse-date "January 5,2026 12:00:00")))
    (should-not (string-match-p "," (aref parsed 5))))
  ;; and a comma where a sign belongs is not a timezone either
  (let ((parsed (vm-parse-date "Mon, 5 Jan 2026 12:00:00 ,0100")))
    (should-not (string-match-p "," (aref parsed 5)))))

(provide 'vm-misc-test)

;;; vm-misc-test.el ends here
