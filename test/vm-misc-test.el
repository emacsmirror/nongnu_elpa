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

(provide 'vm-misc-test)

;;; vm-misc-test.el ends here
