;;; coq-test-coqproject-args.el --- Test _RocqProject parsing -*- lexical-binding: t; -*-
;;
;; This file is part of Proof General.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'coq-system)
(require 'ert)

(defconst coq-test-project-file-args
  '(("-arg -w\n-arg -foo"                            ("-w" "-foo"))
    ("-arg -w -arg -foo -arg -bar"                   ("-w" "-foo" "-bar"))
    ("-arg\t-w\n-arg\t-foo"                          ("-w" "-foo"))
    ("-arg\r-w\r-arg\r-foo"                          ("-w" "-foo"))
    ("-arg \"-w all\""                               ("-w" "all"))
    ("-arg \"-w -notation-overriden\""               ("-w" "-notation-overriden"))
    ("-arg \"-w -a,-b\""                             ("-w" "-a,-b"))
    ("-arg \"-w -a\"\n-arg -w -arg -b"               ("-w" "-a" "-w" "-b"))
    ("-arg \"-w '-a -b'\""                           ("-w" "-a -b"))
    ("-arg \"-set 'Default Goal Selector=!'\""       ("-set" "Default Goal Selector=!"))
    ("-arg -set\n-arg \"'Default Goal Selector=!'\"" ("-set" "Default Goal Selector=!"))
    ("-arg \"'a b'\""                                ("a b"))
    ("-arg \"'a b' c\""                              ("a b" "c"))
    ("-arg \"c 'a b'\""                              ("c" "a b"))
    ("-arg \"'a b' 'c d'\""                          ("a b" "c d"))
    ("-arg \"'a' 'b'\""                              ("a" "b"))
    ("-arg \"a'b c'd\""                              ("ab cd"))
    ("-arg \"-Q '' Foo\""                            ("-Q" "" "Foo"))
    ("-arg \"''\""                                   (""))
    ("-arg \"\""                                     nil)
    ("-arg \"   \""                                  nil)
    ("-arg \"-w\tall\""                              ("-w\tall"))
    ("-arg \"a\\\\b\"\n-arg -w"                      ("a\\\\b" "-w"))
    ("# why\n-arg -w"                                ("-w"))
    ("-arg \"-w all\" # why"                         ("-w" "all"))
    ("-arg -w#why\n-arg -foo"                        ("-w" "-foo"))
    ("# an unbalanced \" is just text\n-arg -w"      ("-w"))
    ("# a carriage return does not end a comment\r-arg -w" nil)
    ("-arg \"-w #foo\""                              ("-w" "#foo"))))

(ert-deftest coq-project-file-prog-args ()
  (dolist (case coq-test-project-file-args)
    (pcase-let ((`(,contents ,expected) case))
      (should (equal (cons contents (coq--extract-prog-args
                                     (coq--read-options-from-project-file contents)))
                     (cons contents expected))))))

(ert-deftest coq-project-file-unpaired-quote ()
  (dolist (contents '("-arg \"'\""
                      "-arg \"a 'b c' d 'e\""
                      "-arg \"-w 'all\""
                      "-arg 'val -arg val'"))
    (should-error (coq--extract-prog-args
                   (coq--read-options-from-project-file contents)))))

(ert-deftest coq-project-file-unterminated-string ()
  (should-error (coq--read-options-from-project-file "-arg \"-w all\n-arg -foo")))

(ert-deftest coq-project-file-load-path ()
  (should (equal (coq--extract-load-path
                  (coq--read-options-from-project-file
                   "# not a -R\n-R theories Top\n-Q \"my dir\" \"\"\n-I src")
                  "/p/")
                 '((rec "/p/theories" "Top")
                   (recnoimport "/p/my dir" "")
                   (ocamlimport "/p/src")))))

(provide 'coq-test-coqproject-args)

;;; coq-test-coqproject-args.el ends here
