;;; devbook-mode-tests.el --- tests for devbook-mode.el -*-lexical-binding:t-*-

;; Copyright 2024-2026 Gentoo Authors

;; Author: Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'devbook-mode)

(defmacro devbook-mode-test-run-silently (&rest body)
  `(let ((inhibit-message t)) ,@body))

(ert-deftest devbook-mode-test-set-schema ()
  (cl-letf* ((rncfile "/home/larry/devmanual/devbook.rnc")
	     (rncschema "start = element foo { empty }\n")
	     ((symbol-function 'file-exists-p)
	      (lambda (file) (string-equal file rncfile)))
	     ((symbol-function 'file-directory-p) #'stringp)
	     ((symbol-function 'insert-file-contents)
	      (lambda (file &rest _args)
		(unless (string-equal file rncfile)
		  (signal 'file-missing nil))
		(insert rncschema))))
    (let ((buffer-file-name "/home/larry/devmanual/quickstart/text.xml"))
      (should (equal (devbook-set-schema) rncfile))
      (setq rncschema "foo = element foo { empty }\n") ; bad schema
      (should (equal
	       (car (should-error (devbook-set-schema)))
	       'rng-c-incorrect-schema)))
    (let ((buffer-file-name "/home/larry/elsewhere/text.xml"))
      (should-not (devbook-set-schema)))))

(ert-deftest devbook-mode-test-indent ()
  (let* ((output
	  (concat "<dl>\n"
		  "  <dt>Ingredients:</dt>\n"
		  "  <dd>\n"
		  "    <ul>\n"
		  "      <li>60 g ground coffee</li>\n"
		  "      <li>1 l water</li>\n"
		  "    </ul>\n"
		  "  </dd>\n"
		  "  <dt>Procedure:</dt>\n"
		  "  <dd>\n"
		  "    <ol>\n"
		  "      <li>Boil the water</li>\n"
		  "      <li>\n"
		  "        Pour the water over the coffee grounds\n"
		  "      </li>\n"
		  "      <li>Let the coffee drip through</li>\n"
		  "    </ol>\n"
		  "  </dd>\n"
		  "</dl>\n\n"
		  "<p>\n"
		  "Enjoy!\n"
		  "</p>\n"))
	 (input (replace-regexp-in-string "^ *" " " output)))
    (with-temp-buffer
      (devbook-mode-test-run-silently
       (devbook-mode)
       (insert input)
       (indent-region (point-min) (point-max)))
      (should (string-equal (buffer-string)
			    output)))))

(ert-deftest devbook-mode-test-fill-nobreak-p ()
  (with-temp-buffer
    (insert "<th align=\"center\" colspan=\"4\">four columns</th>\n")
    (goto-char (point-min))
    (search-forward "<th")
    (should (devbook-fill-tag-nobreak-p))
    (search-forward "center\"")
    (should-not (devbook-fill-tag-nobreak-p))
    (search-forward "four")
    (should-not (devbook-fill-tag-nobreak-p))))

(ert-deftest devbook-mode-test-extend-region ()
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (with-temp-buffer
    (let (b1 b2 i1 i2 a1 a2)
      (setq b1 (point)) (insert "<p>text</p>\n")
      (setq b2 (point)) (insert "<codesample lang=\"ebuild\">\n")
      (setq i1 (point)) (insert "# comment\n")
      (setq i2 (point)) (insert "</codesample>")
      (setq a1 (point)) (insert "\n<p>text</p>\n")
      (setq a2 (point))
      (let ((testcases
	     (list (list b1 b2  nil b1 b2) ; both before
		   (list b1 i1  t   b1 a1) ; start before, end inside
		   (list i1 i2  t   b2 a1) ; both inside
		   (list i2 a2  t   b2 a2) ; start inside, end after
		   (list a1 a2  nil a1 a2) ; both after
		   (list b2 a1  nil b2 a1) ; start before, end after
		   )))
	(dolist (test testcases)
	  (let ((font-lock-beg (car test))
		(font-lock-end (nth 1 test)))
	    (should (equal (list (not (not (devbook-font-lock-extend-region)))
				 font-lock-beg font-lock-end)
			   (nthcdr 2 test)))))))))

(ert-deftest devbook-mode-test-font-lock ()
  (with-temp-buffer
    (insert "<codesample lang=\"ebuild\">\n"
	    "echo hello &gt;README\n"
	    "dodoc README\n"
	    ;; test syntax-table shenanigans with unpaired apostrophe
	    "einfo \"This dodoc isn't a builtin\"\n"
	    "</codesample>\n")
    (devbook-mode-test-run-silently
     (devbook-mode))
    ;; run twice to test re-use of the native-mode fontification buffer
    (dotimes (_ 2)
      (let ((devbook-fontify-codesamples-natively t))
	(font-lock-ensure))
      (goto-char (point-min))
      (search-forward "codesample")
      (should (equal (get-text-property (match-beginning 0) 'face)
		     '(nxml-element-local-name)))
      (search-forward "gt")
      (should (equal (get-text-property (match-beginning 0) 'face)
		     '(nxml-entity-ref-name)))
      (search-forward "dodoc")
      (should (equal (get-text-property (match-beginning 0) 'face)
		     '(nxml-text font-lock-builtin-face)))
      (search-forward "dodoc")
      (should (equal (get-text-property (match-beginning 0) 'face)
		     '(nxml-text font-lock-string-face)))
      (search-forward "</")
      (should (equal (get-text-property (match-beginning 0) 'face)
		     '(nxml-tag-delimiter))))
    (let ((devbook-fontify-codesamples-natively nil))
      (font-lock-ensure))
    (goto-char (point-min))
    (search-forward "dodoc")
    (should (equal (get-text-property (match-beginning 0) 'face)
		   '(nxml-text)))))

(ert-deftest devbook-mode-test-skeleton ()
  (with-temp-buffer
    (cl-letf* ((buffer-file-name "/home/larry/devmanual/quickstart/text.xml")
	       (testinput '("Quickstart guide"))
	       (getinput (lambda (&rest _args)
			   (concat (pop testinput))))
	       ((symbol-function 'read-from-minibuffer) getinput)
	       ((symbol-function 'read-string) getinput))
      (devbook-insert-skeleton))
    (let ((buf1 (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			"<devbook self=\"quickstart/\">\n"
			"<chapter>\n"
			"<title>Quickstart guide</title>\n"))
	  (buf2 (concat "\n"
			"</chapter>\n"
			"</devbook>\n")))
      (should (equal (point)
		     (+ (point-min) (length buf1))))
      (should (string-equal (buffer-string)
			    (concat buf1 buf2))))))

(ert-deftest devbook-mode-test-keybindings ()
  (should (equal (lookup-key devbook-mode-map "\C-c\C-e\C-n")
		 'devbook-insert-skeleton))
  (with-temp-buffer
    (devbook-mode-test-run-silently
     (devbook-mode))
    (should (equal (local-key-binding "\C-c\C-e\C-n")
		   'devbook-insert-skeleton))))

(provide 'devbook-mode-tests)

;; Local Variables:
;; coding: utf-8
;; End:

;;; devbook-mode-tests.el ends here
