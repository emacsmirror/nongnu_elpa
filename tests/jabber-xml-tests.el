;;; jabber-xml-tests.el --- Tests for jabber-xml  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-xml)

;;; Group 1: jabber-escape-xml / jabber-unescape-xml

(ert-deftest jabber-test-escape-xml-special-chars ()
  "Escape ampersand, angle brackets, quotes and apostrophes."
  (should (string= (jabber-escape-xml "&<>\"'")
                   "&amp;&lt;&gt;&quot;&apos;")))

(ert-deftest jabber-test-escape-xml-plain-string ()
  "Plain strings pass through unchanged."
  (should (string= (jabber-escape-xml "hello world") "hello world")))

(ert-deftest jabber-test-escape-xml-nil ()
  "Non-string input is returned as-is."
  (should (eq (jabber-escape-xml nil) nil)))

(ert-deftest jabber-test-escape-xml-control-chars ()
  "Control characters are replaced with spaces."
  (should (string= (jabber-escape-xml "\x01\x02") "  ")))

(ert-deftest jabber-test-escape-xml-form-feed ()
  "Form feed is replaced with newline."
  (should (string= (jabber-escape-xml "a\fb") "a\nb")))

(ert-deftest jabber-test-unescape-xml-entities ()
  "Unescape XML entities back to characters."
  (should (string= (jabber-unescape-xml "&amp;&lt;&gt;&quot;&apos;")
                   "&<>\"'")))

(ert-deftest jabber-test-unescape-xml-nil ()
  "Non-string input is returned as-is."
  (should (eq (jabber-unescape-xml nil) nil)))

(ert-deftest jabber-test-escape-unescape-roundtrip ()
  "Escaping then unescaping produces the original string."
  (let ((input "Tom & Jerry <friends> say \"hi\" & 'bye'"))
    (should (string= (jabber-unescape-xml (jabber-escape-xml input))
                     input))))

;;; Group 2: jabber-sexp2xml

(ert-deftest jabber-test-sexp2xml-self-closing ()
  "Self-closing tag with no children."
  (should (string= (jabber-sexp2xml '(br nil))
                   "<br/>")))

(ert-deftest jabber-test-sexp2xml-with-attributes ()
  "Tag with attributes and no children."
  (should (string= (jabber-sexp2xml '(stream ((to . "example.com") (xmlns . "jabber:client"))))
                   "<stream to='example.com' xmlns='jabber:client'/>")))

(ert-deftest jabber-test-sexp2xml-with-children ()
  "Tag with text children."
  (should (string= (jabber-sexp2xml '(body nil "Hello"))
                   "<body>Hello</body>")))

(ert-deftest jabber-test-sexp2xml-nested ()
  "Nested tags."
  (should (string= (jabber-sexp2xml '(message ((to . "bob@example.com"))
                                      (body nil "Hi")))
                   "<message to='bob@example.com'><body>Hi</body></message>")))

(ert-deftest jabber-test-sexp2xml-string-input ()
  "String input is XML-escaped."
  (should (string= (jabber-sexp2xml "a<b") "a&lt;b")))

(ert-deftest jabber-test-sexp2xml-empty-string-children ()
  "Work around old xml.el bug where (\"\") appears as children."
  (should (string= (jabber-sexp2xml '("")) "")))

(ert-deftest jabber-test-sexp2xml-escapes-attribute-values ()
  "Attribute values are XML-escaped."
  (should (string= (jabber-sexp2xml '(a ((href . "a&b"))))
                   "<a href='a&amp;b'/>")))

;;; Group 3: node accessors

(ert-deftest jabber-test-xml-node-name ()
  "Extract tag name from node."
  (should (eq (jabber-xml-node-name '(message ((type . "chat")) "hi"))
              'message)))

(ert-deftest jabber-test-xml-node-name-nil ()
  "Non-list input returns nil."
  (should (eq (jabber-xml-node-name "string") nil)))

(ert-deftest jabber-test-xml-node-attributes ()
  "Extract attributes alist from node."
  (should (equal (jabber-xml-node-attributes '(iq ((type . "get") (id . "1"))))
                 '((type . "get") (id . "1")))))

(ert-deftest jabber-test-xml-node-children ()
  "Extract children from node."
  (should (equal (jabber-xml-node-children '(body nil "Hello"))
                 '("Hello"))))

(ert-deftest jabber-test-xml-node-children-empty-string-bug ()
  "Work around old xml.el bug where children are ((\"\"))."
  (should (null (jabber-xml-node-children '(tag nil (""))))))

;;; Group 4: jabber-xml-get-children

(ert-deftest jabber-test-xml-get-children-found ()
  "Find children by tag name."
  (let ((node '(iq nil (query nil "data") (error nil "oops"))))
    (should (equal (jabber-xml-get-children node 'query)
                   '((query nil "data"))))))

(ert-deftest jabber-test-xml-get-children-missing ()
  "Return nil when tag not found."
  (let ((node '(iq nil (query nil))))
    (should (null (jabber-xml-get-children node 'error)))))

(ert-deftest jabber-test-xml-get-children-multiple ()
  "Return all children with matching tag."
  (let ((node '(parent nil (item nil "a") (item nil "b"))))
    (should (= (length (jabber-xml-get-children node 'item)) 2))))

;;; Group 5: jabber-xml-get-attribute / jabber-xml-get-xmlns

(ert-deftest jabber-test-xml-get-attribute-present ()
  "Get attribute value when present."
  (should (string= (jabber-xml-get-attribute '(iq ((type . "result"))) 'type)
                   "result")))

(ert-deftest jabber-test-xml-get-attribute-missing ()
  "Return nil when attribute not found."
  (should (null (jabber-xml-get-attribute '(iq ((type . "result"))) 'id))))

(ert-deftest jabber-test-xml-get-attribute-nil-node ()
  "Return nil for non-cons node."
  (should (null (jabber-xml-get-attribute nil 'type))))

(ert-deftest jabber-test-xml-get-xmlns-present ()
  "Get xmlns attribute."
  (should (string= (jabber-xml-get-xmlns '(query ((xmlns . "jabber:iq:roster"))))
                   "jabber:iq:roster")))

(ert-deftest jabber-test-xml-get-xmlns-absent ()
  "Return nil when xmlns not present."
  (should (null (jabber-xml-get-xmlns '(query nil)))))

;;; Group 6: jabber-xml-child-with-xmlns

(ert-deftest jabber-test-xml-child-with-xmlns-found ()
  "Find child element by xmlns."
  (let ((node '(message nil
                 (x ((xmlns . "jabber:x:oob")) (url () "http://example.com"))
                 (body () "text"))))
    (should (equal (jabber-xml-node-name
                    (jabber-xml-child-with-xmlns node "jabber:x:oob"))
                   'x))))

(ert-deftest jabber-test-xml-child-with-xmlns-missing ()
  "Return nil when no child has the given xmlns."
  (let ((node '(message nil (body () "text"))))
    (should-not (jabber-xml-child-with-xmlns node "jabber:x:oob"))))

(ert-deftest jabber-test-xml-child-with-xmlns-nil-node ()
  "Return nil for nil node."
  (should-not (jabber-xml-child-with-xmlns nil "jabber:x:oob")))

(ert-deftest jabber-test-xml-child-with-xmlns-skips-strings ()
  "String children are skipped without error."
  (let ((node '(body nil "just text")))
    (should-not (jabber-xml-child-with-xmlns node "some:ns"))))

;;; Group 7: jabber-xml-path


(ert-deftest jabber-test-xml-path-symbol ()
  "Navigate to child by symbol name."
  (let ((xml '(iq nil (query nil "data"))))
    (should (equal (jabber-xml-path xml '(query))
                   '(query nil "data")))))

(ert-deftest jabber-test-xml-path-string ()
  "String step extracts character data."
  (let ((xml '(body nil "Hello")))
    (should (string= (jabber-xml-path xml '(""))
                     "Hello"))))

(ert-deftest jabber-test-xml-path-multi-step ()
  "Multi-step path navigation."
  (let ((xml '(iq nil (query nil (item ((jid . "bob@example.com")))))))
    (should (equal (jabber-xml-node-name
                    (jabber-xml-path xml '(query item)))
                   'item))))

(ert-deftest jabber-test-xml-path-cons-namespace ()
  "Navigate using cons (namespace . name) step."
  (let ((xml '(message nil
                (delay ((xmlns . "urn:xmpp:delay") (stamp . "2024-01-01T00:00:00Z"))))))
    (should (equal (jabber-xml-node-name
                    (jabber-xml-path xml '(("urn:xmpp:delay" . "delay"))))
                   'delay))))

(ert-deftest jabber-test-xml-path-missing ()
  "Return nil for non-existent path."
  (let ((xml '(iq nil (query nil))))
    (should (null (jabber-xml-path xml '(error))))))

;;; Group 8: jabber-xml-skip-tag-forward

(ert-deftest jabber-test-skip-tag-forward-self-closing ()
  "Skip past a self-closing tag."
  (with-temp-buffer
    (insert "<br/>")
    (goto-char (point-min))
    (should (eq (catch 'unfinished
                  (jabber-xml-skip-tag-forward))
                t))
    (should (= (point) (point-max)))))

(ert-deftest jabber-test-skip-tag-forward-paired ()
  "Skip past a paired open/close tag."
  (with-temp-buffer
    (insert "<body>Hello</body>")
    (goto-char (point-min))
    (should (eq (catch 'unfinished
                  (jabber-xml-skip-tag-forward))
                t))
    (should (= (point) (point-max)))))

(ert-deftest jabber-test-skip-tag-forward-nested ()
  "Skip past nested tags."
  (with-temp-buffer
    (insert "<message><body>Hi</body></message>")
    (goto-char (point-min))
    (should (eq (catch 'unfinished
                  (jabber-xml-skip-tag-forward))
                t))
    (should (= (point) (point-max)))))

(ert-deftest jabber-test-skip-tag-forward-cdata ()
  "Skip past CDATA section."
  (with-temp-buffer
    (insert "<![CDATA[some <data>]]>")
    (goto-char (point-min))
    (should (catch 'unfinished
              (jabber-xml-skip-tag-forward)))
    (should (= (point) (point-max)))))

(ert-deftest jabber-test-skip-tag-forward-incomplete ()
  "Throw unfinished for incomplete tag."
  (with-temp-buffer
    (insert "<message><body>Hi</body>")
    (goto-char (point-min))
    (should-not (catch 'unfinished
                  (jabber-xml-skip-tag-forward)))))

;;; Group 9: jabber-xml-parse-next-stanza

(ert-deftest jabber-test-parse-next-stanza-complete ()
  "Parse a complete XML stanza."
  (with-temp-buffer
    (insert "<message to='bob@example.com'><body>Hi</body></message>")
    (let ((result (jabber-xml-parse-next-stanza)))
      (should result)
      (should (eq (jabber-xml-node-name (car result)) 'message)))))

(ert-deftest jabber-test-parse-next-stanza-incomplete ()
  "Return nil for incomplete stanza."
  (with-temp-buffer
    (insert "<message><body>Hi</body>")
    (should (null (jabber-xml-parse-next-stanza)))))

(ert-deftest jabber-test-parse-next-stanza-empty ()
  "Return nil for empty buffer."
  (with-temp-buffer
    (should (null (jabber-xml-parse-next-stanza)))))

(provide 'jabber-xml-tests)
;;; jabber-xml-tests.el ends here
