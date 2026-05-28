;;; adoc-mode-image-test.el --- tests for adoc-mode image support -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for image font-lock highlighting in adoc-mode.

;;; Code:

(require 'ert)
(require 'adoc-mode)
(require 'adoc-mode-test)

(ert-deftest adoctest-test-images ()
  (adoctest-faces "images"
                  ;; block macros
                  ;; empty arglist
                  "image" 'adoc-complex-replacement-face "::" 'adoc-meta-face
                  "./foo/bar.png" 'adoc-internal-reference-face
                  "[]" 'adoc-meta-face "\n" nil
                  ;; pos attribute 0 = alternate text
                  "image" 'adoc-complex-replacement-face "::" 'adoc-meta-face
                  "./foo/bar.png" 'adoc-internal-reference-face
                  "[" 'adoc-meta-face "lorem ipsum" 'adoc-secondary-text-face "]" 'adoc-meta-face "\n" nil
                  ;; keyword title
                  "image" 'adoc-complex-replacement-face "::" 'adoc-meta-face
                  "./foo/bar.png" 'adoc-internal-reference-face
                  "[" 'adoc-meta-face "alt" 'adoc-attribute-face "=" 'adoc-meta-face "lorem ipsum" 'adoc-secondary-text-face "]" 'adoc-meta-face "\n" nil
                  ;; keyword alt and title
                  "image" 'adoc-complex-replacement-face "::" 'adoc-meta-face
                  "./foo/bar.png" 'adoc-internal-reference-face
                  "[" 'adoc-meta-face "alt" 'adoc-attribute-face "=" 'adoc-meta-face "lorem ipsum" 'adoc-secondary-text-face "," 'adoc-meta-face
                  "title" 'adoc-attribute-face "=" 'adoc-meta-face "lorem ipsum" 'adoc-secondary-text-face "]" 'adoc-meta-face "\n" nil
                  ;; multiline alt and title
                  "image" 'adoc-complex-replacement-face "::" 'adoc-meta-face
                  "./foo/bar.png" 'adoc-internal-reference-face
                  "[" 'adoc-meta-face "alt" 'adoc-attribute-face "=" 'adoc-meta-face
                  "lorem\nipsum\nsit" 'adoc-secondary-text-face "," 'adoc-meta-face
                  "title" 'adoc-attribute-face "=" 'adoc-meta-face
                  "lorem\nipsum\nsit" 'adoc-secondary-text-face "]" 'adoc-meta-face "\n" nil

                  ;; no everything again with inline macros
                  "foo " 'no-face "image" 'adoc-complex-replacement-face ":" 'adoc-meta-face
                  "./foo/bar.png" 'adoc-internal-reference-face
                  "[]" 'adoc-meta-face "bar" 'no-face "\n" nil

                  "foo " 'no-face "image" 'adoc-complex-replacement-face ":" 'adoc-meta-face
                  "./foo/bar.png" 'adoc-internal-reference-face
                  "[" 'adoc-meta-face "lorem ipsum" 'adoc-secondary-text-face "]" 'adoc-meta-face "bar" 'no-face "\n" nil

                  "foo " 'no-face "image" 'adoc-complex-replacement-face ":" 'adoc-meta-face
                  "./foo/bar.png" 'adoc-internal-reference-face
                  "[" 'adoc-meta-face "alt" 'adoc-attribute-face "=" 'adoc-meta-face "lorem ipsum" 'adoc-secondary-text-face "]" 'adoc-meta-face "bar" 'no-face "\n" nil

                  "foo " 'no-face "image" 'adoc-complex-replacement-face ":" 'adoc-meta-face
                  "./foo/bar.png" 'adoc-internal-reference-face
                  "[" 'adoc-meta-face "alt" 'adoc-attribute-face "=" 'adoc-meta-face "lorem ipsum" 'adoc-secondary-text-face "," 'adoc-meta-face
                  "title" 'adoc-attribute-face "=" 'adoc-meta-face "lorem ipsum" 'adoc-secondary-text-face "]" 'adoc-meta-face "bar" 'no-face "\n" nil

                  "image" 'adoc-complex-replacement-face ":" 'adoc-meta-face
                  "./foo/bar.png" 'adoc-internal-reference-face
                  "[" 'adoc-meta-face "alt" 'adoc-attribute-face "=" 'adoc-meta-face
                  "lorem\nipsum\nsit" 'adoc-secondary-text-face "," 'adoc-meta-face
                  "title" 'adoc-attribute-face "=" 'adoc-meta-face
                  "lorem\nipsum\nsit" 'adoc-secondary-text-face "]" 'adoc-meta-face "\n" nil))

(ert-deftest adoctest-test-resolve-attribute-references ()
  (with-temp-buffer
    (adoc-mode)
    (insert ":my-url: https://example.com/image.png\n"
            ":badge: http://melpa.org/badge.svg\n"
            "\n"
            "image:{my-url}[]\n"
            "image:{badge}[alt]\n"
            "image:{undefined}[]\n"
            "image:plain.png[]\n")
    ;; Single reference
    (should (equal (adoc--resolve-attribute-references "{my-url}")
                   "https://example.com/image.png"))
    ;; Another reference
    (should (equal (adoc--resolve-attribute-references "{badge}")
                   "http://melpa.org/badge.svg"))
    ;; Undefined reference is left unchanged
    (should (equal (adoc--resolve-attribute-references "{undefined}")
                   "{undefined}"))
    ;; No references - returned as-is
    (should (equal (adoc--resolve-attribute-references "plain.png")
                   "plain.png"))
    ;; Empty string
    (should (equal (adoc--resolve-attribute-references "")
                   ""))))

;;; adoc-mode-image-test.el ends here
