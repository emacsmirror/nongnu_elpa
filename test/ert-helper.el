(load-file "lisp/mastodon-http.el")
(load-file "lisp/mastodon-iso.el")
(load-file "lisp/mastodon-tl.el")
(load-file "lisp/mastodon-toot.el")
(load-file "lisp/mastodon-search.el")
(load-file "lisp/mastodon.el")
(load-file "lisp/mastodon-search.el")
(load-file "lisp/mastodon-client.el")
(load-file "lisp/mastodon-auth.el")
(load-file "lisp/mastodon-discover.el")
(load-file "lisp/mastodon-inspect.el")
(load-file "lisp/mastodon-media.el")
(load-file "lisp/mastodon-notifications.el")
(load-file "lisp/mastodon-profile.el")
(load-file "lisp/mastodon-async.el")

;; load tests in bulk to avoid using deprecated `cask exec'
(let* ((all-test-files
        (directory-files "test/." t directory-files-no-dot-files-regexp))
       (tests
        (cl-remove-if-not
         (lambda (x)
           (string-suffix-p "-tests.el" x))
         all-test-files)))
  (mapc #'load-file tests))


