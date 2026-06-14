;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../lisp/jabber-avatar.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;;; Avatar display bounds

(ert-deftest jabber-test-avatar-image-uses-configured-bounds ()
  (let ((avatar (make-jabber-avatar :base64-data (base64-encode-string "data" t)
                             :mime-type "image/png"))
        (jabber-avatar-max-width 32)
        (jabber-avatar-max-height 48))
    (cl-letf (((symbol-function 'jabber-image-create)
               (lambda (data mime-type max-width max-height)
                 (list data mime-type max-width max-height))))
      (should (equal (jabber-avatar-image avatar)
                     '("data" "image/png" 32 48))))))

(ert-deftest jabber-test-avatar-set-cached-uses-configured-bounds ()
  (let ((jabber-avatar-cache-directory temporary-file-directory)
        (jabber-avatar-max-width 40)
        (jabber-avatar-max-height 44)
        (calls nil))
    (cl-letf (((symbol-function 'jabber-avatar-find-cached)
               (lambda (_hash) "/tmp/avatar.png"))
              ((symbol-function 'jabber-image-create-from-file)
               (lambda (file max-width max-height)
                 (push (list file max-width max-height) calls)
                 'image)))
      (jabber-avatar-set "romeo@example.net" "hash")
      (should (equal calls '(("/tmp/avatar.png" 40 44))))
      (should (eq (get (jabber-jid-symbol "romeo@example.net") 'avatar)
                  'image)))))

(provide 'jabber-test-avatar)
