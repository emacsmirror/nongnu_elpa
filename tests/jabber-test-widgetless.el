;;; jabber-test-widgetless.el --- Tests for text-first editors  -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests for compose, registration, search, and vCard paths.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'jabber-compose)
(require 'jabber-register)
(require 'jabber-vcard)
(require 'jabber-xdata)
(require 'jabber-xdata-form)

(defconst jabber-test-widgetless--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root used by source-boundary tests.")

(ert-deftest jabber-test-compose-send-uses-buffer-body ()
  (let (sent)
    (with-temp-buffer
      (jabber-compose-mode)
      (insert "first line\nsecond line")
      (setq-local jabber-buffer-connection 'connection
                  jabber-compose-recipients '("a@example.org" "b@example.org")
                  jabber-compose-subject "Subject")
      (cl-letf (((symbol-function 'jabber-send-message)
                 (lambda (jc to subject body _thread)
                   (push (list jc to subject body) sent))))
        (jabber-compose-send 'legacy-widget 'legacy-event)))
    (should
     (equal (nreverse sent)
            '((connection "a@example.org" "Subject" "first line\nsecond line")
              (connection "b@example.org" "Subject" "first line\nsecond line"))))))

(ert-deftest jabber-test-compose-noninteractive-prefill-does-not-prompt ()
  (let (compose-buffer)
    (cl-letf (((symbol-function 'jabber-compose--read-recipients)
               (lambda (&rest _ignore) (ert-fail "recipient reader called")))
              ((symbol-function 'read-string)
               (lambda (&rest _ignore) (ert-fail "subject reader called")))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _ignore)
                 (setq compose-buffer buffer))))
      (jabber-compose 'connection "romeo@example.org"))
    (unwind-protect
        (with-current-buffer compose-buffer
          (should (equal jabber-buffer-connection 'connection))
          (should (equal jabber-compose-recipients '("romeo@example.org")))
          (should (equal jabber-compose-subject "")))
      (when (buffer-live-p compose-buffer)
        (kill-buffer compose-buffer)))))

(ert-deftest jabber-test-compose-empty-acceptance-preserves-all-recipients ()
  "Empty recipient input accepts every prefilled recipient."
  (cl-letf (((symbol-function 'jabber-concat-rosters) #'ignore)
            ((symbol-function 'completing-read-multiple)
             (lambda (_prompt _collection &rest arguments)
               (let ((default (nth 4 arguments)))
                 (should (stringp default))
                 (split-string default "[ \t]*,[ \t]*" t)))))
    (should
     (equal (jabber-compose--read-recipients
             '("romeo@example.org" "juliet@example.org"))
            '("romeo@example.org" "juliet@example.org")))))

(ert-deftest jabber-test-register-legacy-new-account-requires-requested-fields ()
  (let* ((query '(query nil
                        (instructions nil "Fill this in")
                        (username nil)
                        (password nil)))
         (form (jabber-register--legacy-form query "romeo")))
    (with-temp-buffer
      (jabber-xdata-form-mode)
      (setq-local jabber-xdata-form--form form
                  jabber-register--legacy-p t
                  jabber-register--registered-p nil)
      (should-error (jabber-register--submission 'register)
                    :type 'user-error))))

(ert-deftest jabber-test-register-legacy-existing-account-preserves-empty-password ()
  (let ((jabber-register--legacy-p t)
        (jabber-register--registered-p t)
        (jabber-xdata-form--form
         '(:fields ((:var "username" :values ("romeo"))
                    (:var "password" :values (""))))))
    (should
     (equal (jabber-register--submission 'register)
            '((username nil "romeo") (password nil ""))))))

(ert-deftest jabber-test-register-xdata-account-without-username-field ()
  "Open and submit account forms that do not request a username."
  (let ((xml
         '(iq ((from . "register.example.org"))
              (query ((xmlns . "jabber:iq:register"))
                     (x ((xmlns . "jabber:x:data") (type . "form"))
                        (field ((var . "FORM_TYPE") (type . "hidden"))
                               (value nil "jabber:iq:register"))
                        (field ((var . "password") (type . "text-private"))
                               (value nil "password-value"))))))
        buffer)
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (_jc)
                 '(:registerp t :username "romeo" :server "example.org")))
              ((symbol-function 'jabber-xdata-form-open)
               (lambda (form _actions)
                 (setq buffer (generate-new-buffer " *jabber-register-test*"))
                 (with-current-buffer buffer
                   (jabber-xdata-form-mode)
                   (setq-local jabber-xdata-form--form form))
                 buffer)))
      (jabber-process-register-or-search 'connection xml))
    (unwind-protect
        (with-current-buffer buffer
          (should-not (jabber-xdata-field jabber-xdata-form--form "username"))
          (let ((submission (car (jabber-register--submission 'register))))
            (should-not
             (seq-find
              (lambda (field)
                (equal "username" (jabber-xml-get-attribute field 'var)))
              (jabber-xml-get-children submission 'field)))
            (let ((password
                   (seq-find
                    (lambda (field)
                      (equal "password"
                             (jabber-xml-get-attribute field 'var)))
                    (jabber-xml-get-children submission 'field))))
              (should (equal "password-value"
                             (jabber-xml-path password '(value "")))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest jabber-test-search-legacy-omits-unused-criteria ()
  (let ((jabber-register--legacy-p t)
        (jabber-xdata-form--form
         '(:fields ((:var "first" :values ("Romeo"))
                    (:var "last" :values (""))))))
    (should
     (equal (jabber-register--submission 'search)
            '((first nil "Romeo"))))))

(ert-deftest jabber-test-xdata-result-renderer-is-widgetless ()
  (with-temp-buffer
    (jabber-xdata-render-result
     '(x ((type . "result"))
         (reported nil
                   (field ((var . "jid") (label . "JID")
                           (type . "jid-single"))))
         (item nil (field ((var . "jid"))
                          (value nil "romeo@example.org")))))
    (should (string-match-p "JID" (buffer-string)))
    (should (string-match-p "romeo@example.org" (buffer-string)))
    (goto-char (point-min))
    (search-forward "romeo@example.org")
    (should (equal (get-text-property (1- (point)) 'jabber-jid)
                   "romeo@example.org"))))

(ert-deftest jabber-test-vcard-editor-uses-plain-data ()
  (with-temp-buffer
    (jabber-vcard-edit-mode)
    (setq-local jabber-vcard--edit-data '((FN . "Romeo")))
    (jabber-vcard--set 'NICKNAME "Montague")
    (jabber-vcard--render-editor)
    (should (equal (cdr (assq 'NICKNAME jabber-vcard--edit-data)) "Montague"))
    (should (string-match-p "NICKNAME: Montague" (buffer-string)))
    (should-not (bound-and-true-p widget-minor-mode))))

(ert-deftest jabber-test-vcard-repeatable-fields-round-trip ()
  "Repeatable phone, email, and address entries survive XML conversion."
  (let* ((jabber-vcard-photo nil)
         (parsed '((TEL ((HOME VOICE) . "111") ((WORK CELL) . "222"))
                   (EMAIL ((HOME INTERNET) . "home@example.org")
                          ((WORK INTERNET) . "work@example.org"))
                   (ADR ((HOME) (STREET . "Home Street") (CTRY . "GR"))
                        ((WORK) (STREET . "Work Street") (CTRY . "DE")))))
         (round-trip (jabber-vcard-parse (jabber-vcard-reassemble parsed))))
    (should (equal '("111" "222")
                   (sort (mapcar #'cdr (cdr (assq 'TEL round-trip))) #'string<)))
    (should (equal '("home@example.org" "work@example.org")
                   (sort (mapcar #'cdr (cdr (assq 'EMAIL round-trip))) #'string<)))
    (should
     (equal '("Home Street" "Work Street")
            (sort (mapcar (lambda (address)
                            (cdr (assq 'STREET (cdr address))))
                          (cdr (assq 'ADR round-trip)))
                  #'string<)))))

(ert-deftest jabber-test-vcard-delete-removes-one-identical-value ()
  "Deleting one indexed duplicate preserves the other through XML conversion."
  (let ((jabber-vcard-photo nil))
    (with-temp-buffer
      (jabber-vcard-edit-mode)
      (setq-local jabber-vcard--edit-data
                  '((TEL ((HOME VOICE) . "111")
                         ((HOME VOICE) . "111"))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _)
                   (car (cadr candidates))))
                ((symbol-function 'jabber-vcard--render-editor) #'ignore))
        (jabber-vcard-delete-phone))
      (let* ((round-trip
              (jabber-vcard-parse
               (jabber-vcard-reassemble jabber-vcard--edit-data)))
             (phones (cdr (assq 'TEL round-trip))))
        (should (= 1 (length phones)))
        (should (equal "111" (cdar phones)))
        (should (equal '(HOME VOICE)
                       (sort (copy-sequence (caar phones))
                             (lambda (a b)
                               (string< (symbol-name a) (symbol-name b))))))))))

(ert-deftest jabber-test-vcard-editor-map-preserves-command-groups ()
  "The vCard editor exposes basic, repeatable, avatar, and action groups."
  (let* ((rows (lookup-key jabber-vcard-edit-mode-map
                           [keymap-popup descriptions]))
         (groups (car rows)))
    (should (equal '("Basic" "Repeatable" "Avatar" "Actions")
                   (mapcar (lambda (group) (plist-get group :name)) groups)))
    (should (eq (lookup-key jabber-vcard-edit-mode-map "t")
                #'jabber-vcard-add-phone))
    (should (eq (lookup-key jabber-vcard-edit-mode-map "e")
                #'jabber-vcard-add-email))
    (should (eq (lookup-key jabber-vcard-edit-mode-map "a")
                #'jabber-vcard-add-address))
    (should (eq (lookup-key jabber-vcard-edit-mode-map "p")
                #'jabber-vcard-edit-avatar))))

(ert-deftest jabber-test-vcard-avatar-keep-remove-replace ()
  "Keeping, removing, and replacing an avatar produce matching XML."
  (let ((file (make-temp-file "jabber-vcard-avatar-" nil ".png")))
    (unwind-protect
        (progn
          (write-region "new avatar" nil file nil 'silent)
          (with-temp-buffer
            (jabber-vcard-edit-mode)
            (setq-local jabber-vcard--edit-data
                        '((PHOTO "image/png" "b2xkIGF2YXRhcg==")))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "Keep existing")))
              (jabber-vcard-edit-avatar))
            (let ((xml (jabber-vcard-reassemble jabber-vcard--edit-data)))
              (should (equal "b2xkIGF2YXRhcg=="
                             (jabber-xml-path xml '(PHOTO BINVAL ""))))))
          (with-temp-buffer
            (jabber-vcard-edit-mode)
            (setq-local jabber-vcard--edit-data
                        '((PHOTO "image/png" "b2xkIGF2YXRhcg==")))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "Remove")))
              (jabber-vcard-edit-avatar))
            (should-not (jabber-xml-get-children
                         (jabber-vcard-reassemble jabber-vcard--edit-data)
                         'PHOTO)))
          (with-temp-buffer
            (jabber-vcard-edit-mode)
            (setq-local jabber-vcard--edit-data nil)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "Choose file"))
                      ((symbol-function 'read-file-name)
                       (lambda (&rest _) file)))
              (jabber-vcard-edit-avatar))
            (let ((xml (jabber-vcard-reassemble jabber-vcard--edit-data)))
              (should (equal "image/png"
                             (jabber-xml-path xml '(PHOTO TYPE ""))))
              (should (equal (base64-encode-string "new avatar")
                             (jabber-xml-path xml '(PHOTO BINVAL "")))))))
      (delete-file file))))

(ert-deftest jabber-test-vcard-concurrent-account-editors-stay-isolated ()
  (let (buffers sent)
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _ignore)
                 (push buffer buffers)))
              ((symbol-function 'jabber-vcard-edit-menu) #'ignore)
              ((symbol-function 'jabber-send-iq)
               (lambda (jc _to _type data &rest _ignore)
                 (push (list jc data) sent))))
      (jabber-vcard-do-edit
       'jc-one '(iq nil (vCard nil (FN nil "One"))) nil)
      (jabber-vcard-do-edit
       'jc-two '(iq nil (vCard nil (FN nil "Two"))) nil)
      (unwind-protect
          (progn
            (should-not (eq (car buffers) (cadr buffers)))
            (dolist (buffer buffers)
              (with-current-buffer buffer
                (jabber-vcard--set
                 'NICKNAME
                 (if (eq jabber-buffer-connection 'jc-one) "Uno" "Dos"))
                (jabber-vcard-submit 'legacy-widget 'legacy-event)))
            (should
             (equal sent
                    '((jc-one
                       (vCard ((xmlns . "vcard-temp"))
                              (NICKNAME nil "Uno") nil
                              (FN nil "One") (N nil)))
                      (jc-two
                       (vCard ((xmlns . "vcard-temp"))
                              (NICKNAME nil "Dos") nil
                              (FN nil "Two") (N nil)))))))
        (mapc (lambda (buffer)
                (when (buffer-live-p buffer)
                  (kill-buffer buffer)))
              buffers)))))

(ert-deftest jabber-test-jabber-widget-compatibility-stays-lazy-and-functional ()
  (let ((widget-loaded (featurep 'widget))
        (wid-edit-loaded (featurep 'wid-edit)))
    (require 'jabber-widget)
    (should (eq widget-loaded (featurep 'widget)))
    (should (eq wid-edit-loaded (featurep 'wid-edit)))
    (should (eq (symbol-function 'jabber-widget-xdata-formtype)
                'jabber-xdata-form-type))
    (require 'wid-edit)
    (should (eq (car (get 'jabber-widget-jid 'widget-type)) 'string)))
  (with-temp-buffer
    (funcall (symbol-function 'jabber-widget-init-buffer)
             "search.example.org")
    (funcall
     (symbol-function 'jabber-widget-render-register-form)
     '(query nil (username nil "romeo") (email nil)))
    (should
     (equal (funcall (symbol-function 'jabber-widget-parse-register-form))
            '((username nil "romeo") (email nil "")))))
  (with-temp-buffer
    (funcall
     (symbol-function 'jabber-widget-render-xdata-search-results)
     '(x ((type . "result"))
         (field ((var . "jid") (label . "JID"))
                (value nil "romeo@example.org"))))
    (should (string-match-p "romeo@example.org" (buffer-string))))
  (with-temp-buffer
    (funcall
     (symbol-function 'jabber-widget-render-xdata-form)
     '(x ((type . "form"))
         (field ((var . "email")) (value nil "romeo@example.org")))
     '(("username" . "romeo")))
    (should-not (jabber-xdata-field jabber-xdata-form--form "username"))
    (should (equal '("romeo@example.org")
                   (plist-get (jabber-xdata-field
                               jabber-xdata-form--form "email")
                              :values)))))

(ert-deftest jabber-test-source-has-no-widget-editor-dependency ()
  (dolist (file (directory-files
                 (expand-file-name "lisp" jabber-test-widgetless--root)
                 t "\\.el\\'"))
    (unless (string-suffix-p "/jabber-widget.el" file)
      (with-temp-buffer
        (insert-file-contents file)
        (should-not
         (re-search-forward
          "\\(?:require '[^\n]*\\(?:widget\\|wid-edit\\|jabber-widget\\)\\|widget-\\(?:create\\|value\\|insert\\|setup\\|minor-mode\\)\\)"
          nil t))))))

(ert-deftest jabber-test-internal-feature-declaration-budget ()
  (let (internal)
    (dolist (file (directory-files
                   (expand-file-name "lisp" jabber-test-widgetless--root)
                   t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(declare-function[[:space:]]+" nil t)
          (goto-char (match-beginning 0))
          (let* ((form (read (current-buffer)))
                 (source (nth 2 form)))
            (unless (or (string-prefix-p "ext:" source)
                        (member source '("gnutls.el" "auth-source")))
              (push (list file form) internal))))))
    (should (<= (length internal) 3))))

(provide 'jabber-test-widgetless)

;;; jabber-test-widgetless.el ends here
