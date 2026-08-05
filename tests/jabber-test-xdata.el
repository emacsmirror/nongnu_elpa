;;; jabber-test-xdata.el --- Tests for Jabber data forms  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0004 data form parsing, editing, and submission.

;;; Code:

(require 'ert)
(require 'jabber-xdata)
(require 'jabber-xdata-form)

(defconst jabber-test-xdata--form
  '(x ((xmlns . "jabber:x:data") (type . "form"))
      (title nil "Room configuration")
      (instructions nil "Choose carefully.")
      (instructions nil "Defaults can be retained.")
      (field ((var . "FORM_TYPE") (type . "hidden"))
             (value nil "http://jabber.org/protocol/muc#roomconfig"))
      (field ((type . "fixed"))
             (value nil "Access"))
      (field ((var . "name") (label . "Room name") (type . "text-single"))
             (required nil)
             (value nil "Old name"))
      (field ((var . "roles") (label . "Visible roles")
              (type . "list-multi"))
             (value nil "moderator")
             (value nil "visitor")
             (option ((label . "Moderator")) (value nil "moderator"))
             (option ((label . "Participant")) (value nil "participant"))
             (option ((label . "Visitor")) (value nil "visitor")))
      (field ((var . "owners") (label . "Owners") (type . "jid-multi"))
             (value nil "one@example.org")
             (value nil "two@example.org"))
      (field ((var . "fallback") (label . "Fallback")
              (type . "vendor-special"))
             (value nil "plain")))
  "Representative editable XEP-0004 form.")

(ert-deftest jabber-test-xdata-parse-preserves-form-data ()
  "Parsing preserves ordered fields, options, defaults, and metadata."
  (let* ((form (jabber-xdata-parse jabber-test-xdata--form))
         (fields (plist-get form :fields))
         (roles (jabber-xdata-field form "roles")))
    (should (equal (plist-get form :title) "Room configuration"))
    (should (equal (plist-get form :instructions)
                   '("Choose carefully." "Defaults can be retained.")))
    (should (equal (mapcar (lambda (field) (plist-get field :var)) fields)
                   '("FORM_TYPE" nil "name" "roles" "owners" "fallback")))
    (should (plist-get (jabber-xdata-field form "name") :required))
    (should (equal (plist-get roles :values) '("moderator" "visitor")))
    (should
     (equal (plist-get roles :options)
            '((:label "Moderator" :value "moderator")
              (:label "Participant" :value "participant")
              (:label "Visitor" :value "visitor"))))
    (should (equal (plist-get (jabber-xdata-field form "owners") :values)
                   '("one@example.org" "two@example.org")))
    (should (equal (plist-get (jabber-xdata-field form "fallback") :type)
                   "text-single"))))

(ert-deftest jabber-test-xdata-list-multi-rejects-and-orders-values ()
  "List-multi editing accepts advertised options in server order only."
  (let* ((form (jabber-xdata-parse jabber-test-xdata--form))
         (edited (jabber-xdata-set-values
                  form "roles" '("visitor" "invented" "moderator"))))
    (should (equal (plist-get (jabber-xdata-field edited "roles") :values)
                   '("moderator" "visitor")))))

(ert-deftest jabber-test-xdata-set-values-does-not-mutate-source ()
  "Editing a field returns a new form without changing the source form."
  (let* ((form (jabber-xdata-parse jabber-test-xdata--form))
         (edited (jabber-xdata-set-values form "name" '("New name"))))
    (should (equal (plist-get (jabber-xdata-field form "name") :values)
                   '("Old name")))
    (should (equal (plist-get (jabber-xdata-field edited "name") :values)
                   '("New name")))))

(ert-deftest jabber-test-xdata-required-fields-are-reported ()
  "Required fields with no value are reported by label."
  (let* ((form (jabber-xdata-parse jabber-test-xdata--form))
         (edited (jabber-xdata-set-values form "name" nil)))
    (should (equal (jabber-xdata-missing-required-fields edited)
                   '("Room name")))))

(ert-deftest jabber-test-xdata-submit-preserves-hidden-and-empty-fields ()
  "Submission preserves hidden values, order, and explicit empty fields."
  (let* ((form (jabber-xdata-parse jabber-test-xdata--form))
         (edited (jabber-xdata-set-values form "owners" nil))
         (submit (jabber-xdata-submit edited))
         (fields (jabber-xml-get-children submit 'field)))
    (should (equal (mapcar (lambda (field)
                             (jabber-xml-get-attribute field 'var))
                           fields)
                   '("FORM_TYPE" "name" "roles" "owners" "fallback")))
    (should
     (equal (mapcar (lambda (value)
                      (car (jabber-xml-node-children value)))
                    (jabber-xml-get-children (nth 2 fields) 'value))
            '("moderator" "visitor")))
    (should-not (jabber-xml-get-children (nth 3 fields) 'value))
    (should
     (equal (car (jabber-xml-node-children
                  (car (jabber-xml-get-children (car fields) 'value))))
            "http://jabber.org/protocol/muc#roomconfig"))))

(ert-deftest jabber-test-xdata-boolean-without-default-stays-absent ()
  "An optional boolean without a value does not acquire false."
  (let* ((form
          (jabber-xdata-parse
           '(x ((xmlns . "jabber:x:data") (type . "form"))
               (field ((var . "enabled") (type . "boolean"))))))
         (field (jabber-xdata-field form "enabled"))
         (submitted (jabber-xdata-submit form))
         (submitted-field
          (car (jabber-xml-get-children submitted 'field))))
    (should-not (plist-get field :values))
    (should-not (jabber-xml-get-children submitted-field 'value))))

(ert-deftest jabber-test-xdata-form-list-multi-uses-advertised-options ()
  "The form editor stores selected list values in advertised order."
  (let ((jabber-xdata-form--form
         (jabber-xdata-parse jabber-test-xdata--form)))
    (cl-letf (((symbol-function 'completing-read-multiple)
               (lambda (_prompt collection &rest _ignore)
                 (should (equal collection
                                '(("Clear selection" . :clear)
                                  ("Moderator [moderator]" . "moderator")
                                  ("Participant [participant]" . "participant")
                                  ("Visitor [visitor]" . "visitor"))))
                 '("Visitor [visitor]" "Moderator [moderator]"))))
      (jabber-xdata-form-edit-field "roles"))
    (should
     (equal (plist-get
             (jabber-xdata-field jabber-xdata-form--form "roles")
            :values)
            '("moderator" "visitor")))))

(ert-deftest jabber-test-xdata-form-text-multi-uses-edit-buffer ()
  "The form editor reads text-multi values from a multiline buffer."
  (let ((jabber-xdata-form--form
         (jabber-xdata-parse
          '(x ((xmlns . "jabber:x:data") (type . "form"))
              (field ((var . "description") (type . "text-multi"))
                     (value nil "First line")
                     (value nil "Second line"))))))
    (cl-letf (((symbol-function 'read-string-from-buffer)
               (lambda (prompt initial)
                 (should (equal prompt "description: "))
                 (should (equal initial "First line\nSecond line"))
                 "Changed\nText")))
      (jabber-xdata-form-edit-field "description"))
    (should
     (equal (plist-get
             (jabber-xdata-field jabber-xdata-form--form "description")
             :values)
            '("Changed" "Text")))))

(ert-deftest jabber-test-xdata-form-submit-refuses-missing-required ()
  "The form submission boundary refuses an incomplete required field."
  (let* ((form (jabber-xdata-parse jabber-test-xdata--form))
         (jabber-xdata-form--form
          (jabber-xdata-set-values form "name" nil)))
    (should-error (jabber-xdata-form-submit-form) :type 'user-error)))

(ert-deftest jabber-test-xdata-jid-multi-ignores-duplicates ()
  "JID-multi parsing ignores duplicates while retaining first-seen order."
  (let* ((form
          (jabber-xdata-parse
           '(x ((xmlns . "jabber:x:data") (type . "form"))
               (field ((var . "owners") (type . "jid-multi"))
                      (value nil "Alice@Example.org/Home")
                      (value nil "alice@example.org/Home")
                      (value nil "alice@example.org/Work")))))
         (values (plist-get (jabber-xdata-field form "owners") :values)))
    (should (equal values
                   '("Alice@Example.org/Home" "alice@example.org/Work")))
    (let* ((submit (jabber-xdata-submit form))
           (field (car (jabber-xml-get-children submit 'field))))
      (should (= (length (jabber-xml-get-children field 'value)) 2)))))

(ert-deftest jabber-test-xdata-result-multiple-jid-columns-own-properties ()
  "Each JID cell owns its property when a result has multiple JID columns."
  (with-temp-buffer
    (jabber-xdata-render-result
     '(x ((type . "result"))
         (reported nil
                   (field ((var . "a") (label . "A")
                           (type . "jid-single")))
                   (field ((var . "b") (label . "B")
                           (type . "jid-single"))))
         (item nil
               (field ((var . "a")) (value nil "a@example.org"))
               (field ((var . "b")) (value nil "b@example.org")))))
    (goto-char (point-min))
    (search-forward "a@example.org")
    (should (equal (get-text-property (1- (point)) 'jabber-jid)
                   "a@example.org"))
    (search-forward "b@example.org")
    (should (equal (get-text-property (1- (point)) 'jabber-jid)
                   "b@example.org"))))

(ert-deftest jabber-test-xdata-form-renders-complete-staged-form ()
  "The submission buffer shows instructions, fixed text, and every field."
  (let* ((form (jabber-xdata-parse jabber-test-xdata--form))
         (actions
          (list (list :key "RET" :label "Submit"
                      :command #'ignore :submits-form t)
                (list :key "q" :label "Cancel" :command #'ignore))))
    (with-temp-buffer
      (jabber-xdata-form-mode)
      (setq-local jabber-xdata-form--form form)
      (setq-local jabber-xdata-form--original-form form)
      (setq-local jabber-xdata-form--actions actions)
      (jabber-xdata-form--render)
      (let ((text (buffer-substring (point-min) (point-max))))
        (should (string-match-p "Choose carefully\\." text))
        (should (string-match-p "Access" text))
        (should (string-match-p "Room name \\*: Old name" text))
        (should (string-match-p
                 "Visible roles: Moderator, Visitor" text))
        (should (string-match-p "Owners: one@example.org, two@example.org"
                                text))
        (should-not (string-match-p "FORM_TYPE" text))
        (should (string-match-p "\\[C-c C-c\\] Submit" text))
        (should (string-match-p "\\[q/C-c C-k\\] Cancel" text)))
      (let ((value (1+ (string-match "Moderator" (buffer-string)))))
        (should (equal (get-text-property value 'jabber-xdata-field)
                       "roles"))
        (should (button-at value))
        (should (eq (get-text-property value 'face) 'button)))
      (dolist (text '("Room configuration" "Access" "Room name" "Actions"))
        (let ((position (1+ (string-match text (buffer-string)))))
          (should-not (get-text-property position 'face))))
      (should (eq (keymap-lookup jabber-xdata-form-mode-map "u")
                  #'jabber-xdata-form-reset-field))
      (should (eq (keymap-lookup jabber-xdata-form-mode-map "U")
                  #'jabber-xdata-form-reset))
      (should-not (buffer-modified-p)))))

(ert-deftest jabber-test-xdata-form-edit-marks-pending-submission ()
  "Editing a field marks the buffer and submitting action as pending."
  (let* ((form (jabber-xdata-parse jabber-test-xdata--form))
         (action (list :key "RET" :label "Submit"
                       :command #'ignore :submits-form t)))
    (with-temp-buffer
      (jabber-xdata-form-mode)
      (setq-local jabber-xdata-form--form form)
      (setq-local jabber-xdata-form--original-form form)
      (setq-local jabber-xdata-form--actions (list action))
      (jabber-xdata-form--render)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _ignore) "New name")))
        (jabber-xdata-form-edit-field "name"))
      (should (buffer-modified-p))
      (should (string-match-p "Pending changes"
                              (substring-no-properties
                               header-line-format)))
      (let ((submit (1+ (string-match
                         "\\[C-c C-c\\] Submit" (buffer-string)))))
        (should (eq (get-text-property submit 'face)
                    'jabber-xdata-form-pending-action)))
      (let ((value (1+ (string-match "New name" (buffer-string)))))
        (should (eq (get-text-property value 'face)
                    'jabber-xdata-form-changed-value))))))

(ert-deftest jabber-test-xdata-form-reset-field-restores-default ()
  "Resetting a field restores its server value and clears its change face."
  (let ((form (jabber-xdata-parse jabber-test-xdata--form)))
    (with-temp-buffer
      (jabber-xdata-form-mode)
      (setq-local jabber-xdata-form--form form)
      (setq-local jabber-xdata-form--original-form form)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _ignore) "New name")))
        (jabber-xdata-form-edit-field "name"))
      (jabber-xdata-form-reset-field "name")
      (should
       (equal (plist-get
               (jabber-xdata-field jabber-xdata-form--form "name")
               :values)
              '("Old name")))
      (should (equal (jabber-xdata-form--field-at-point) "name"))
      (let ((value (1+ (string-match "Old name" (buffer-string)))))
        (should (eq (get-text-property value 'face) 'button)))
      (should-not (buffer-modified-p)))))

(ert-deftest jabber-test-xdata-form-reset-restores-all-defaults ()
  "Resetting the form discards every staged change."
  (let* ((form (jabber-xdata-parse jabber-test-xdata--form))
         (renamed (jabber-xdata-set-values form "name" '("New name")))
         (edited (jabber-xdata-set-values renamed "owners" nil)))
    (with-temp-buffer
      (jabber-xdata-form-mode)
      (setq-local jabber-xdata-form--form edited)
      (setq-local jabber-xdata-form--original-form form)
      (jabber-xdata-form--render "owners")
      (jabber-xdata-form-reset)
      (should (equal jabber-xdata-form--form form))
      (should (equal (jabber-xdata-form--field-at-point) "owners"))
      (should-not (buffer-modified-p)))))

(ert-deftest jabber-test-xdata-form-submit-visits-missing-field ()
  "Submission visits the first missing editable required field."
  (let ((form
         (jabber-xdata-parse
          '(x ((xmlns . "jabber:x:data") (type . "form"))
              (field ((var . "token") (type . "hidden"))
                     (required nil))
              (field ((var . "optional") (label . "Optional")
                      (type . "text-single"))
                     (value nil "Present"))
              (field ((var . "later") (label . "Later required")
                      (type . "text-single"))
                     (required nil)))))
        executed)
    (with-temp-buffer
      (jabber-xdata-form-mode)
      (setq-local jabber-xdata-form--form form)
      (setq-local jabber-xdata-form--original-form form)
      (setq-local
       jabber-xdata-form--actions
       (list (list :key "RET" :label "Submit" :submits-form t
                   :command (lambda ()
                              (interactive)
                              (setq executed t)))))
      (jabber-xdata-form--render)
      (goto-char (1+ (string-match "\\[C-c C-c\\] Submit"
                                   (buffer-string))))
      (should-not (jabber-xdata-form--field-at-point))
      (should-error (jabber-xdata-form-submit) :type 'user-error)
      (should (equal (jabber-xdata-form--field-at-point) "later"))
      (should-not executed))))

(ert-deftest jabber-test-xdata-form-clears-optional-list-defaults ()
  "Optional list fields can explicitly clear server defaults."
  (let* ((form
          (jabber-xdata-parse
           '(x ((xmlns . "jabber:x:data") (type . "form"))
               (field ((var . "single") (type . "list-single"))
                      (value nil "one")
                      (option nil (value nil "one")))
               (field ((var . "multi") (type . "list-multi"))
                      (value nil "one")
                      (option nil (value nil "one"))))))
         (single (jabber-xdata-field form "single"))
         (multi (jabber-xdata-field form "multi")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _ignore)
                 (should (eq (cdar collection) :clear))
                 (caar collection)))
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt collection &rest _ignore)
                 (should (eq (cdar collection) :clear))
                 (list (caar collection)))))
      (should-not (jabber-xdata-form--read-list-single single))
      (should-not (jabber-xdata-form--read-list-multi multi)))))

(ert-deftest jabber-test-xdata-form-open-uses-dedicated-buffer ()
  "Opening a form creates a dedicated editing buffer."
  (let (form-buffer)
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _ignore) buffer)))
      (setq form-buffer
            (jabber-xdata-form-open
             (jabber-xdata-parse jabber-test-xdata--form)
             (list (list :key "RET" :label "Submit"
                         :command #'ignore)))))
    (unwind-protect
        (with-current-buffer form-buffer
          (should (derived-mode-p 'jabber-xdata-form-mode))
          (should (string-match-p "Room configuration"
                                  (buffer-string))))
      (when (buffer-live-p form-buffer)
        (kill-buffer form-buffer)))))

(ert-deftest jabber-test-xdata-form-action-runs-in-form-buffer ()
  "Submission runs with the staged form in its dedicated buffer."
  (let (executed-buffer executed-form form-buffer)
    (setq form-buffer (generate-new-buffer " *jabber form test*"))
    (unwind-protect
        (with-current-buffer form-buffer
          (jabber-xdata-form-mode)
          (setq-local jabber-xdata-form--form
                      (jabber-xdata-parse jabber-test-xdata--form))
          (setq-local
           jabber-xdata-form--actions
           (list
            (list :key "RET" :label "Submit"
                  :command
                  (lambda ()
                    (interactive)
                    (setq executed-buffer (current-buffer)
                          executed-form jabber-xdata-form--form)))))
          (cl-letf (((symbol-function 'quit-window)
                     (lambda (&optional _kill _window)
                       (kill-buffer (current-buffer)))))
            (jabber-xdata-form-submit)))
      (when (buffer-live-p form-buffer)
        (kill-buffer form-buffer)))
    (should (eq executed-buffer form-buffer))
    (should (equal (plist-get executed-form :title)
                   "Room configuration"))
    (should-not (buffer-live-p form-buffer))))

(ert-deftest jabber-test-xdata-form-without-default-refuses-submit-key ()
  "A form without a default XEP-0050 action rejects `C-c C-c'."
  (with-temp-buffer
    (jabber-xdata-form-mode)
    (setq-local jabber-xdata-form--actions
                (list (list :key "n" :label "Next"
                            :command #'ignore)))
    (should-error (jabber-xdata-form-submit) :type 'user-error)))

(provide 'jabber-test-xdata)
;;; jabber-test-xdata.el ends here
