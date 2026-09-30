;;; jabber-test-menu.el --- Tests for jabber-menu  -*- lexical-binding: t; -*-

;;; Commentary:

;; Keymap popup menu structure.

;;; Code:

(require 'ert)
(require 'jabber-bookmarks)
(require 'jabber-chat-commands)
(require 'jabber-disco-menu)
(require 'jabber-keymap)
(require 'jabber-muc-menu)
(require 'jabber-omemo-trust)
(require 'jabber-roster-menu)

;;; Helpers

(defun jabber-test-menu--extract-popup-commands (keymap)
  "Extract jabber command symbols bound in KEYMAP.
Only returns symbols with a `jabber-' prefix, skipping
inherited bindings from parent mode keymaps."
  (let (commands)
    (map-keymap
     (lambda (_key binding)
       (when (and (symbolp binding)
                  (string-prefix-p "jabber-" (symbol-name binding)))
         (push binding commands)))
     keymap)
    commands))

;;; Tests

(ert-deftest jabber-test-menu-popup-commands-defined ()
  "Every command in a jabber popup keymap must be fboundp."
  (let ((maps (list jabber-common-keymap
                    jabber-global-keymap
                    jabber-chat-operations-menu-map
                    jabber-chat-encryption-menu-map
                    jabber-roster-popup-map
                    jabber-roster-presence-map
                    jabber-roster-discovery-map
                    jabber-roster-contact-action-map
                    jabber-info-menu-map
                    jabber-muc-menu-map
                    jabber-service-menu-map
                    jabber-bookmarks-mode-map
                    jabber-bookmarks-edit-map
                    jabber-omemo-trust-mode-map))
        (missing nil))
    (dolist (map maps)
      (dolist (cmd (jabber-test-menu--extract-popup-commands map))
        (unless (fboundp cmd)
          (push (format "%s" cmd) missing))))
    (should (null missing))))

(ert-deftest jabber-test-menu-global-bindings ()
  "Expose every global Jabber command through its prefix map."
  (dolist (binding '(("C-c" . jabber-connect-all)
                     ("C-d" . jabber-disconnect)
                     ("C-r" . jabber-roster-popup)
                     ("C-j" . jabber-chat-with)
                     ("C-l" . jabber-activity-switch-to)
                     ("C-a" . jabber-send-away-presence)
                     ("C-o" . jabber-send-default-presence)
                     ("C-x" . jabber-send-xa-presence)
                     ("C-p" . jabber-send-presence)
                     ("C-b" . jabber-chat-buffer-switch)
                     ("C-m" . jabber-muc-join)))
    (should (eq (keymap-lookup jabber-global-keymap (car binding))
                (cdr binding))))
  (should-not (keymap-lookup jabber-global-keymap "C-g")))

(ert-deftest jabber-test-menu-thread-commands ()
  "Expose thread roots in the menu and keep normal thread sending on RET."
  (should
   (eq (keymap-lookup jabber-chat-operations-menu-map "t")
       'jabber-message-thread-open))
  (should
   (eq (keymap-lookup jabber-chat-operations-menu-map "T")
       'jabber-message-thread-start))
  (should
   (eq (keymap-lookup jabber-chat-operations-menu-map "l")
       'jabber-message-thread-browse))
  (let ((jabber-message-thread-id nil))
    (should-not
     (keymap-lookup jabber-chat-operations-menu-map "L")))
  (let ((jabber-message-thread-id "thread-id"))
    (should
     (eq (keymap-lookup jabber-chat-operations-menu-map "L")
         'jabber-message-thread-set-title)))
  (should
   (eq (keymap-lookup jabber-chat-mode-map "C-c C-t")
       'jabber-message-thread-open))
  (should
   (eq (keymap-lookup jabber-chat-mode-map "RET")
       'jabber-chat-goto-reply-target-or-send)))

(ert-deftest jabber-test-menu-thread-title-context ()
  "Resolve the title command only in its current thread buffer."
  (with-temp-buffer
    (let ((map jabber-chat-operations-menu-map))
      (use-local-map map)
      (dolist (id '(nil "thread-id" nil "other-thread"))
        (setq-local jabber-message-thread-id id)
        (let ((command (and id #'jabber-message-thread-set-title)))
          (should (eq (keymap-lookup map "L") command))
          (should (eq (local-key-binding (kbd "L")) command)))))))

(defun jabber-test-menu--connection (name contacts)
  "Return a disposable connection NAME with CONTACTS."
  (let ((jc (make-symbol name)))
    (put jc :state-data (list :username name :server "example.test"
                              :roster (mapcar (lambda (jid) (jabber-jid-symbol jid jc)) contacts)))
    jc))

(ert-deftest jabber-test-menu-scoped-contact-actions ()
  "Every action uses the selected account when peers overlap."
  (let* ((jabber-jid-obarray (make-vector 31 0))
         (a (jabber-test-menu--connection "a" '("peer@example.test")))
         (b (jabber-test-menu--connection "b" '("peer@example.test")))

         (jabber-connections (list a b))
         (jabber-roster--scoped-connection b)
         (jabber-roster--selected-jid "peer@example.test")
         calls)
    (cl-letf (((symbol-function 'jabber-chat-with)
               (lambda (jc jid) (push (list jc jid) calls)))
              ((symbol-function 'jabber-get-info)
               (lambda (jc jid) (push (list jc jid) calls)))
              ((symbol-function 'jabber-roster-delete)
               (lambda (jc jid) (push (list jc jid) calls)))
              ((symbol-function 'jabber-blocking-block-jid)
               (lambda (jc jid) (push (list jc jid) calls)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (mapc #'call-interactively
            '(jabber-roster--action-chat jabber-roster--action-info
              jabber-roster--action-delete jabber-roster--action-block)))
    (should (= (length calls) 4))
    (should (cl-every (lambda (call) (eq (car call) b)) calls))))

(ert-deftest jabber-test-menu-selection-survives-scope-change ()
  "A selected contact retains its owner even after scope changes."
  (let* ((jabber-jid-obarray (make-vector 31 0))
         (a (jabber-test-menu--connection "a" '("peer@example.test")))
         (b (jabber-test-menu--connection "b" '("peer@example.test")))
         (peer (jabber-jid-symbol "peer@example.test" b))
         (jabber-connections (list a b))
         (jabber-roster--scoped-connection b)
         (jabber-roster--selected-jid nil)
         calls)
    (cl-letf (((symbol-function 'jabber-read-jid-completing)
               (lambda (_prompt subset &rest _)
                 (should (equal subset (list peer)))
                 (setq jabber-roster--scoped-connection a)
                 "peer@example.test"))
              ((symbol-function 'keymap-popup) #'ignore)
              ((symbol-function 'jabber-chat-with)
               (lambda (jc jid) (push (list jc jid) calls))))
      (call-interactively #'jabber-roster-chat-any)
      (call-interactively #'jabber-roster--action-chat)
      (should (eq (caar calls) b))
      (setq jabber-connections (list a))
      (should-error (call-interactively #'jabber-roster--action-chat)
                    :type 'user-error)
      (should (= (length calls) 1)))))

(ert-deftest jabber-test-menu-delete-retains-selection-through-prompt ()
  "Confirmation cannot retarget the selected contact or retired account."
  (let* ((jabber-jid-obarray (make-vector 31 0))
         (a (jabber-test-menu--connection "a" '("peer@example.test")))
         (b (jabber-test-menu--connection "b" '("peer@example.test")))

         (jabber-connections (list a b))
         (jabber-roster--scoped-connection b)
         (jabber-roster--selected-jid "peer@example.test")
         sent)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (setq jabber-roster--selected-jid "other@example.test"
                       jabber-roster--scoped-connection a)
                 t))
              ((symbol-function 'jabber-roster-delete)
               (lambda (jc jid) (setq sent (list jc jid)))))
      (call-interactively #'jabber-roster--action-delete)
      (should (equal sent (list b "peer@example.test"))))))

(ert-deftest jabber-test-menu-online-scoped-and-empty ()
  "Online completion stays scoped, including when its subset is empty."
  (let* ((jabber-jid-obarray (make-vector 31 0))
         (a (jabber-test-menu--connection "a" '("peer@example.test")))
         (b (jabber-test-menu--connection "b" '("peer@example.test")))
         (peer (jabber-jid-symbol "peer@example.test" b))
         (jabber-connections (list a b))
         (jabber-roster--scoped-connection b)
         sent)
    (put peer 'connected t)
    (cl-letf (((symbol-function 'jabber-read-jid-completing)
               (lambda (_prompt subset &rest _)
                 (should (equal subset (list peer)))
                 (setq jabber-roster--scoped-connection a)
                 "peer@example.test"))
              ((symbol-function 'jabber-chat-with)
               (lambda (jc jid) (setq sent (list jc jid)))))
      (call-interactively #'jabber-roster-chat-online)
      (should (equal sent (list b "peer@example.test")))
      (put peer 'connected nil)
      (should-error (call-interactively #'jabber-roster-chat-online)
                    :type 'user-error))))

(ert-deftest jabber-test-menu-ambiguous-contact-asks-and-retains-account ()
  "Unscoped duplicate peers require an account choice, retained by actions."
  (let* ((jabber-jid-obarray (make-vector 31 0))
         (a (jabber-test-menu--connection "a" '("peer@example.test")))
         (b (jabber-test-menu--connection "b" '("peer@example.test")))

         (jabber-connections (list a b))
         (jabber-roster--scoped-connection nil)
         (jabber-roster--selected-jid nil)
         (choices 0)
         sent)
    (cl-letf (((symbol-function 'jabber-read-jid-completing)
               (lambda (&rest _) "peer@example.test"))
              ((symbol-function 'completing-read)
               (lambda (_prompt accounts &rest _)
                 (cl-incf choices)
                 (should (equal (mapcar #'cdr accounts) (list a b)))
                 "b@example.test"))
              ((symbol-function 'keymap-popup) #'ignore)
              ((symbol-function 'jabber-blocking-block-jid)
               (lambda (jc jid) (setq sent (list jc jid)))))
      (call-interactively #'jabber-roster-chat-any)
      (call-interactively #'jabber-roster--action-block)
      (should (= choices 1))
      (should (equal sent (list b "peer@example.test"))))))

(ert-deftest jabber-test-menu-roster-edit-policy ()
  "Both entry points preserve defaults, empty groups, and selected account."
  (dolist (command '(jabber-roster-change jabber-roster--action-edit))
    (dolist (groups '(("friends") ("work" "family") ("") nil))
      (let* ((jabber-jid-obarray (make-vector 31 0))
             (jc (jabber-test-menu--connection "a" '("peer@example.test")))
             (peer (jabber-jid-symbol "peer@example.test" jc))
             (jabber-connections (list jc))
             (jabber-roster--scoped-connection jc)
             (jabber-roster--selected-jid "peer@example.test")
             sent)
        (put peer 'name "Peer")
        (put peer 'groups '("friends"))
        (cl-letf (((symbol-function 'jabber-read-jid-completing)
                   (lambda (&rest _) "peer@example.test"))
                  ((symbol-function 'read-string)
                   (lambda (_prompt _initial _history default &rest _)
                     (should (equal default "Peer"))
                     ;; A recursive command changing global selection is harmless.
                     (setq jabber-roster--selected-jid "other@example.test")
                     default))
                  ((symbol-function 'completing-read-multiple)
                   (lambda (_prompt candidates _pred _match _initial _history default &rest _)
                     (should (equal candidates '("friends")))
                     (should (equal default "friends"))
                     (copy-sequence groups)))
                  ((symbol-function 'jabber-send-iq)
                   (lambda (owner _to _type stanza &rest _)
                     (setq sent (list owner stanza)))))
          (call-interactively command)
          (should (eq (car sent) jc))
          (let ((item (car (jabber-xml-get-children (cadr sent) 'item))))
            (should (equal (jabber-xml-get-attribute item 'jid) "peer@example.test"))
            (should (equal (jabber-xml-get-attribute item 'name) "Peer"))
            (should (equal (mapcar #'jabber-xml-node-children
                                  (jabber-xml-get-children item 'group))
                           (mapcar #'list (delete "" (copy-sequence groups)))))))))))

(ert-deftest jabber-test-menu-roster-edit-quit-and-disconnect ()
  "Cancellation and retirement during editing cause zero remote mutation."
  (dolist (command '(jabber-roster-change jabber-roster--action-edit))
    (dolist (failure '(quit disconnect))
      (let* ((jabber-jid-obarray (make-vector 31 0))
             (jc (jabber-test-menu--connection "a" '("peer@example.test")))

             (jabber-connections (list jc))
             (jabber-roster--scoped-connection jc)
             (jabber-roster--selected-jid "peer@example.test")
             sent)
        (cl-letf (((symbol-function 'jabber-read-jid-completing)
                   (lambda (&rest _) "peer@example.test"))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "Peer"))
                  ((symbol-function 'completing-read-multiple)
                   (lambda (&rest _)
                     (if (eq failure 'quit) (signal 'quit nil)
                       (setq jabber-connections nil)
                       '("friends"))))
                  ((symbol-function 'jabber-send-iq)
                   (lambda (&rest _) (setq sent t))))
          (if (eq failure 'quit)
              (should (eq (condition-case nil
                              (progn (call-interactively command) 'sent)
                            (quit 'cancelled))
                          'cancelled))
            (should-error (call-interactively command) :type 'user-error))
          (should-not sent))))))

(ert-deftest jabber-test-menu-scoped-room-switch ()
  "Room candidates and the actual switched buffer retain the selected account."
  (let* ((a (jabber-test-menu--connection "a" nil))
         (b (jabber-test-menu--connection "b" nil))
         (jabber-connections (list a b))
         (jabber-roster--scoped-connection b)
         (jabber-muc--rooms (make-hash-table :test #'equal))
         (jabber-db-path nil)
         (jabber-chat-mode-hook nil)
         created)
    (unwind-protect
        (save-window-excursion
          (jabber-muc-join-set "only-a@example.test" a "alice")
          (jabber-muc-join-set "shared@example.test" a "alice")
          (jabber-muc-join-set "shared@example.test" b "bob")
          (push (jabber-muc-create-buffer a "shared@example.test") created)
          (push (jabber-muc-create-buffer b "shared@example.test") created)
          (cl-letf (((symbol-function 'jabber-get-conference-data)
                     (lambda (jc &rest _)
                       (should (eq jc b))
                       "Shared"))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt table &rest _)
                       (should (equal (all-completions "" table) '("Shared")))
                       (setq jabber-roster--scoped-connection a)
                       "Shared")))
            (jabber-roster-switch-muc t)
            (should (eq (current-buffer) (car created)))
            (should (eq jabber-buffer-connection b))))
      (mapc #'kill-buffer created))))

(provide 'jabber-test-menu)
;;; jabber-test-menu.el ends here
