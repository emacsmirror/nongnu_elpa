;;; jabber-test-activity-owners.el --- Owned activity regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise real dispatch, buffers, EWOCs, visibility, and navigation without I/O.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'jabber-activity)
(require 'jabber-subscription)
(require 'jabber-presence)
(require 'jabber-mam)
(require 'jabber-roster-menu)
(require 'jabber-message-thread)
(require 'jabber-keymap)

(defun jabber-test-activity-owners--connection (name)
  "Return a disposable connection named NAME."
  (let ((jc (make-symbol name)))
    (put jc :state-data (list :username name :server "example.test"))
    jc))

(defmacro jabber-test-activity-owners--with-state (&rest body)
  "Run BODY with isolated activity, connection, and buffer state."
  (declare (indent 0) (debug t))
  `(let* ((a (jabber-test-activity-owners--connection "a"))
          (b (jabber-test-activity-owners--connection "b"))
          (jabber-connections (list a b))
          (jabber-db-path nil)
          (jabber-mam-enable nil)
          (jabber-activity-jids nil)
          (jabber-activity-personal-jids nil)
          (jabber-activity-name-alist nil)
          (jabber-activity-mode-string "")
          (jabber-activity-count-string "0")
          (jabber-activity-last-buffer nil)
          (jabber-activity--shortened-names (make-hash-table :test #'equal))
          (jabber-activity-update-hook nil)
          (jabber-activity-show-p #'jabber-activity-show-p-default)
          (jabber-activity-make-string #'identity)
          (jabber-activity-make-strings #'jabber-activity-make-strings-default)
          (jabber-activity--event-owner nil)
          (jabber-buffer-registry--buffers (make-hash-table :test #'equal))
          (jabber-muc--rooms (make-hash-table :test #'equal))
          (jabber-chat-buffer-format " *activity-%a-%j*")
          (jabber-groupchat-buffer-format " *activity-%a-%n*")
          (jabber-muc-private-buffer-format " *activity-%a-%g-%n*")
          (jabber-chat-default-encryption 'plaintext)
          (jabber-chat-mode-hook nil)
          (jabber-message-hooks '(jabber-activity-add))
          (jabber-alert-message-hooks nil)
          (jabber-muc-hooks '(jabber-activity-add-muc))
          (jabber-alert-muc-hooks nil)
          (jabber-presence-hooks '(jabber-activity-presence))
          (jabber-alert-presence-hooks nil)
          (jabber-alert-message-function (lambda (&rest _) "Message"))
          (jabber-alert-muc-function (lambda (&rest _) "Room"))
          (jabber-alert-presence-message-function (lambda (&rest _) "Presence"))
          (before (buffer-list)))
     (unwind-protect
         (save-window-excursion
           (with-temp-buffer ,@body))
       (dolist (buffer (seq-difference (buffer-list) before))
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun jabber-test-activity-owners--message (jc buffer)
  "Deliver an incoming direct message on JC into BUFFER."
  (jabber-chat--display-message
   jc '(message nil) buffer nil "peer@example.test"
   (list :from "peer@example.test" :body "hello" :timestamp (current-time))))

(ert-deftest jabber-test-activity-owners-direct-visibility ()
  "Visible A never suppresses B; actual dispatch retains the background owner."
  (jabber-test-activity-owners--with-state
    (let ((ab (jabber-chat-create-buffer a "peer@example.test"))
          (bb (jabber-chat-create-buffer b "peer@example.test")))
      (setq-local jabber-buffer-connection a)
      (jabber-test-activity-owners--message a ab)
      (jabber-test-activity-owners--message b bb)
      (should (= 2 (length jabber-activity-jids)))
      (should (= 2 (length jabber-activity-personal-jids)))
      (set-window-buffer (selected-window) ab)
      (jabber-activity-clean)
      (should (equal jabber-activity-jids (list (cons b "peer@example.test"))))
      (jabber-test-activity-owners--message b bb)
      (should (= 1 (length jabber-activity-jids)))
      (jabber-activity-switch-to)
      (should (eq (current-buffer) bb))
      (should-not jabber-activity-jids))))

(ert-deftest jabber-test-activity-owners-killed-and-retired ()
  "Killed B is recreated on B, but a retired B is never redirected to A."
  (jabber-test-activity-owners--with-state
    (let* ((bb (jabber-chat-create-buffer b "peer@example.test"))
           (entry (cons b "peer@example.test")))
      (jabber-test-activity-owners--message b bb)
      (kill-buffer bb)
      (jabber-activity-switch-to entry)
      (should (eq jabber-buffer-connection b))
      (should (equal jabber-chatting-with "peer@example.test"))
      (kill-buffer (current-buffer))
      (setq jabber-connections (list a)
            jabber-activity-jids (list entry))
      (let ((before (current-buffer)))
        (jabber-activity-switch-to entry)
        (should (eq (current-buffer) before)))
      (should-not jabber-activity-jids))))

(ert-deftest jabber-test-activity-owners-closed-muc-dispatch ()
  "Closed-room events retain owner and mention policy across earlier hooks."
  (jabber-test-activity-owners--with-state
    (jabber-muc-join-set "room@example.test" a "alice")
    (jabber-muc-join-set "room@example.test" b "bob")
    (setq-local jabber-buffer-connection a)
    (let ((foreign (current-buffer))
          (jabber-muc-hooks
           (list (lambda (&rest _) (set-buffer (get-buffer-create " *activity-hook*")))
                 #'jabber-activity-add-muc)))
      (jabber-muc--display-message
       b '(message nil) "room@example.test" "sender" :muc-foreign
       '(:from "room@example.test/sender" :body "bob: hello"))
      (should (eq (current-buffer) foreign)))
    (jabber-muc--display-message
     a '(message nil) "room@example.test" "sender" :muc-foreign
     '(:from "room@example.test/sender" :body "bob: hello"))
    (should (= 2 (length jabber-activity-jids)))
    (should (equal jabber-activity-personal-jids
                   (list (cons b "room@example.test"))))
    (jabber-activity-switch-to (cons b "room@example.test"))
    (should (eq jabber-buffer-connection b))
    (should (equal jabber-group "room@example.test"))
    (should (equal jabber-activity-jids (list (cons a "room@example.test"))))))

(ert-deftest jabber-test-activity-owners-muc-visible-and-killed ()
  "Room visibility and killed-buffer navigation are account qualified."
  (jabber-test-activity-owners--with-state
    (jabber-muc-join-set "room@example.test" a "alice")
    (jabber-muc-join-set "room@example.test" b "bob")
    (let ((ab (jabber-muc-create-buffer a "room@example.test"))
          (bb (jabber-muc-create-buffer b "room@example.test")))
      (set-window-buffer (selected-window) ab)
      (jabber-muc--display-message
       b '(message nil) "room@example.test" "sender" :muc-foreign
       '(:from "room@example.test/sender" :body "bob: hello"))
      (jabber-activity-clean)
      (should (equal jabber-activity-jids (list (cons b "room@example.test"))))
      (kill-buffer bb)
      (jabber-activity-switch-to)
      (should (eq jabber-buffer-connection b))
      (should-not (eq (current-buffer) ab)))))

(ert-deftest jabber-test-activity-owners-private-muc-killed ()
  "A private MUC entry reconstructs only its original account's buffer."
  (jabber-test-activity-owners--with-state
    (jabber-muc-join-set "room@example.test" a "alice")
    (jabber-muc-join-set "room@example.test" b "bob")
    (let* ((jid "room@example.test/sender")
           (bb (jabber-muc-private-create-buffer b "room@example.test" "sender")))
      (jabber-activity-add jid bb "hello" "Message")
      (kill-buffer bb)
      (jabber-activity-switch-to)
      (should (eq jabber-buffer-connection b))
      (should (equal jabber-chatting-with jid)))))

(ert-deftest jabber-test-activity-owners-formatters-and-mouse ()
  "Fixed-arity string formatters retain separate cache keys and mouse owners."
  (jabber-test-activity-owners--with-state
    (let ((ab (jabber-chat-create-buffer a "peer@example.test"))
          (bb (jabber-chat-create-buffer b "peer@example.test"))
          (jabber-activity-show-p (lambda (jid) (stringp jid)))
          (jabber-activity-make-string (lambda (jid) (concat "name:" jid)))
          (jabber-activity-make-strings
           (lambda (jids)
             (should (seq-every-p #'stringp jids))
             (jabber-activity-make-strings-shorten jids))))
      (jabber-activity-add "peer@example.test" ab "hi" "Message")
      (jabber-activity-add "peer@example.test" bb "hi" "Message")
      (should (= 2 (length jabber-activity-name-alist)))
      (should (= 2 (hash-table-count jabber-activity--shortened-names)))
      (let* ((entry (jabber-activity-lookup-name (cons b "peer@example.test")))
             (text (jabber-activity--propertize-entry entry))
             (map (get-text-property 0 'local-map text)))
        (call-interactively (lookup-key map [mode-line mouse-1]))
        (should (eq (current-buffer) bb))))))

(ert-deftest jabber-test-activity-owners-ambiguous-legacy ()
  "An accountless action never uses the selected or first active account."
  (jabber-test-activity-owners--with-state
    (setq-local jabber-buffer-connection a)
    (let ((before (current-buffer)))
      (jabber-activity-switch-to "peer@example.test")
      (should (eq (current-buffer) before)))
    (should (equal (jabber-activity-make-string-default "peer@example.test")
                   "peer"))))

(ert-deftest jabber-test-activity-owners-disconnect ()
  "Disconnecting A preserves B's activity and personal state."
  (jabber-test-activity-owners--with-state
    (setq jabber-activity-jids (list (cons a "peer@example.test")
                                     (cons b "peer@example.test"))
          jabber-activity-personal-jids (copy-sequence jabber-activity-jids)
          jabber-connections (list b))
    (jabber-activity--on-disconnect)
    (should (equal jabber-activity-jids (list (cons b "peer@example.test"))))
    (should (equal jabber-activity-jids jabber-activity-personal-jids))))

(ert-deftest jabber-test-activity-owners-roster-unread ()
  "Unread completion keeps B's identity across the interactive reader."
  (jabber-test-activity-owners--with-state
    (let ((bb (jabber-chat-create-buffer b "peer@example.test")))
      (setq jabber-activity-jids (list (cons a "peer@example.test")
                                       (cons b "peer@example.test")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _)
                   (should (= 2 (length candidates)))
                   (car (nth 1 candidates)))))
        (call-interactively #'jabber-roster-chat-unread))
      (should (eq (current-buffer) bb)))))

(ert-deftest jabber-test-activity-owners-subscription-who ()
  "Native subscription delivery uses scoped WHO despite background context."
  (jabber-test-activity-owners--with-state
    (setq-local jabber-buffer-connection a)
    (jabber-presence-events-dispatch-subscription-request
     a "peer@example.test" "first")
    (jabber-presence-events-dispatch-subscription-request
     b "peer@example.test" "second")
    (should (= 2 (length jabber-activity-jids)))
    (should (member (cons a "peer@example.test") jabber-activity-jids))
    (should (member (cons b "peer@example.test") jabber-activity-jids))))

(ert-deftest jabber-test-activity-owners-subscription-cleanup ()
  "Native ordinary presence removes only exact-owner EWOC request nodes."
  (jabber-test-activity-owners--with-state
    (dolist (order '(forward reverse))
      (let* ((ab (jabber-chat-create-buffer a "peer@example.test"))
             (bb (jabber-chat-create-buffer b "peer@example.test")))
        (dolist (buffer (if (eq order 'forward) (list ab bb) (list bb ab)))
          (with-current-buffer buffer
            (jabber-buffer-registry-register 'chat "peer@example.test")
            (jabber-chat-ewoc-enter '(:subscription-request "allow"))
            (jabber-chat-ewoc-enter '(:notice "keep"))))
        (with-current-buffer bb
          (jabber-process-presence a '(presence ((from . "peer@example.test/phone")))))
        (with-current-buffer ab
          (should (equal (ewoc-collect jabber-chat-ewoc #'identity)
                         '((:notice "keep")))))
        (with-current-buffer bb
          (should (equal (ewoc-collect jabber-chat-ewoc #'identity)
                         '((:subscription-request "allow") (:notice "keep")))))
        (kill-buffer ab)
        (jabber-process-presence a '(presence ((from . "peer@example.test/phone"))))
        (setq jabber-connections (list a))
        (jabber-process-presence b '(presence ((from . "peer@example.test/phone"))))
        (with-current-buffer bb
          (should (= 2 (length (ewoc-collect jabber-chat-ewoc #'identity)))))
        (kill-buffer bb)
        (setq jabber-connections (list a b))))))

(ert-deftest jabber-test-activity-owners-subscription-reply ()
  "Reply uses the request buffer's connection and removes just its prompt."
  (jabber-test-activity-owners--with-state
    (let ((bb (jabber-chat-create-buffer b "peer@example.test")) sent)
      (with-current-buffer bb
        (let ((node (jabber-chat-ewoc-enter '(:subscription-request "allow"))))
          (goto-char (ewoc-location node))
          (cl-letf (((symbol-function 'jabber-send-sexp)
                     (lambda (jc stanza) (push (cons jc stanza) sent))))
            (jabber-subscription-reply "subscribed"))
          (should (eq (caar sent) b))
          (should-not (ewoc-collect jabber-chat-ewoc #'identity)))))))

(ert-deftest jabber-test-activity-owners-closed-direct-dispatch ()
  "A closed direct-message event retains its receiver across earlier hooks."
  (jabber-test-activity-owners--with-state
    (setq-local jabber-buffer-connection a)
    (let ((jabber-message-hooks
           (list (lambda (&rest _) (set-buffer (get-buffer-create " *activity-hook*")))
                 #'jabber-activity-add)))
      (jabber-test-activity-owners--message b nil))
    (should (equal jabber-activity-jids (list (cons b "peer@example.test"))))))

(ert-deftest jabber-test-activity-owners-reconstruction-name-collision ()
  "Reconstructing B cannot adopt A's buffer with an accountless format."
  (jabber-test-activity-owners--with-state
    (let* ((jabber-chat-buffer-format " *activity-collision-%j*")
           (ab (jabber-chat-create-buffer a "peer@example.test")))
      (jabber-activity-switch-to (cons b "peer@example.test"))
      (should (eq jabber-buffer-connection b))
      (should-not (eq (current-buffer) ab))
      (should (eq (buffer-local-value 'jabber-buffer-connection ab) a)))))

(ert-deftest jabber-test-activity-owners-names ()
  "Default naming and shortening use each entry's scoped contact metadata."
  (jabber-test-activity-owners--with-state
    (put (jabber-jid-symbol "peer@example.test" a) 'name "Alice")
    (put (jabber-jid-symbol "peer@example.test" b) 'name "Bob")
    (let ((jabber-activity-make-string #'jabber-activity-make-string-default))
      (setq jabber-activity-jids (list (cons a "peer@example.test")
                                     (cons b "peer@example.test")))
      (jabber-activity-make-name-alist)
      (should (equal (cdr (jabber-activity-lookup-name (cons a "peer@example.test")))
                     "Alice"))
      (should (equal (cdr (jabber-activity-lookup-name (cons b "peer@example.test")))
                     "Bob"))
      (should (equal (jabber-activity-make-string-default "peer@example.test")
                     "peer")))))

(ert-deftest jabber-test-activity-owners-unowned-and-killed-buffer ()
  "A legacy event without an explicit owner never inherits the current chat."
  (jabber-test-activity-owners--with-state
    (setq-local jabber-buffer-connection a)
    (jabber-activity-add "peer@example.test" nil "hello" "Message")
    (let ((buffer (generate-new-buffer " *activity-killed*")))
      (with-current-buffer buffer (setq-local jabber-buffer-connection b))
      (kill-buffer buffer)
      (jabber-activity-add "peer@example.test" buffer "hello" "Message"))
    (should-not jabber-activity-jids)))

(ert-deftest jabber-test-activity-owners-roster-reader-retirement ()
  "Retiring B during unread completion cannot redirect the action to A."
  (jabber-test-activity-owners--with-state
    (setq jabber-activity-jids (list (cons b "peer@example.test")))
    (let ((before (current-buffer)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _)
                   (setq jabber-connections (list a))
                   (caar candidates))))
        (call-interactively #'jabber-roster-chat-unread))
      (should (eq (current-buffer) before))
      (should-not jabber-activity-jids))))

(defun jabber-test-activity-owners--parent (jc peer type)
  "Create the parent for JC, PEER and TYPE."
  (if (equal type "groupchat")
      (progn
        (jabber-muc-join-set peer jc "reader")
        (jabber-muc-create-buffer jc peer))
    (jabber-chat-create-buffer jc peer)))

(defun jabber-test-activity-owners--deliver (jc peer type body &optional thread)
  "Dispatch BODY from PEER on JC as TYPE, optionally in THREAD."
  (funcall (if (equal type "groupchat")
               #'jabber-muc-process-message
             #'jabber-process-chat)
           jc `(message ((from . ,(if (equal type "groupchat")
                                      (concat peer "/sender") peer))
                         (type . ,type))
                        (body nil ,body)
                        ,@(when thread `((thread nil ,thread))))))

(defmacro jabber-test-activity-owners--with-threads (type reverse &rest body)
  "Run BODY with real parent/thread views of TYPE in REVERSE account order."
  (declare (indent 2) (debug (form form body)))
  `(jabber-test-activity-owners--with-state
     (let* ((peer "peer@example.test")
            (kind ,type)
            (jabber-connections (if ,reverse (list b a) (list a b)))
            (parents
             (mapcar (lambda (jc)
                       (cons jc (jabber-test-activity-owners--parent jc peer kind)))
                     jabber-connections))
            (ab (cdr (assq a parents)))
            (bb (cdr (assq b parents)))
            (threads
             (mapcar (lambda (jc)
                       (cons jc (jabber-message-thread-create-buffer
                                 jc peer kind "topic" nil (cdr (assq jc parents)))))
                     jabber-connections))
            (thread (cdr (assq b threads)))
            (entry (cons b peer)))
       (cl-letf (((symbol-function 'jabber-send-sexp)
                  (lambda (&rest _) (error "Unexpected wire send"))))
         ,@body))))

(ert-deftest jabber-test-activity-owners-parent-thread-visibility ()
  "A visible dedicated thread never hides unthreaded parent activity."
  (dolist (type '("chat" "groupchat"))
    (dolist (reverse '(nil t))
      (jabber-test-activity-owners--with-threads type reverse
        (switch-to-buffer thread)
        (jabber-test-activity-owners--deliver a peer kind "foreign parent")
        (jabber-test-activity-owners--deliver b peer kind "own parent")
        (should (get-buffer-window thread))
        (should-not (get-buffer-window ab))
        (should-not (get-buffer-window bb))
        (with-current-buffer bb
          (should (string-match-p "own parent" (buffer-string))))
        (with-current-buffer thread
          (should-not (string-match-p "own parent" (buffer-string))))
        (should (member entry jabber-activity-jids))
        (should (member (cons a peer) jabber-activity-jids))
        (jabber-activity-clean)
        (should (member entry jabber-activity-jids))
        (switch-to-buffer bb)
        (jabber-activity-clean)
        (should-not (member entry jabber-activity-jids))
        (should (equal jabber-activity-jids (list (cons a peer))))))))

(ert-deftest jabber-test-activity-owners-parent-thread-navigation ()
  "Native activity actions select parents across renaming and recreation."
  (dolist (type '("chat" "groupchat"))
    (dolist (reverse '(nil t))
      (dolist (lifetime '(original renamed closed))
        (dolist (action '(keyboard mouse unread))
          (jabber-test-activity-owners--with-threads type reverse
            (switch-to-buffer (get-buffer-create " *activity-origin*"))
            (jabber-test-activity-owners--deliver a peer kind "foreign parent")
            (jabber-test-activity-owners--deliver b peer kind "own parent")
            (pcase lifetime
              ('renamed
               (let ((old-name (buffer-name bb)))
                 (with-current-buffer bb (rename-buffer " *activity-renamed*" t))
                 (with-current-buffer (get-buffer-create old-name)
                   (insert "Unrelated replacement"))))
              ('closed (kill-buffer bb)))
            ;; Make the wrong view the most recently selected matching buffer.
            (switch-to-buffer thread)
            (switch-to-buffer (get-buffer-create " *activity-origin*"))
            (pcase action
              ('keyboard
               (should (eq (key-binding (kbd "C-x C-j C-l"))
                           #'jabber-activity-switch-to))
               (execute-kbd-macro (kbd "C-x C-j C-l")))
              ('mouse
               (let* ((text (jabber-activity--propertize-entry
                             (jabber-activity-lookup-name entry)))
                      (map (get-text-property 0 'local-map text)))
                 (call-interactively (lookup-key map [mode-line mouse-1]))))
              ('unread
               ;; Supply only the human choice, never lookup or navigation.
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (_prompt candidates &rest _)
                            (car (rassoc entry candidates)))))
                 (call-interactively #'jabber-roster-chat-unread))))
            (should (eq jabber-buffer-connection b))
            (should-not jabber-message-thread-id)
            (should-not (eq (current-buffer) ab))
            (unless (eq lifetime 'closed)
              (should (eq (current-buffer) bb)))
            (should (equal (or jabber-group jabber-chatting-with) peer))
            (should-not (member entry jabber-activity-jids))
            (should (equal jabber-activity-jids (list (cons a peer))))
            (should (buffer-live-p thread))
            (should (equal (buffer-local-value 'jabber-message-thread-id thread)
                           "topic"))))))))

(ert-deftest jabber-test-activity-owners-parent-thread-actions ()
  "Opening and renaming a real thread preserves parent activity routing."
  (dolist (type '("chat" "groupchat"))
    (jabber-test-activity-owners--with-state
      (let* ((dir (make-temp-file "jabber-activity-thread-" t))
             (jabber-db-path (expand-file-name "test.sqlite" dir))
             (jabber-db--connection nil)
             (peer "peer@example.test")
             (parent (jabber-test-activity-owners--parent b peer type)))
        (unwind-protect
            (progn
              (switch-to-buffer parent)
              (jabber-message-thread-open
               (list :id "root" :server-id "server-root" :thread-id "topic"
                     :from (concat peer "/sender") :body "Thread root"
                     :timestamp (current-time)))
              (let ((thread (current-buffer)))
                (should-not (eq parent thread))
                (should (equal jabber-message-thread-id "topic"))
                (jabber-message-thread-set-title "Renamed thread")
                (should (string-match-p "Renamed thread" (buffer-name)))
                ;; Keep only the thread visible after native pop-to-buffer.
                (delete-other-windows)
                (jabber-test-activity-owners--deliver b peer type "Thread reply" "topic")
                (with-current-buffer thread
                  (should (string-match-p "Thread reply" (buffer-string))))
                (with-current-buffer parent
                  (should-not (string-match-p "Thread reply" (buffer-string))))
                (jabber-test-activity-owners--deliver b peer type "Parent message")
                (should (member (cons b peer) jabber-activity-jids))
                (jabber-activity-clean)
                (should (member (cons b peer) jabber-activity-jids))
                (call-interactively #'jabber-activity-switch-to)
                (should (eq (current-buffer) parent))
                (should-not jabber-activity-jids)
                (should (buffer-live-p thread))))
          (jabber-db-close)
          (delete-directory dir t))))))

(ert-deftest jabber-test-activity-owners-parent-thread-private-muc ()
  "Private MUC activity keeps the exact occupant and excludes thread views."
  (dolist (reverse '(nil t))
    (jabber-test-activity-owners--with-state
      (let* ((room "room@example.test")
             (peer (concat room "/sender"))
             (jabber-connections (if reverse (list b a) (list a b)))
             (parents
              (mapcar (lambda (jc)
                        (jabber-muc-join-set room jc "reader")
                        (cons jc (jabber-muc-private-create-buffer jc room "sender")))
                      jabber-connections))
             (parent (cdr (assq b parents)))
             (other (jabber-muc-private-create-buffer b room "other"))
             (thread (jabber-message-thread-create-buffer
                      b peer "chat" "private-topic" nil parent)))
        (switch-to-buffer thread)
        (jabber-process-chat
         b `(message ((from . ,peer) (type . "chat")) (body nil "Private message")))
        (with-current-buffer parent
          (should (string-match-p "Private message" (buffer-string))))
        (jabber-activity-clean)
        (should (equal jabber-activity-jids (list (cons b peer))))
        (call-interactively #'jabber-activity-switch-to)
        (should (eq (current-buffer) parent))
        (should jabber-muc-private-p)
        (should-not jabber-message-thread-id)
        (should-not (eq (current-buffer) other))
        (should-not jabber-activity-jids)))))

(provide 'jabber-test-activity-owners)
;;; jabber-test-activity-owners.el ends here
