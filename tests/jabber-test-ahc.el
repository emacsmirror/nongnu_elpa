;;; jabber-test-ahc.el --- Tests for Jabber ad-hoc commands  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0050 command discovery and form submission.

;;; Code:

(require 'ert)

(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)
(defvar jabber-jid-obarray (make-vector 127 0))

(require 'jabber-ahc)

(ert-deftest jabber-test-ahc-get-list-uses-command-node ()
  "Command discovery uses the XEP-0050 disco node and a focused callback."
  (let (arguments)
    (cl-letf (((symbol-function 'jabber-disco-get-items)
               (lambda (&rest args) (setq arguments args))))
      (with-temp-buffer
        (jabber-ahc-get-list 'fake-jc "commands.example.org")))
    (should (eq (nth 0 arguments) 'fake-jc))
    (should (equal (nth 1 arguments) "commands.example.org"))
    (should (equal (nth 2 arguments) jabber-ahc-xmlns))
    (should (eq (nth 3 arguments) #'jabber-ahc--command-list-result))
    (should (bufferp (car (nth 4 arguments))))))

(ert-deftest jabber-test-ahc-command-list-executes-item-directly ()
  "Choosing a discovered command passes its JID and node without prompts."
  (let ((jabber-connections '(fake-jc))
        shown-map executed)
    (cl-letf (((symbol-function 'keymap-popup)
               (lambda (map) (setq shown-map map)))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'jabber-connection-active-p) #'identity)
              ((symbol-function 'jabber-ahc-execute-command)
               (lambda (jc to node)
                 (setq executed (list jc to node)))))
      (with-temp-buffer
        (jabber-ahc--command-list-result
         'fake-jc
         (list (current-buffer) "commands.example.org")
         (list ["Restart service" "admin.example.org" "restart"]))
        (should (keymapp shown-map))
        (call-interactively (keymap-lookup shown-map "1"))))
    (should (equal executed
                   '(fake-jc "admin.example.org" "restart")))))

(ert-deftest jabber-test-ahc-command-list-keeps-item-specific-data ()
  "Each discovered command keeps its own JID and node."
  (let ((jabber-connections '(fake-jc))
        executed)
    (cl-letf (((symbol-function 'jabber-connection-active-p) #'identity)
              ((symbol-function 'jabber-ahc-execute-command)
               (lambda (jc to node)
                 (push (list jc to node) executed))))
      (let ((map
             (jabber-ahc--command-list-map
              'fake-jc "commands.example.org"
              (list ["First" "one.example.org" "node-1"]
                    ["Second" "two.example.org" "node-2"]))))
        (call-interactively (keymap-lookup map "1"))
        (call-interactively (keymap-lookup map "2"))))
    (should (equal (nreverse executed)
                   '((fake-jc "one.example.org" "node-1")
                     (fake-jc "two.example.org" "node-2"))))))

(ert-deftest jabber-test-ahc-command-list-resolves-live-connection ()
  "Command activation resolves a replacement for a stale connection."
  (let ((jabber-connections '(live-jc))
        executed)
    (cl-letf (((symbol-function 'jabber-find-active-connection)
               (lambda (jc)
                 (should (eq jc 'dead-jc))
                 'live-jc))
              ((symbol-function 'jabber-connection-active-p)
               (lambda (jc) (eq jc 'live-jc)))
              ((symbol-function 'jabber-ahc-execute-command)
               (lambda (jc _to _node) (setq executed jc))))
      (let ((map
             (jabber-ahc--command-list-map
              'dead-jc "commands.example.org"
              (list ["First" "one.example.org" "node-1"]))))
        (call-interactively (keymap-lookup map "1"))))
    (should (eq executed 'live-jc))))

(ert-deftest jabber-test-ahc-command-list-pages-four-at-a-time ()
  "Discovered commands are divided into pages of at most four."
  (let ((items
         (cl-loop for number from 1 to 5
                  collect
                  (vector (format "Command %d" number)
                          "admin.example.org"
                          (format "node-%d" number)))))
    (with-temp-buffer
      (jabber-ahc--command-list-map
       'fake-jc "commands.example.org" items)
      (should (equal (jabber-ahc--command-description 0) "Command 1"))
      (should (jabber-ahc--command-has-next-page-p))
      (jabber-ahc-command-next-page)
      (should (equal (jabber-ahc--command-description 0) "Command 5"))
      (should-not (jabber-ahc--command-at 1))
      (should-not (jabber-ahc--command-has-next-page-p)))))

(ert-deftest jabber-test-ahc-display-opens-data-form-with-server-actions ()
  "An executing command uses the data-form menu and advertised actions."
  (let (shown-form shown-actions)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'jabber-xdata-form-open)
               (lambda (form actions)
                 (setq shown-form form
                       shown-actions actions))))
      (with-temp-buffer
        (jabber-ahc-display
         'fake-jc
         '(iq ((from . "admin.example.org") (type . "result"))
              (command ((xmlns . "http://jabber.org/protocol/commands")
                        (node . "restart") (sessionid . "session-1")
                        (status . "executing"))
                       (actions ((execute . "complete"))
                                (prev nil)
                                (complete nil))
                       (x ((xmlns . "jabber:x:data") (type . "form"))
                          (field ((var . "confirm") (type . "boolean"))
                                 (value nil "1"))))))))
    (should (equal (plist-get
                    (jabber-xdata-field shown-form "confirm") :values)
                   '("1")))
    (should (equal (mapcar (lambda (action)
                            (list (plist-get action :key)
                                  (plist-get action :label)))
                          shown-actions)
                   '(("p" "Prev") ("RET" "Complete") ("q" "Cancel"))))))

(ert-deftest jabber-test-ahc-overlapping-forms-keep-session-context ()
  "Submitting an older form uses its original XEP-0050 session."
  (let (opened sent)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'jabber-xdata-form-open)
               (lambda (_form actions)
                 (setq opened (append opened (list actions)))))
              ((symbol-function 'jabber-connection-active-p) #'identity)
              ((symbol-function 'jabber-send-iq)
               (lambda (jc to _type query &rest _ignore)
                 (setq sent (list jc to query)))))
      (with-temp-buffer
        (dolist (session '(("one.example.org" "node-1" "session-1")
                           ("two.example.org" "node-2" "session-2")))
          (jabber-ahc-display
           'fake-jc
           `(iq ((from . ,(nth 0 session)) (type . "result"))
                (command
                 ((xmlns . "http://jabber.org/protocol/commands")
                  (node . ,(nth 1 session))
                  (sessionid . ,(nth 2 session))
                  (status . "executing"))
                 (actions ((execute . "complete")) (complete nil))
                 (x ((xmlns . "jabber:x:data") (type . "form")))))))
      (with-temp-buffer
        (setq-local jabber-xdata-form--form '(:fields nil))
        (let ((jabber-connections '(fake-jc))
              (complete
               (seq-find
                (lambda (action)
                  (equal (plist-get action :label) "Complete"))
                (car opened))))
          (call-interactively (plist-get complete :command))))))
    (should (equal (nth 0 sent) 'fake-jc))
    (should (equal (nth 1 sent) "one.example.org"))
    (let ((command (nth 2 sent)))
      (should (equal (jabber-xml-get-attribute command 'node) "node-1"))
      (should (equal (jabber-xml-get-attribute command 'sessionid)
                     "session-1")))))

(ert-deftest jabber-test-ahc-result-does-not-enable-widget-ui ()
  "A read-only command result renders without widget buffer machinery."
  (with-temp-buffer
    (jabber-ahc-display
     'fake-jc
     '(iq ((from . "admin.example.org") (type . "result"))
          (command ((xmlns . "http://jabber.org/protocol/commands")
                    (node . "status") (sessionid . "session-1")
                    (status . "completed"))
                   (x ((xmlns . "jabber:x:data") (type . "result"))
                      (field ((var . "status") (label . "Status"))
                             (value nil "Running"))))))
    (should-not (bound-and-true-p widget-minor-mode))
    (should (string-match-p "Status:.*Running" (buffer-string)))))

(ert-deftest jabber-test-ahc-submit-sends-form-only-for-forward-actions ()
  "Complete submits the current form while cancel sends no form."
  (let (queries)
    (cl-letf (((symbol-function 'jabber-connection-active-p) #'identity)
              ((symbol-function 'jabber-send-iq)
               (lambda (_jc _to _type query &rest _ignore)
                 (push query queries))))
      (with-temp-buffer
        (setq-local jabber-buffer-connection 'fake-jc)
        (setq-local jabber-ahc--submit-to "admin.example.org")
        (setq-local jabber-ahc-sessionid "session-1")
        (setq-local jabber-ahc-node "restart")
        (setq-local jabber-ahc--has-form t)
        (setq-local jabber-xdata-form--form
                    (jabber-xdata-parse
                     '(x ((xmlns . "jabber:x:data") (type . "form"))
                         (field ((var . "FORM_TYPE") (type . "hidden"))
                                (value nil "urn:test")))))
        (let ((jabber-connections '(fake-jc)))
          (jabber-ahc-submit 'complete)
          (jabber-ahc-submit 'cancel))))
    (should (jabber-xml-get-children (cadr queries) 'x))
    (should-not (jabber-xml-get-children (car queries) 'x))))

(ert-deftest jabber-test-ahc-rejects-reconnecting-connection ()
  "Command activation and submission reject a reconnecting FSM."
  (let ((reconnecting (make-symbol "reconnecting"))
        jabber-connections called)
    (setq jabber-connections (list reconnecting))
    (put reconnecting :state :connecting)
    (cl-letf (((symbol-function 'jabber-ahc-execute-command)
               (lambda (&rest _ignore) (setq called t)))
              ((symbol-function 'jabber-send-iq)
               (lambda (&rest _ignore) (setq called t))))
      (let ((map
             (jabber-ahc--command-list-map
              reconnecting "commands.example.org"
              (list ["First" "one.example.org" "node-1"]))))
        (should-error (call-interactively (keymap-lookup map "1"))
                      :type 'user-error))
      (with-temp-buffer
        (setq-local jabber-buffer-connection reconnecting)
        (setq-local jabber-ahc--submit-to "one.example.org")
        (setq-local jabber-ahc--has-form nil)
        (should-error (jabber-ahc-submit 'cancel) :type 'user-error)))
    (should-not called)))

(ert-deftest jabber-test-ahc-actions-without-execute-have-no-default-key ()
  "An actions element without execute does not advertise a default button."
  (let ((actions (jabber-ahc--menu-actions
                  '(actions nil (next nil)) nil)))
    (should (equal (mapcar (lambda (action)
                            (plist-get action :key))
                          actions)
                   '("n" "q")))))

(provide 'jabber-test-ahc)
;;; jabber-test-ahc.el ends here
