;;; jabber-pubsub-tests.el --- Tests for jabber-pubsub  -*- lexical-binding: t; -*-

(require 'ert)

;; Pre-define variables that other modules expect at load time:
(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)
(defvar jabber-jid-obarray (make-vector 127 0))

(require 'jabber-pubsub)

;;; ---- Group 1: publish-options helper ----

(ert-deftest jabber-pubsub-test-publish-options-single ()
  "Single option produces correct XML structure."
  (let ((result (jabber-pubsub--publish-options
                 '(("pubsub#access_model" . "open")))))
    (should (eq (car result) 'publish-options))
    (let* ((x (nth 2 result))
           (fields (cl-remove-if-not
                    (lambda (c) (and (listp c) (eq (car c) 'field)))
                    (cddr x))))
      ;; FORM_TYPE + one option = 2 fields
      (should (= (length fields) 2))
      ;; First field is FORM_TYPE hidden
      (should (string= (cdr (assq 'var (cadr (nth 0 fields)))) "FORM_TYPE"))
      (should (string= (cdr (assq 'type (cadr (nth 0 fields)))) "hidden"))
      ;; Second field is our option
      (should (string= (cdr (assq 'var (cadr (nth 1 fields))))
                        "pubsub#access_model")))))

(ert-deftest jabber-pubsub-test-publish-options-multiple ()
  "Multiple options produce multiple fields."
  (let ((result (jabber-pubsub--publish-options
                 '(("pubsub#access_model" . "open")
                   ("pubsub#max_items" . "100")))))
    (let* ((x (nth 2 result))
           (fields (cl-remove-if-not
                    (lambda (c) (and (listp c) (eq (car c) 'field)))
                    (cddr x))))
      ;; FORM_TYPE + two options = 3 fields
      (should (= (length fields) 3)))))

(ert-deftest jabber-pubsub-test-publish-options-form-type ()
  "FORM_TYPE hidden field value is pubsub#publish-options."
  (let* ((result (jabber-pubsub--publish-options '(("foo" . "bar"))))
         (x (nth 2 result))
         (form-type-field (nth 2 x))
         (value-elem (nth 2 form-type-field)))
    (should (string= (nth 2 value-elem)
                      "http://jabber.org/protocol/pubsub#publish-options"))))

;;; ---- Group 2: event handler dispatch ----

(ert-deftest jabber-pubsub-test-dispatch-matching-node ()
  "Dispatches to registered handler for matching node."
  (let ((jabber-pubsub-node-handlers nil)
        (called-with nil))
    (push (cons "urn:xmpp:omemo:2:bundles"
                (lambda (_jc from node items)
                  (setq called-with (list from node items))))
          jabber-pubsub-node-handlers)
    (jabber-pubsub--process-event
     nil
     `(message ((from . "alice@example.com"))
               (event ((xmlns . ,jabber-pubsub-event-xmlns))
                      (items ((node . "urn:xmpp:omemo:2:bundles"))
                             (item ((id . "1")) (bundle () "data"))))))
    (should called-with)
    (should (string= (nth 0 called-with) "alice@example.com"))
    (should (string= (nth 1 called-with) "urn:xmpp:omemo:2:bundles"))))

(ert-deftest jabber-pubsub-test-dispatch-no-event ()
  "Ignores messages without <event> element."
  (let ((jabber-pubsub-node-handlers
         (list (cons "some-node" (lambda (&rest _) (error "Should not be called")))))
        (result nil))
    (jabber-pubsub--process-event
     nil
     '(message ((from . "alice@example.com"))
               (body () "hello")))
    (should-not result)))

(ert-deftest jabber-pubsub-test-dispatch-wrong-xmlns ()
  "Ignores events with wrong xmlns."
  (let ((jabber-pubsub-node-handlers
         (list (cons "some-node" (lambda (&rest _) (error "Should not be called"))))))
    (jabber-pubsub--process-event
     nil
     '(message ((from . "alice@example.com"))
               (event ((xmlns . "urn:wrong:xmlns"))
                      (items ((node . "some-node"))
                             (item ((id . "1")) (data () "x"))))))))

(ert-deftest jabber-pubsub-test-dispatch-unregistered-node ()
  "Ignores events with unregistered node."
  (let ((jabber-pubsub-node-handlers nil))
    ;; Should not error
    (jabber-pubsub--process-event
     nil
     `(message ((from . "alice@example.com"))
               (event ((xmlns . ,jabber-pubsub-event-xmlns))
                      (items ((node . "unknown:node"))
                             (item ((id . "1")) (data () "x"))))))))

(ert-deftest jabber-pubsub-test-dispatch-purge ()
  "Handles <purge> events."
  (let ((jabber-pubsub-node-handlers nil)
        (called-with nil))
    (push (cons "urn:xmpp:omemo:2:bundles"
                (lambda (_jc from node items)
                  (setq called-with (list from node items))))
          jabber-pubsub-node-handlers)
    (jabber-pubsub--process-event
     nil
     `(message ((from . "alice@example.com"))
               (event ((xmlns . ,jabber-pubsub-event-xmlns))
                      (purge ((node . "urn:xmpp:omemo:2:bundles"))))))
    (should called-with)
    (should (string= (nth 1 called-with) "urn:xmpp:omemo:2:bundles"))))

;;; ---- Group 3: IQ XML structure ----

(defvar jabber-pubsub-test--captured-args nil
  "Captured arguments from mocked `jabber-send-iq'.")

(defmacro jabber-pubsub-test-with-mock-iq (&rest body)
  "Execute BODY with `jabber-send-iq' mocked to capture its arguments."
  `(let ((jabber-pubsub-test--captured-args nil))
     (cl-letf (((symbol-function 'jabber-send-iq)
                (lambda (jc to type query &rest _rest)
                  (setq jabber-pubsub-test--captured-args
                        (list :jc jc :to to :type type :query query)))))
       ,@body)))

(ert-deftest jabber-pubsub-test-publish-iq-structure ()
  "Publish builds correct IQ set with pubsub xmlns."
  (jabber-pubsub-test-with-mock-iq
   (jabber-pubsub-publish 'fake-jc "pubsub.example.com" "mynode" "item1"
                          '(payload () "data"))
   (let ((args jabber-pubsub-test--captured-args))
     (should (equal (plist-get args :to) "pubsub.example.com"))
     (should (string= (plist-get args :type) "set"))
     (let ((query (plist-get args :query)))
       (should (eq (car query) 'pubsub))
       (should (string= (cdr (assq 'xmlns (cadr query))) jabber-pubsub-xmlns))
       ;; Check publish element
       (let ((publish (nth 2 query)))
         (should (eq (car publish) 'publish))
         (should (string= (cdr (assq 'node (cadr publish))) "mynode"))
         ;; Check item
         (let ((item (nth 2 publish)))
           (should (eq (car item) 'item))
           (should (string= (cdr (assq 'id (cadr item))) "item1"))))))))

(ert-deftest jabber-pubsub-test-publish-with-options ()
  "Publish with options includes <publish-options>."
  (jabber-pubsub-test-with-mock-iq
   (jabber-pubsub-publish 'fake-jc nil "mynode" "item1"
                          '(payload () "data")
                          '(("pubsub#access_model" . "open")))
   (let* ((query (plist-get jabber-pubsub-test--captured-args :query))
          (children (cddr query))
          (pub-opts (cl-find 'publish-options children :key #'car)))
     (should pub-opts))))

(ert-deftest jabber-pubsub-test-retract-iq-structure ()
  "Retract builds correct IQ set."
  (jabber-pubsub-test-with-mock-iq
   (jabber-pubsub-retract 'fake-jc "pubsub.example.com" "mynode" "item1")
   (let* ((args jabber-pubsub-test--captured-args)
          (query (plist-get args :query)))
     (should (string= (plist-get args :type) "set"))
     (should (eq (car query) 'pubsub))
     (let ((retract (nth 2 query)))
       (should (eq (car retract) 'retract))
       (should (string= (cdr (assq 'node (cadr retract))) "mynode"))
       (let ((item (nth 2 retract)))
         (should (string= (cdr (assq 'id (cadr item))) "item1")))))))

(ert-deftest jabber-pubsub-test-request-iq-structure ()
  "Request items builds correct IQ get."
  (jabber-pubsub-test-with-mock-iq
   (jabber-pubsub-request 'fake-jc "pubsub.example.com" "mynode" #'ignore)
   (let* ((args jabber-pubsub-test--captured-args)
          (query (plist-get args :query)))
     (should (string= (plist-get args :type) "get"))
     (let ((items (nth 2 query)))
       (should (eq (car items) 'items))
       (should (string= (cdr (assq 'node (cadr items))) "mynode"))))))

(ert-deftest jabber-pubsub-test-delete-node-iq-structure ()
  "Delete node builds correct IQ set with owner xmlns."
  (jabber-pubsub-test-with-mock-iq
   (jabber-pubsub-delete-node 'fake-jc "pubsub.example.com" "mynode")
   (let* ((args jabber-pubsub-test--captured-args)
          (query (plist-get args :query)))
     (should (string= (plist-get args :type) "set"))
     (should (string= (cdr (assq 'xmlns (cadr query)))
                       jabber-pubsub-owner-xmlns))
     (let ((delete (nth 2 query)))
       (should (eq (car delete) 'delete))
       (should (string= (cdr (assq 'node (cadr delete))) "mynode"))))))

(ert-deftest jabber-pubsub-test-configure-node-iq-structure ()
  "Configure node builds correct IQ set with data form."
  (jabber-pubsub-test-with-mock-iq
   (jabber-pubsub-configure-node 'fake-jc "pubsub.example.com" "mynode"
                                 '(("pubsub#access_model" . "open")))
   (let* ((args jabber-pubsub-test--captured-args)
          (query (plist-get args :query)))
     (should (string= (plist-get args :type) "set"))
     (should (string= (cdr (assq 'xmlns (cadr query)))
                       jabber-pubsub-owner-xmlns))
     (let* ((configure (nth 2 query))
            (x-form (nth 2 configure))
            (fields (cl-remove-if-not
                     (lambda (c) (and (listp c) (eq (car c) 'field)))
                     (cddr x-form))))
       (should (eq (car configure) 'configure))
       (should (string= (cdr (assq 'node (cadr configure))) "mynode"))
       ;; FORM_TYPE + 1 option
       (should (= (length fields) 2))
       ;; Check FORM_TYPE value
       (let* ((ft-field (car fields))
              (ft-value (nth 2 (nth 2 ft-field))))
         (should (string= ft-value
                           "http://jabber.org/protocol/pubsub#node_config")))))))

(provide 'jabber-pubsub-tests)
;;; jabber-pubsub-tests.el ends here
