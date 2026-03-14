;;; jabber-bookmarks-tests.el --- Tests for jabber-bookmarks  -*- lexical-binding: t; -*-

(require 'ert)

;; Pre-define variables that other modules expect at load time:
(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)
(defvar jabber-jid-obarray (make-vector 127 0))

(require 'jabber-bookmarks)

;;; Group 1: Parse XEP-0402 items

(ert-deftest jabber-bookmarks2-test-parse-full ()
  "Parse conference item with all fields."
  (let* ((item '(item ((id . "room@conference.example.com"))
                       (conference ((xmlns . "urn:xmpp:bookmarks:1")
                                    (name . "The Room")
                                    (autojoin . "true"))
                                   (nick () "MyNick")
                                   (password () "secret"))))
         (result (jabber-bookmarks2--parse-item item)))
    (should (string= (plist-get result :jid) "room@conference.example.com"))
    (should (string= (plist-get result :name) "The Room"))
    (should (plist-get result :autojoin))
    (should (string= (plist-get result :nick) "MyNick"))
    (should (string= (plist-get result :password) "secret"))))

(ert-deftest jabber-bookmarks2-test-parse-minimal ()
  "Parse conference item with only JID (no name, nick, password)."
  (let* ((item '(item ((id . "room@conference.example.com"))
                       (conference ((xmlns . "urn:xmpp:bookmarks:1")))))
         (result (jabber-bookmarks2--parse-item item)))
    (should (string= (plist-get result :jid) "room@conference.example.com"))
    (should-not (plist-get result :name))
    (should-not (plist-get result :autojoin))
    (should-not (plist-get result :nick))
    (should-not (plist-get result :password))))

(ert-deftest jabber-bookmarks2-test-parse-autojoin-variants ()
  "Parse autojoin attribute: \"true\", \"1\", and absent."
  (let ((make-item (lambda (val)
                     `(item ((id . "r@c.example.com"))
                            (conference ((xmlns . "urn:xmpp:bookmarks:1")
                                         ,@(when val `((autojoin . ,val)))))))))
    (should (plist-get (jabber-bookmarks2--parse-item
                        (funcall make-item "true"))
                       :autojoin))
    (should (plist-get (jabber-bookmarks2--parse-item
                        (funcall make-item "1"))
                       :autojoin))
    (should-not (plist-get (jabber-bookmarks2--parse-item
                            (funcall make-item nil))
                           :autojoin))
    (should-not (plist-get (jabber-bookmarks2--parse-item
                            (funcall make-item "false"))
                           :autojoin))))

(ert-deftest jabber-bookmarks2-test-parse-no-conference ()
  "Return nil when item has no <conference> child."
  (let ((item '(item ((id . "room@conference.example.com"))
                      (something-else ()))))
    (should-not (jabber-bookmarks2--parse-item item))))

;;; Group 2: Build conference XML

(ert-deftest jabber-bookmarks2-test-build-full ()
  "Build conference element with all fields."
  (let ((elem (jabber-bookmarks2--build-conference
               '(:jid "room@c.example.com"
                 :name "Room" :autojoin t :nick "Me" :password "pw"))))
    (should (eq (car elem) 'conference))
    (should (string= (cdr (assq 'xmlns (cadr elem)))
                      "urn:xmpp:bookmarks:1"))
    (should (string= (cdr (assq 'name (cadr elem))) "Room"))
    (should (string= (cdr (assq 'autojoin (cadr elem))) "true"))
    ;; Check nick child
    (let ((nick (car (jabber-xml-get-children elem 'nick))))
      (should nick)
      (should (string= (car (jabber-xml-node-children nick)) "Me")))
    ;; Check password child
    (let ((pw (car (jabber-xml-get-children elem 'password))))
      (should pw)
      (should (string= (car (jabber-xml-node-children pw)) "pw")))))

(ert-deftest jabber-bookmarks2-test-build-minimal ()
  "Build conference element with JID only."
  (let ((elem (jabber-bookmarks2--build-conference
               '(:jid "room@c.example.com"))))
    (should (eq (car elem) 'conference))
    (should (string= (cdr (assq 'xmlns (cadr elem)))
                      "urn:xmpp:bookmarks:1"))
    (should-not (assq 'name (cadr elem)))
    (should-not (assq 'autojoin (cadr elem)))
    (should-not (jabber-xml-get-children elem 'nick))
    (should-not (jabber-xml-get-children elem 'password))))

(ert-deftest jabber-bookmarks2-test-build-autojoin-false ()
  "Autojoin nil omits the attribute entirely."
  (let ((elem (jabber-bookmarks2--build-conference
               '(:jid "r@c.example.com" :autojoin nil))))
    (should-not (assq 'autojoin (cadr elem)))))

(ert-deftest jabber-bookmarks2-test-roundtrip ()
  "Build then parse returns equivalent plist."
  (let* ((original '(:jid "room@c.example.com"
                     :name "Room" :autojoin t :nick "Me" :password "pw"))
         (elem (jabber-bookmarks2--build-conference original))
         (item `(item ((id . "room@c.example.com")) ,elem))
         (parsed (jabber-bookmarks2--parse-item item)))
    (should (string= (plist-get parsed :jid) "room@c.example.com"))
    (should (string= (plist-get parsed :name) "Room"))
    (should (plist-get parsed :autojoin))
    (should (string= (plist-get parsed :nick) "Me"))
    (should (string= (plist-get parsed :password) "pw"))))

;;; Group 3: Parse XEP-0048 items (regression)

(ert-deftest jabber-bookmarks-test-parse-0048 ()
  "Verify jabber-parse-conference-bookmark still works for XEP-0048."
  (let* ((node '(conference ((jid . "room@conference.example.com")
                             (name . "Old Room")
                             (autojoin . "true"))
                            (nick () "OldNick")
                            (password () "oldpw")))
         (result (jabber-parse-conference-bookmark node)))
    (should (string= (plist-get result :jid) "room@conference.example.com"))
    (should (string= (plist-get result :name) "Old Room"))
    (should (plist-get result :autojoin))
    (should (string= (plist-get result :nick) "OldNick"))
    (should (string= (plist-get result :password) "oldpw"))))

(ert-deftest jabber-bookmarks-test-parse-0048-minimal ()
  "XEP-0048 conference with no optional fields."
  (let* ((node '(conference ((jid . "room@conference.example.com"))))
         (result (jabber-parse-conference-bookmark node)))
    (should (string= (plist-get result :jid) "room@conference.example.com"))
    (should-not (plist-get result :autojoin))
    (should-not (plist-get result :nick))))

(ert-deftest jabber-bookmarks-test-parse-non-conference ()
  "jabber-parse-conference-bookmark returns nil for non-conference nodes."
  (should-not (jabber-parse-conference-bookmark
               '(url ((url . "http://example.com") (name . "Test"))))))

;;; Group 4: Fetch and cache

(defun jabber-bookmarks-test--fake-jc ()
  "Create a fake connection symbol with bare JID."
  (let ((jc (gensym "test-jc-")))
    (put jc :state-data '(:username "user" :server "example.com"))
    jc))

(defun jabber-bookmarks-test--bare-jid (jc)
  "Return bare JID for fake JC."
  (let ((data (get jc :state-data)))
    (concat (plist-get data :username) "@" (plist-get data :server))))

(ert-deftest jabber-bookmarks2-test-handle-fetch ()
  "PubSub response is parsed into plists and cached."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (jc (jabber-bookmarks-test--fake-jc))
        (result nil))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (j) (jabber-bookmarks-test--bare-jid j))))
      (jabber-bookmarks2--handle-fetch
       jc
       `(iq ((type . "result"))
            (pubsub ((xmlns . ,jabber-pubsub-xmlns))
                    (items ((node . ,jabber-bookmarks2-xmlns))
                           (item ((id . "room1@c.example.com"))
                                 (conference ((xmlns . ,jabber-bookmarks2-xmlns)
                                              (name . "Room 1")
                                              (autojoin . "true"))
                                             (nick () "Me")))
                           (item ((id . "room2@c.example.com"))
                                 (conference ((xmlns . ,jabber-bookmarks2-xmlns)))))))
       (lambda (_jc bookmarks) (setq result bookmarks))))
    ;; Two bookmarks parsed
    (should (= 2 (length result)))
    (should (string= (plist-get (nth 0 result) :jid) "room1@c.example.com"))
    (should (plist-get (nth 0 result) :autojoin))
    (should (string= (plist-get (nth 0 result) :nick) "Me"))
    (should (string= (plist-get (nth 1 result) :jid) "room2@c.example.com"))
    ;; Cached
    (let ((cached (gethash "user@example.com" jabber-bookmarks)))
      (should (listp cached))
      (should (= 2 (length cached))))))

(ert-deftest jabber-bookmarks2-test-handle-fetch-empty ()
  "Empty PubSub response caches t."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (jc (jabber-bookmarks-test--fake-jc))
        (result 'not-called))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (j) (jabber-bookmarks-test--bare-jid j))))
      (jabber-bookmarks2--handle-fetch
       jc
       `(iq ((type . "result"))
            (pubsub ((xmlns . ,jabber-pubsub-xmlns))
                    (items ((node . ,jabber-bookmarks2-xmlns)))))
       (lambda (_jc bookmarks) (setq result bookmarks))))
    (should (null result))
    (should (eq t (gethash "user@example.com" jabber-bookmarks)))))

(ert-deftest jabber-bookmarks-test-handle-legacy ()
  "XEP-0049 storage response is parsed to plists and cached."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (jc (jabber-bookmarks-test--fake-jc))
        (result nil))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (j) (jabber-bookmarks-test--bare-jid j))))
      (jabber-bookmarks--handle-legacy
       jc
       '(storage ((xmlns . "storage:bookmarks"))
                 (conference ((jid . "room@c.example.com")
                              (name . "Room")
                              (autojoin . "1"))
                             (nick () "Nick"))
                 (url ((url . "http://example.com") (name . "Site"))))
       (lambda (_jc bookmarks) (setq result bookmarks))))
    ;; Only conference is kept (url is discarded)
    (should (= 1 (length result)))
    (should (string= (plist-get (car result) :jid) "room@c.example.com"))
    (should (string= (plist-get (car result) :nick) "Nick"))))

(ert-deftest jabber-bookmarks-test-get-uses-cache ()
  "jabber-get-bookmarks returns cached data without fetching."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (jc (jabber-bookmarks-test--fake-jc))
        (result nil)
        (fetched nil))
    (puthash "user@example.com"
             (list '(:jid "cached@c.example.com" :name "Cached"))
             jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (j) (jabber-bookmarks-test--bare-jid j)))
              ((symbol-function 'jabber-pubsub-request)
               (lambda (&rest _) (setq fetched t)))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat fn &rest args) (apply fn args))))
      (jabber-get-bookmarks jc (lambda (_jc bms) (setq result bms)))
      (should-not fetched)
      (should (= 1 (length result)))
      (should (string= (plist-get (car result) :jid) "cached@c.example.com")))))

(ert-deftest jabber-bookmarks-test-get-conference-data-from-cache ()
  "jabber-get-conference-data-internal finds by JID in plist cache."
  (let ((cache '((:jid "room1@c.example.com" :name "Room 1" :nick "A")
                 (:jid "room2@c.example.com" :name "Room 2" :nick "B"))))
    (should (string= (jabber-get-conference-data-internal
                       cache "room2@c.example.com" :nick)
                      "B"))
    (should (string= (plist-get
                       (jabber-get-conference-data-internal
                        cache "room1@c.example.com" nil)
                       :name)
                      "Room 1"))
    (should-not (jabber-get-conference-data-internal
                  cache "unknown@c.example.com" nil))))

;;; Group 5: Publish and retract IQ structure

(defvar jabber-bookmarks-test--iq-calls nil
  "List of captured jabber-send-iq calls.")

(defmacro jabber-bookmarks-test-with-mock-iq (&rest body)
  "Execute BODY with `jabber-send-iq' mocked to capture calls."
  `(let ((jabber-bookmarks-test--iq-calls nil))
     (cl-letf (((symbol-function 'jabber-send-iq)
                (lambda (jc to type query
                         &optional success-cb success-data error-cb error-data
                         &rest _)
                  (push (list :jc jc :to to :type type :query query
                              :success-cb success-cb :error-cb error-cb)
                        jabber-bookmarks-test--iq-calls))))
       ,@body)))

(ert-deftest jabber-bookmarks2-test-publish-iq ()
  "Publish sends correct PubSub IQ with publish-options."
  (jabber-bookmarks-test-with-mock-iq
   (jabber-bookmarks2--publish
    'fake-jc '(:jid "room@c.example.com" :name "Room" :autojoin t :nick "Me"))
   (should (= 1 (length jabber-bookmarks-test--iq-calls)))
   (let* ((call (car jabber-bookmarks-test--iq-calls))
          (query (plist-get call :query))
          (publish (nth 2 query))
          (item (nth 2 publish)))
     (should (eq (car query) 'pubsub))
     (should (eq (car publish) 'publish))
     (should (string= (cdr (assq 'node (cadr publish)))
                       jabber-bookmarks2-xmlns))
     (should (string= (cdr (assq 'id (cadr item)))
                       "room@c.example.com"))
     ;; Has publish-options
     (let ((pub-opts (cl-find 'publish-options (cddr query) :key #'car)))
       (should pub-opts)))))

(ert-deftest jabber-bookmarks2-test-retract-iq ()
  "Retract sends correct PubSub IQ with notify."
  (jabber-bookmarks-test-with-mock-iq
   (jabber-bookmarks2--retract 'fake-jc "room@c.example.com")
   (should (= 1 (length jabber-bookmarks-test--iq-calls)))
   (let* ((call (car jabber-bookmarks-test--iq-calls))
          (query (plist-get call :query))
          (retract (nth 2 query))
          (item (nth 2 retract)))
     (should (string= (cdr (assq 'notify (cadr retract))) "true"))
     (should (string= (cdr (assq 'id (cadr item)))
                       "room@c.example.com")))))

;;; Group 6: set-bookmarks diff logic

(ert-deftest jabber-bookmarks-test-set-publishes-and-retracts ()
  "set-bookmarks publishes new/changed and retracts removed."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (published nil)
        (retracted nil)
        (cb-result nil))
    ;; Old cache: room1 and room2
    (puthash "user@example.com"
             '((:jid "room1@c.example.com" :name "Room 1")
               (:jid "room2@c.example.com" :name "Room 2"))
             jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com"))
              ((symbol-function 'jabber-bookmarks2--publish)
               (lambda (_jc plist &optional cb _ecb)
                 (push (plist-get plist :jid) published)
                 (when cb (funcall cb _jc nil nil))))
              ((symbol-function 'jabber-bookmarks2--retract)
               (lambda (_jc jid &optional cb _ecb)
                 (push jid retracted)
                 (when cb (funcall cb _jc nil nil)))))
      ;; New: keep room1, add room3, drop room2
      (jabber-set-bookmarks
       'fake-jc
       '((:jid "room1@c.example.com" :name "Room 1")
         (:jid "room3@c.example.com" :name "Room 3"))
       (lambda (_jc ok) (setq cb-result ok))))
    (should (member "room1@c.example.com" published))
    (should (member "room3@c.example.com" published))
    (should (equal retracted '("room2@c.example.com")))
    (should cb-result)))

(ert-deftest jabber-bookmarks-test-set-empty-to-empty ()
  "set-bookmarks with no old and no new succeeds immediately."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (cb-result nil))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com")))
      (jabber-set-bookmarks
       'fake-jc nil
       (lambda (_jc ok) (setq cb-result ok))))
    (should cb-result)))

(ert-deftest jabber-bookmarks-test-set-falls-back-on-error ()
  "set-bookmarks falls back to XEP-0049 on PubSub error."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (legacy-called nil))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com"))
              ((symbol-function 'jabber-bookmarks2--publish)
               (lambda (_jc _plist &optional _cb ecb)
                 ;; Simulate PubSub error
                 (when ecb (funcall ecb _jc nil nil))))
              ((symbol-function 'jabber-bookmarks--set-legacy)
               (lambda (_jc _bms &optional _cb)
                 (setq legacy-called t))))
      (jabber-set-bookmarks
       'fake-jc
       '((:jid "room@c.example.com" :name "Room"))
       #'ignore))
    (should legacy-called)))

;;; Group 7: Event handler (live sync)

(ert-deftest jabber-bookmarks2-test-event-item-autojoin ()
  "New item with autojoin=true updates cache and joins."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (joined nil))
    (puthash "user@example.com" t jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com"))
              ((symbol-function 'jabber-muc-joined-p)
               (lambda (_g) nil))
              ((symbol-function 'jabber-muc-join)
               (lambda (_jc group _nick &optional _popup)
                 (push group joined)))
              ((symbol-function 'fsm-get-state-data)
               (lambda (_jc) '(:username "user"))))
      (jabber-bookmarks2--handle-event
       'fake-jc nil nil
       `((item ((id . "room@c.example.com"))
               (conference ((xmlns . ,jabber-bookmarks2-xmlns)
                            (autojoin . "true"))
                           (nick () "Me"))))))
    (should (member "room@c.example.com" joined))
    (let ((cached (gethash "user@example.com" jabber-bookmarks)))
      (should (= 1 (length cached)))
      (should (string= (plist-get (car cached) :jid) "room@c.example.com")))))

(ert-deftest jabber-bookmarks2-test-event-item-no-autojoin-leaves ()
  "Item without autojoin leaves if currently joined."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (left nil))
    (puthash "user@example.com"
             '((:jid "room@c.example.com" :autojoin t))
             jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com"))
              ((symbol-function 'jabber-muc-joined-p)
               (lambda (_g) t))
              ((symbol-function 'jabber-muc-get-buffer)
               (lambda (_g &optional _jc) nil))
              ((symbol-function 'jabber-muc-leave)
               (lambda (_jc group) (push group left))))
      (jabber-bookmarks2--handle-event
       'fake-jc nil nil
       `((item ((id . "room@c.example.com"))
               (conference ((xmlns . ,jabber-bookmarks2-xmlns)))))))
    (should (member "room@c.example.com" left))))

(ert-deftest jabber-bookmarks2-test-event-retract ()
  "Retract removes from cache and leaves."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (left nil))
    (puthash "user@example.com"
             '((:jid "room@c.example.com" :name "Room"))
             jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com"))
              ((symbol-function 'jabber-muc-joined-p)
               (lambda (_g) t))
              ((symbol-function 'jabber-muc-get-buffer)
               (lambda (_g &optional _jc) nil))
              ((symbol-function 'jabber-muc-leave)
               (lambda (_jc group) (push group left))))
      (jabber-bookmarks2--handle-event
       'fake-jc nil nil
       '((retract ((id . "room@c.example.com"))))))
    (should (member "room@c.example.com" left))
    ;; Cache should be t (empty)
    (should (eq t (gethash "user@example.com" jabber-bookmarks)))))

(ert-deftest jabber-bookmarks2-test-event-already-joined-skips ()
  "Item with autojoin for already-joined room does not re-join."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (join-count 0))
    (puthash "user@example.com" t jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com"))
              ((symbol-function 'jabber-muc-joined-p)
               (lambda (_g) t))
              ((symbol-function 'jabber-muc-join)
               (lambda (&rest _) (cl-incf join-count))))
      (jabber-bookmarks2--handle-event
       'fake-jc nil nil
       `((item ((id . "room@c.example.com"))
               (conference ((xmlns . ,jabber-bookmarks2-xmlns)
                            (autojoin . "true")))))))
    (should (= 0 join-count))))

(ert-deftest jabber-bookmarks2-test-event-retract-not-joined-skips ()
  "Retract for not-joined room only updates cache."
  (let ((jabber-bookmarks (make-hash-table :test 'equal))
        (leave-count 0))
    (puthash "user@example.com"
             '((:jid "room@c.example.com"))
             jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com"))
              ((symbol-function 'jabber-muc-joined-p)
               (lambda (_g) nil))
              ((symbol-function 'jabber-muc-leave)
               (lambda (&rest _) (cl-incf leave-count))))
      (jabber-bookmarks2--handle-event
       'fake-jc nil nil
       '((retract ((id . "room@c.example.com"))))))
    (should (= 0 leave-count))
    (should (eq t (gethash "user@example.com" jabber-bookmarks)))))

;;; Group 8: Cache management

(ert-deftest jabber-bookmarks2-test-update-cache-new ()
  "Update cache adds a new bookmark."
  (let ((jabber-bookmarks (make-hash-table :test 'equal)))
    (puthash "user@example.com" t jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com")))
      (jabber-bookmarks2--update-cache
       'fake-jc '(:jid "room@c.example.com" :name "Room")))
    (let ((cached (gethash "user@example.com" jabber-bookmarks)))
      (should (= 1 (length cached)))
      (should (string= (plist-get (car cached) :jid) "room@c.example.com")))))

(ert-deftest jabber-bookmarks2-test-update-cache-replace ()
  "Update cache replaces existing entry with same JID."
  (let ((jabber-bookmarks (make-hash-table :test 'equal)))
    (puthash "user@example.com"
             '((:jid "room@c.example.com" :name "Old"))
             jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com")))
      (jabber-bookmarks2--update-cache
       'fake-jc '(:jid "room@c.example.com" :name "New")))
    (let ((cached (gethash "user@example.com" jabber-bookmarks)))
      (should (= 1 (length cached)))
      (should (string= (plist-get (car cached) :name) "New")))))

(ert-deftest jabber-bookmarks2-test-remove-from-cache ()
  "Remove from cache drops the entry."
  (let ((jabber-bookmarks (make-hash-table :test 'equal)))
    (puthash "user@example.com"
             '((:jid "room1@c.example.com") (:jid "room2@c.example.com"))
             jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com")))
      (jabber-bookmarks2--remove-from-cache 'fake-jc "room1@c.example.com"))
    (let ((cached (gethash "user@example.com" jabber-bookmarks)))
      (should (= 1 (length cached)))
      (should (string= (plist-get (car cached) :jid) "room2@c.example.com")))))

(ert-deftest jabber-bookmarks2-test-remove-last-caches-t ()
  "Removing the last bookmark caches t."
  (let ((jabber-bookmarks (make-hash-table :test 'equal)))
    (puthash "user@example.com"
             '((:jid "room@c.example.com"))
             jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_j) "user@example.com")))
      (jabber-bookmarks2--remove-from-cache 'fake-jc "room@c.example.com"))
    (should (eq t (gethash "user@example.com" jabber-bookmarks)))))

;;; Group 9: Tabulated-list entries

(ert-deftest jabber-bookmarks-test-entries-full ()
  "Entries builds correct vectors from cache."
  (let ((jabber-bookmarks (make-hash-table :test 'equal)))
    (puthash "user@example.com"
             '((:jid "room@c.example.com" :name "Room"
                :autojoin t :nick "Me" :password "secret"))
             jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (j) (jabber-bookmarks-test--bare-jid j))))
      (with-temp-buffer
        (setq-local jabber-buffer-connection
                    (jabber-bookmarks-test--fake-jc))
        (let ((entries (jabber-bookmarks--entries)))
          (should (= 1 (length entries)))
          (let ((entry (car entries)))
            (should (string= (car entry) "room@c.example.com"))
            (let ((cols (cadr entry)))
              (should (string= (aref cols 0) "room@c.example.com"))
              (should (string= (aref cols 1) "Room"))
              (should (string= (aref cols 2) "true"))
              (should (string= (aref cols 3) "Me"))
              (should (string= (aref cols 4) "***")))))))))

(ert-deftest jabber-bookmarks-test-entries-minimal ()
  "Entries handles missing optional fields."
  (let ((jabber-bookmarks (make-hash-table :test 'equal)))
    (puthash "user@example.com"
             '((:jid "room@c.example.com"))
             jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (j) (jabber-bookmarks-test--bare-jid j))))
      (with-temp-buffer
        (setq-local jabber-buffer-connection
                    (jabber-bookmarks-test--fake-jc))
        (let* ((entries (jabber-bookmarks--entries))
               (cols (cadr (car entries))))
          (should (string= (aref cols 1) ""))
          (should (string= (aref cols 2) "false"))
          (should (string= (aref cols 3) ""))
          (should (string= (aref cols 4) "")))))))

(ert-deftest jabber-bookmarks-test-entries-empty ()
  "Entries returns nil when cache is empty (t)."
  (let ((jabber-bookmarks (make-hash-table :test 'equal)))
    (puthash "user@example.com" t jabber-bookmarks)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (j) (jabber-bookmarks-test--bare-jid j))))
      (with-temp-buffer
        (setq-local jabber-buffer-connection
                    (jabber-bookmarks-test--fake-jc))
        (should-not (jabber-bookmarks--entries))))))

(provide 'jabber-bookmarks-tests)

;;; jabber-bookmarks-tests.el ends here
