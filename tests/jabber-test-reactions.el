;;; jabber-test-reactions.el --- Tests for jabber-reactions  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0444 Message Reactions helper tests.

;;; Code:

(require 'ert)
(require 'jabber-reactions)

;;; Group 1: Pure reaction state helpers

(ert-deftest jabber-test-reactions-deduplicate-filters-empty-and-duplicates ()
  "Deduplication keeps first non-empty string occurrences only."
  (should (equal (jabber-reactions--deduplicate
                  '(nil "" "👍" "" "🎉" "👍" nil "🎉" "❤️"))
                 '("👍" "🎉" "❤️"))))

(ert-deftest jabber-test-reactions-target-id-selects-chat-id ()
  "Direct chat reactions target the message :id."
  (should (equal (jabber-reactions--target-id '(:id "client-1"
                                                    :server-id "server-1")
                                              nil)
                 "client-1")))

(ert-deftest jabber-test-reactions-target-id-selects-muc-server-id ()
  "MUC reactions target the message :server-id."
  (should (equal (jabber-reactions--target-id '(:id "client-1"
                                                    :server-id "server-1")
                                              t)
                 "server-1")))

(ert-deftest jabber-test-reactions-display-entries-aggregate-counts-and-chosen ()
  "Display entries aggregate reactions and mark the chosen sender."
  (let ((entries (jabber-reactions--display-entries
                  '(("me@example.com" . ("👍" "🎉" "👍"))
                    ("you@example.com" . ("👍" "❤️"))
                    ("them@example.com" . ("🎉")))
                  "me@example.com")))
    (should (equal (mapcar (lambda (entry) (plist-get entry :reaction)) entries)
                   '("👍" "🎉" "❤️")))
    (let ((thumbs (cl-find "👍" entries
                           :key (lambda (entry) (plist-get entry :reaction))
                           :test #'equal))
          (party (cl-find "🎉" entries
                          :key (lambda (entry) (plist-get entry :reaction))
                          :test #'equal))
          (heart (cl-find "❤️" entries
                          :key (lambda (entry) (plist-get entry :reaction))
                          :test #'equal)))
      (should (= (plist-get thumbs :count) 2))
      (should (plist-get thumbs :chosen))
      (should (= (plist-get party :count) 2))
      (should (plist-get party :chosen))
      (should (= (plist-get heart :count) 1))
      (should-not (plist-get heart :chosen)))))

(ert-deftest jabber-test-reactions-toggle-reaction-adds-and-removes-from-full-list ()
  "Toggling updates the complete local reaction list."
  (should (equal (jabber-reactions--toggle-reaction "❤️" '("👍" "👍" "🎉"))
                 '("👍" "🎉" "❤️")))
  (should (equal (jabber-reactions--toggle-reaction "👍" '("👍" "🎉" "👍"))
                 '("🎉"))))

(ert-deftest jabber-test-reactions-update-message-replaces-sender-reactions ()
  "Incoming updates replace only the sender's full reaction list."
  (let* ((msg '(:id "m1" :reactions (("alice" . ("👍"))
                                     ("bob" . ("🎉")))))
         (updated (jabber-reactions--update-message msg "alice" '("❤️" "❤️" ""))))
    (should (equal (alist-get "alice" (plist-get updated :reactions) nil nil #'equal)
                   '("❤️")))
    (should (equal (alist-get "bob" (plist-get updated :reactions) nil nil #'equal)
                   '("🎉")))))

(ert-deftest jabber-test-reactions-update-message-removes-sender-on-empty-update ()
  "Incoming empty updates remove that sender from reaction state."
  (let* ((msg '(:id "m1" :reactions (("alice" . ("👍"))
                                     ("bob" . ("🎉")))))
         (updated (jabber-reactions--update-message msg "alice" nil)))
    (should-not (assoc "alice" (plist-get updated :reactions)))
    (should (equal (alist-get "bob" (plist-get updated :reactions) nil nil #'equal)
                   '("🎉")))))

;;; Group 2: Outgoing stanza helpers

(ert-deftest jabber-test-reactions-build-stanza-includes-store-hint ()
  "Outgoing reaction stanzas include the XEP-0334 store hint."
  (let* ((stanza (jabber-reactions--build-stanza
                  "juliet@example.com" "chat" "target-1" '("👍" "👍" "🎉")
                  "reaction-1"))
         (store (assq 'store (cddr stanza))))
    (should (equal (cdr (assq 'xmlns (cadr store)))
                   jabber-reactions-hints-xmlns))))

(ert-deftest jabber-test-reactions-build-stanza-keeps-empty-reactions-element ()
  "An empty full local reaction list sends an empty reactions element."
  (let* ((stanza (jabber-reactions--build-stanza
                  "room@example.com" "groupchat" "target-1" nil "reaction-1"))
         (reactions (assq 'reactions (cddr stanza))))
    (should reactions)
    (should (equal (cadr reactions)
                   `((xmlns . ,jabber-reactions-xmlns) (id . "target-1"))))
    (should-not (cddr reactions))))

;;; Group 3: Incoming stanza classification

(ert-deftest jabber-test-reactions-reaction-only-p-accepts-bodyless-update ()
  "A bodyless stanza with only reactions is reaction-only."
  (should (jabber-reactions--reaction-only-p
           `(message ((from . "juliet@example.com") (type . "chat"))
                     (reactions ((xmlns . ,jabber-reactions-xmlns)
                                 (id . "target-1"))
                                (reaction nil "👍"))))))

(ert-deftest jabber-test-reactions-reaction-only-p-rejects-body-subject-or-error ()
  "Reaction stanzas with body, subject, or error are not reaction-only."
  (dolist (extra '((body nil "hello")
                   (subject nil "topic")
                   (error ((type . "cancel")))))
    (should-not
     (jabber-reactions--reaction-only-p
      `(message ((from . "juliet@example.com") (type . "chat"))
                (reactions ((xmlns . ,jabber-reactions-xmlns)
                            (id . "target-1"))
                           (reaction nil "👍"))
                ,extra)))))

(ert-deftest jabber-test-reactions-react-allows-custom-picker-input ()
  "Outgoing reaction picker accepts custom reactions outside defaults."
  (let ((sent nil)
        (require-match :unset))
    (cl-letf (((symbol-function 'jabber-reactions--reactable-node-at-point)
               (lambda () (list 'node '(:id "target-1") "target-1")))
              ((symbol-function 'jabber-reactions--chat-target)
               (lambda () (list "juliet@example.com" "chat")))
              ((symbol-function 'jabber-reactions--local-sender)
               (lambda () "romeo@example.com"))
              ((symbol-function 'completing-read)
               (lambda (_prompt _collection _predicate require &rest _args)
                 (setq require-match require)
                 "🔥"))
              ((symbol-function 'jabber-send-sexp)
               (lambda (_jc stanza) (setq sent stanza)))
              ((symbol-function 'jabber-reactions--optimistic-update) #'ignore))
      (with-temp-buffer
        (setq-local jabber-buffer-connection 'fake-jc)
        (jabber-reactions-react-at-point-or-insert)
        (should-not require-match)
        (should (equal (cadr (assq 'reaction (cddr (assq 'reactions (cddr sent)))))
                       nil))
        (should (equal (car (last (assq 'reaction
                                        (cddr (assq 'reactions (cddr sent))))))
                       "🔥"))))))

(ert-deftest jabber-test-reactions-handle-carbon-wrapped-update ()
  "Incoming carbon-wrapped reaction updates the targeted message node."
  (with-temp-buffer
    (let* ((jabber-chat-ewoc (ewoc-create #'ignore))
           (msg '(:id "target-1" :body "hello"))
           (node (ewoc-enter-last jabber-chat-ewoc (list :foreign msg)))
           (inner `(message ((from . "juliet@example.com/laptop")
                             (to . "romeo@example.com")
                             (type . "chat"))
                            (reactions ((xmlns . ,jabber-reactions-xmlns)
                                        (id . "target-1"))
                                       (reaction nil "👍"))))
           (outer `(message ((from . "romeo@example.com") (type . "chat"))
                            (received ((xmlns . "urn:xmpp:carbons:2"))
                                      (forwarded ((xmlns . "urn:xmpp:forward:0"))
                                                 ,inner)))))
      (cl-letf (((symbol-function 'jabber-chat--unwrap-carbon)
                 (lambda (_jc _xml-data) (cons inner nil)))
                ((symbol-function 'jabber-chat-find-buffer)
                 (lambda (_chat-with) (current-buffer)))
                ((symbol-function 'jabber-chat-ewoc-find-by-id)
                 (lambda (_stanza-id) node))
                ((symbol-function 'jabber-chat-ewoc-invalidate) #'ignore))
        (jabber-reactions--handle-message 'fake-jc outer)
        (should (equal (plist-get (cadr (ewoc-data node)) :reactions)
                       '(("juliet@example.com" . ("👍")))))))))

(ert-deftest jabber-test-reactions-handle-sent-carbon-uses-carbon-buffer ()
  "Sent carbon reaction updates the buffer returned by carbon unwrapping."
  (with-temp-buffer
    (let* ((carbon-buffer (current-buffer))
           (jabber-chat-ewoc (ewoc-create #'ignore))
           (msg '(:id "target-1" :body "hello"))
           (node (ewoc-enter-last jabber-chat-ewoc (list :local msg)))
           (inner `(message ((from . "romeo@example.com/phone")
                             (to . "juliet@example.com")
                             (type . "chat"))
                            (reactions ((xmlns . ,jabber-reactions-xmlns)
                                        (id . "target-1"))
                                       (reaction nil "🔥"))))
           (outer `(message ((from . "romeo@example.com") (type . "chat"))
                            (sent ((xmlns . "urn:xmpp:carbons:2"))
                                  (forwarded ((xmlns . "urn:xmpp:forward:0"))
                                             ,inner)))))
      (cl-letf (((symbol-function 'jabber-chat--unwrap-carbon)
                 (lambda (_jc _xml-data) (cons inner carbon-buffer)))
                ((symbol-function 'jabber-chat-find-buffer)
                 (lambda (_chat-with) nil))
                ((symbol-function 'jabber-chat-ewoc-find-by-id)
                 (lambda (_stanza-id) node))
                ((symbol-function 'jabber-chat-ewoc-invalidate) #'ignore))
        (jabber-reactions--handle-message 'fake-jc outer)
        (should (equal (plist-get (cadr (ewoc-data node)) :reactions)
                       '(("romeo@example.com" . ("🔥")))))))))

(provide 'jabber-test-reactions)
;;; jabber-test-reactions.el ends here
