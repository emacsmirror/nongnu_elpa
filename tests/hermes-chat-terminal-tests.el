;;; hermes-chat-terminal-tests.el --- Chat topic tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'delsel)
(require 'hermes-test-helpers)

(ert-deftest hermes-chat-command-terminal-take-clears-only-exact-owner ()
  "Command terminal take cannot clear a replacement operation."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--command-owner 'old)
   (let ((snapshot (hermes-chat--capture-command-terminal-owner)))
     (should (eq (plist-get snapshot :owner) 'old))
     (setq hermes-chat--command-owner 'new)
     (should-not (hermes-chat--take-command-terminal-owner snapshot))
     (should (eq hermes-chat--command-owner 'new)))
   (let ((snapshot (hermes-chat--capture-command-terminal-owner)))
     (should-not (hermes-chat--take-command-terminal-owner snapshot))
     (should-not hermes-chat--command-owner))))

(ert-deftest hermes-chat-terminal-owner-registry-takes-current-dormantly-in-order ()
  "Combined take clears exact owners and returns ordered dormant effects."
  (hermes-test-with-chat-buffer
   (let* ((token (list 'response-token))
          (timer (timer-create))
          (poll (list :id 'handoff :timer timer))
          (prompt (list :prompt-type "clarify" :request-id "request"
                        :response-token token))
          (retained (list :buffer (current-buffer)
                          :generation hermes-chat--lifecycle-generation
                          :response-token token :text "answer"))
          observed)
     (puthash "request" prompt hermes-chat--pending-prompts)
     (setq hermes-chat--retained-clarify-owners (list retained)
           hermes-chat--command-owner 'command
           hermes-chat--handoff-owner 'handoff
           hermes-chat--handoff-poll poll)
     (let* ((snapshot (hermes-chat--capture-terminal-owners))
            (effects (hermes-chat--take-terminal-owners snapshot)))
       (should (= (length effects) 2))
       (should-not observed)
       (should-not hermes-chat--command-owner)
       (should-not hermes-chat--handoff-owner)
       (should-not hermes-chat--handoff-poll)
       (should-not (gethash "request" hermes-chat--pending-prompts))
       (cl-letf (((symbol-function 'hermes-chat--restore-prompt-response)
                  (lambda (_text) (setq observed (append observed '(prompt)))))
                 ((symbol-function 'cancel-timer)
                  (lambda (_timer) (setq observed (append observed '(timer))))))
         (mapc #'funcall effects))
       (should (equal observed '(prompt timer)))))))

(ert-deftest hermes-chat-terminal-owner-registry-preserves-all-replacements ()
  "Combined stale take leaves every successor authority intact."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--command-owner 'old-command
         hermes-chat--handoff-owner 'old-handoff
         hermes-chat--handoff-poll
         (list :id 'old-handoff :timer (timer-create)))
   (let ((snapshot (hermes-chat--capture-terminal-owners))
         (prompt-table (make-hash-table :test #'equal))
         (auto-table (make-hash-table :test #'equal))
         (poll (list :id 'new-handoff :timer (timer-create))))
     (puthash "successor" '(:prompt-type "sudo") prompt-table)
     (setq hermes-chat--pending-prompts prompt-table
           hermes-chat--auto-prompt-keys auto-table
           hermes-chat--command-owner 'new-command
           hermes-chat--handoff-owner 'new-handoff
           hermes-chat--handoff-poll poll)
     (should-not (hermes-chat--take-terminal-owners snapshot))
     (should (eq hermes-chat--pending-prompts prompt-table))
     (should (eq hermes-chat--auto-prompt-keys auto-table))
     (should (eq hermes-chat--command-owner 'new-command))
     (should (eq hermes-chat--handoff-owner 'new-handoff))
     (should (eq hermes-chat--handoff-poll poll)))))

(ert-deftest hermes-chat-terminal-clear-fields-are-exactly-ephemeral ()
  "Terminal clear fields contain ephemeral authority, never durable state."
  (should
   (equal hermes-chat--terminal-clear-fields
          '(hermes-chat--dashboard-client
            hermes-chat--dashboard-token
            hermes-chat--process
            hermes-chat--dashboard-active-session-id
            hermes-chat--dashboard-session-ready-p
            hermes-chat--dashboard-running-p
            hermes-chat--pending-assistant-id
            hermes-chat--dashboard-stream-assistant-id
            hermes-chat--dashboard-interim-assistant-id
            hermes-chat--dashboard-detached-assistant-id
            hermes-chat--dashboard-suppress-stream-p
            hermes-chat--dashboard-last-start-idle-count
            hermes-chat--server-queued-assistant-id
            hermes-chat--server-queued-user-id
            hermes-chat--server-queued-after-idle-count
            hermes-chat--server-queued-prior-terminal-p
            hermes-chat--busy-submit-context
            hermes-chat--unsettled-submit-context
            hermes-chat--prepared-submit-assistant-id
            hermes-chat--queued-submit-id
            hermes-chat--interrupted-assistant-id
            hermes-chat--interrupted-events
            hermes-chat--interrupt-request-pending-p
            hermes-dashboard-transport-request-owner
            hermes-chat--active-tools)))
  (dolist (field '(hermes-chat--queued-messages
                   hermes-chat--dashboard-idle-count
                   hermes-chat--ewoc hermes-chat--nodes
                   hermes-chat--input-history-draft default-directory
                   hermes-chat--working-directory hermes-chat--profile
                   hermes-chat--model hermes-chat--agent-name
                   hermes-chat--context hermes-chat--goal
                   hermes-chat--runtime-flags hermes-chat--session-id
                   hermes-chat--status-state hermes-chat--title
                   hermes-chat--transport-generation
                   hermes-chat--lifecycle-generation))
    (should-not (memq field hermes-chat--terminal-clear-fields))))

(defun hermes-chat-test--fingerprint-under (value hostile callback)
  "Fingerprint VALUE under one HOSTILE printer ambience using CALLBACK."
  (let ((print-length (if hostile 1 nil))
        (print-level (if hostile 1 nil))
        (print-circle (not hostile))
        (print-gensym (not hostile))
        (print-quoted (not hostile))
        (print-continuous-numbering hostile)
        (print-number-table (and hostile (make-vector 67 nil)))
        (print-escape-newlines (not hostile))
        (print-escape-control-characters (not hostile))
        (print-escape-nonascii (not hostile))
        (print-escape-multibyte (not hostile))
        (print-charset-text-property (not hostile))
        (print-unreadable-function callback)
        (print-integers-as-characters hostile)
        (print-symbols-bare hostile)
        (float-output-format (and hostile "%.2f")))
    (hermes-chat--terminal-fingerprint value)))

(ert-deftest hermes-chat-terminal-fingerprint-binds-every-printer-control ()
  "Fingerprinting overrides every supported ambient printer control."
  (let ((print-length 1) (print-level 1) (print-circle nil)
        (print-gensym nil) (print-quoted nil)
        (print-continuous-numbering t) (print-number-table (make-vector 67 nil))
        (print-escape-newlines nil) (print-escape-control-characters nil)
        (print-escape-nonascii nil) (print-escape-multibyte nil)
        (print-charset-text-property nil) (print-unreadable-function #'ignore)
        (print-integers-as-characters t) (print-symbols-bare t)
        (float-output-format "%.2f") observed)
    (cl-letf (((symbol-function 'prin1-to-string)
               (lambda (&rest _)
                 (setq observed
                       (list print-length print-level print-circle print-gensym
                             print-quoted print-continuous-numbering
                             print-number-table print-escape-newlines
                             print-escape-control-characters print-escape-nonascii
                             print-escape-multibyte print-charset-text-property
                             print-unreadable-function
                             print-integers-as-characters print-symbols-bare
                             float-output-format))
                 "private")))
      (hermes-chat--terminal-fingerprint nil))
    (should (equal observed
                   '(nil nil t t t nil nil t t t t t nil nil nil nil)))))

(ert-deftest hermes-chat-terminal-fingerprint-is-private-and-deterministic ()
  "Fingerprinting defeats hostile printer state for complete structured values."
  (let* ((shared (list "shared-queue-secret"))
         (cycle (list 'cycle-secret))
         (charset (propertize "charset-λ-secret" 'charset 'greek-iso8859-7))
         (value (list :nested (list (list "token-ZZ" "prompt-YY"))
                      :shared shared shared :cycle cycle :charset charset
                      :positioned (position-symbol 'positioned-secret 19)
                      :gensym (make-symbol "gensym-secret")
                      :unreadable (current-buffer) :quoted '(function quoted-secret)
                      :response "response-XX"
                      :input "input-WW\n\x1f" :held "held-VV"
                      :nonascii (unibyte-string 200)
                      :integer 9876543210123456789 :float 12345.6789))
         (calls 0)
         (state 'untouched)
         (callback (lambda (&rest _)
                     (setq calls (1+ calls) state 'mutated)
                     "callback-secret")))
    (setcdr cycle cycle)
    (let ((hostile (hermes-chat-test--fingerprint-under value t callback))
          (opposite (hermes-chat-test--fingerprint-under value nil nil)))
      (should (equal hostile opposite))
      (should (equal hostile
                     (hermes-chat-test--fingerprint-under value t callback)))
      (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" hostile))
      (dolist (plaintext '("token-ZZ" "prompt-YY" "response-XX"
                           "input-WW" "held-VV" "shared-queue-secret"
                           "charset-λ-secret" "9876543210123456789"
                           "12345.6789" "positioned-secret" "gensym-secret"
                           "\n"))
        (should-not (string-match-p (regexp-quote plaintext) hostile)))
      (should (= calls 0))
      (should (eq state 'untouched)))))

(ert-deftest hermes-chat-terminal-field-record-schema-is-exact ()
  "Only one exact three-element record per catalog field passes schema."
  (let* ((exact-value (list :exact (car hermes-chat--terminal-clear-fields)
                            "value-secret"))
         (records
          (cons (hermes-chat--terminal-field-record
                 (car hermes-chat--terminal-clear-fields) exact-value)
                (mapcar (lambda (field)
                          (hermes-chat--terminal-field-record
                           field (list :exact field "value-secret")))
                        (cdr hermes-chat--terminal-clear-fields))))
         (first (car records))
         (second (cadr records)))
    (should (equal first
                   (list (car first) (cadr first)
                         (hermes-chat--terminal-fingerprint (cadr first)))))
    (should (eq exact-value (cadr first)))
    (should (hermes-chat--terminal-fields-schema-p records))
    (dolist (malformed
             (list nil
                   (butlast records)
                   (cons first records)
                   (cons (cons 'unknown-field (cdr first)) (cdr records))
                   (cons second (cons first (cddr records)))
                   (cons (cons 'hermes-chat--session-id (cdr first))
                         (cdr records))
                   (cons first 'dotted-tail)
                   (cons (butlast first) (cdr records))
                   (cons (append first '(extra)) (cdr records))
                   (cons (list (car first) (cadr first) "not-a-digest")
                         (cdr records))))
      (should-not (hermes-chat--terminal-fields-schema-p malformed)))))

(ert-deftest hermes-chat-terminal-fingerprint-schema-is-inert ()
  "Catalog, fingerprint, record, and schema calls do not take live authority."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-client 'client
         hermes-chat--pending-assistant-id "assistant"
         hermes-chat--queued-messages '((:id "queue" :content "durable")))
   (let ((before (mapcar #'symbol-value hermes-chat--terminal-clear-fields))
         (route hermes-chat--turn-event-function)
         (calls 0))
     (let ((hermes-chat--busy-submit-event-function
            (lambda (&rest _) (setq calls (1+ calls))))
           (hermes-chat--busy-submit-abandon-function
            (lambda (&rest _) (setq calls (1+ calls)))))
       (let ((record (hermes-chat--terminal-field-record
                      'hermes-chat--dashboard-client
                      hermes-chat--dashboard-client)))
         (hermes-chat--terminal-fingerprint record)
         (hermes-chat--terminal-fields-schema-p
          (mapcar (lambda (field)
                    (hermes-chat--terminal-field-record field (symbol-value field)))
                  hermes-chat--terminal-clear-fields))))
     (should (equal before
                    (mapcar #'symbol-value hermes-chat--terminal-clear-fields)))
     (should (eq route hermes-chat--turn-event-function))
     (should (= calls 0))
     (should (equal hermes-chat--queued-messages
                    '((:id "queue" :content "durable")))))))

(defun hermes-chat-test--terminal-owner-fixture ()
  "Install real prompt, command, and handoff owners and return their leaves."
  (let* ((token (list 'response-token))
         (prompt (list :prompt-type "clarify" :request-id "clarify"
                       :response-token token))
         (member (list :prompt-type "approval" :session-id "session"))
         (approval (list :prompt-type "approval" :prompt-queue (list member)))
         (claim (list "clarify" prompt))
         (retained (list :buffer (current-buffer)
                         :generation hermes-chat--lifecycle-generation
                         :response-token token :text "answer"))
         (timer (timer-create))
         (meta (list "poll-metadata"))
         (poll (list :id 'handoff :timer timer :meta meta)))
    (puthash "clarify" prompt hermes-chat--pending-prompts)
    (puthash "approval" approval hermes-chat--pending-prompts)
    (puthash "clarify" claim hermes-chat--auto-prompt-keys)
    (setq hermes-chat--retained-clarify-owners (list retained)
          hermes-chat--command-owner (list 'command)
          hermes-chat--handoff-owner (list 'handoff)
          hermes-chat--handoff-poll poll)
    (list :prompt prompt :approval approval :token token :member member
          :claim claim :retained retained :timer timer :poll poll :meta meta)))

(defun hermes-chat-test--replace-terminal-owner-leaf (case fixture)
  "Replace CASE authority in FIXTURE with an equal successor."
  (pcase case
    ('registry (setq hermes-chat--terminal-owner-functions
                     (copy-sequence hermes-chat--terminal-owner-functions)))
    ('lifecycle (setq hermes-chat--lifecycle-generation
                      (list 'replacement-lifecycle)))
    ('mode (fundamental-mode))
    ('take (let ((registry (copy-tree hermes-chat--terminal-owner-functions)))
             (setcdr (car registry) #'ignore)
             (setq hermes-chat--terminal-owner-functions registry)))
    ('command (setq hermes-chat--command-owner
                    (copy-tree hermes-chat--command-owner)))
    ('handoff-owner (setq hermes-chat--handoff-owner
                          (copy-tree hermes-chat--handoff-owner)))
    ('handoff-poll (setq hermes-chat--handoff-poll
                         (copy-tree hermes-chat--handoff-poll)))
    ('handoff-timer (setf (plist-get hermes-chat--handoff-poll :timer)
                          (timer-create)))
    ('prompt-table (setq hermes-chat--pending-prompts
                         (copy-hash-table hermes-chat--pending-prompts)))
    ('auto-table (setq hermes-chat--auto-prompt-keys
                       (copy-hash-table hermes-chat--auto-prompt-keys)))
    ('prompt (puthash "approval" (copy-sequence (plist-get fixture :approval))
                      hermes-chat--pending-prompts))
    ('token (setf (plist-get (plist-get fixture :prompt) :response-token)
                  (copy-tree (plist-get fixture :token))))
    ('retained (setq hermes-chat--retained-clarify-owners
                     (list (copy-tree (plist-get fixture :retained)))))
    ('member (setf (plist-get (plist-get fixture :approval) :prompt-queue)
                   (list (copy-tree (plist-get fixture :member)))))
    ('claim (puthash "clarify" (copy-sequence (plist-get fixture :claim))
                     hermes-chat--auto-prompt-keys))))

(ert-deftest hermes-chat-terminal-owner-authority-is-integrated-and-inert ()
  "Real registered owners validate in capture order without taking authority."
  (hermes-test-with-chat-buffer
   (let* ((fixture (hermes-chat-test--terminal-owner-fixture))
          (prompt-capture (symbol-function 'hermes-chat--capture-terminal-prompts))
          (command-capture (symbol-function 'hermes-chat--capture-command-terminal-owner))
          (handoff-capture (symbol-function 'hermes-chat--capture-handoff-terminal-owner))
          observed authority)
     (cl-letf (((symbol-function 'hermes-chat--capture-terminal-prompts)
                (lambda () (setq observed (append observed '(prompt)))
                  (funcall prompt-capture)))
               ((symbol-function 'hermes-chat--capture-command-terminal-owner)
                (lambda () (setq observed (append observed '(command)))
                  (funcall command-capture)))
               ((symbol-function 'hermes-chat--capture-handoff-terminal-owner)
                (lambda () (setq observed (append observed '(handoff)))
                  (funcall handoff-capture)))
               ((symbol-function 'hermes-chat--take-terminal-prompts) #'ert-fail)
               ((symbol-function 'hermes-chat--take-command-terminal-owner) #'ert-fail)
               ((symbol-function 'hermes-chat--take-handoff-terminal-owner) #'ert-fail))
       (setq authority (hermes-chat--capture-terminal-owner-authority))
       (should (hermes-chat--terminal-owner-authority-current-p authority)))
     (should (eq (plist-get authority :registry)
                 hermes-chat--terminal-owner-functions))
     (should (equal (mapcar #'car (plist-get authority :owners))
                    '(hermes-chat--take-terminal-prompts
                      hermes-chat--take-command-terminal-owner
                      hermes-chat--take-handoff-terminal-owner)))
     (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'"
                             (plist-get authority :digest)))
     (should (equal observed '(prompt command handoff prompt command handoff)))
     (should (eq (gethash "clarify" hermes-chat--auto-prompt-keys)
                 (plist-get fixture :claim))))))

(ert-deftest hermes-chat-terminal-owner-authority-rejects-equal-successors ()
  "Every exact registry and owner leaf rejects a structurally equal successor."
  (dolist (case '(registry lifecycle mode take command
                  handoff-owner handoff-poll handoff-timer
                  prompt-table auto-table prompt token retained member claim))
    (let ((hermes-chat--terminal-owner-functions
           hermes-chat--terminal-owner-functions))
      (hermes-test-with-chat-buffer
       (let* ((fixture (hermes-chat-test--terminal-owner-fixture))
              (authority (hermes-chat--capture-terminal-owner-authority))
              (table hermes-chat--auto-prompt-keys)
              (prompt (plist-get fixture :prompt)))
         (hermes-chat-test--replace-terminal-owner-leaf case fixture)
         (cl-letf (((symbol-function 'remhash) #'ert-fail)
                   ((symbol-function 'cancel-timer) #'ert-fail)
                   ((symbol-function 'hermes-chat--take-terminal-prompts) #'ert-fail)
                   ((symbol-function 'hermes-chat--take-command-terminal-owner) #'ert-fail)
                   ((symbol-function 'hermes-chat--take-handoff-terminal-owner) #'ert-fail))
           (should-not (hermes-chat--terminal-owner-authority-current-p authority)))
         (when (eq case 'claim)
           (should (eq table hermes-chat--auto-prompt-keys))
           (should (eq prompt (cadr (gethash "clarify" table))))))))))

(ert-deftest hermes-chat-terminal-owner-authority-digest-detects-nested-mutation ()
  "Saved digest rejects shared nested mutation under hostile printer settings."
  (hermes-test-with-chat-buffer
   (let* ((fixture (hermes-chat-test--terminal-owner-fixture))
          (authority (hermes-chat--capture-terminal-owner-authority)))
     (setcar (plist-get fixture :meta) "mutated-poll-metadata")
     (let ((print-length 1) (print-level 1) (print-circle nil)
           (print-quoted nil) (print-escape-newlines nil)
           (print-symbols-bare t) (float-output-format "%.1f"))
       (should-not (hermes-chat--terminal-owner-authority-current-p authority))))))

(ert-deftest hermes-chat-terminal-owner-authority-malformed-fails-closed ()
  "Unknown schemas and invalid A3a claims neither signal nor run effects."
  (hermes-test-with-chat-buffer
   (hermes-chat-test--terminal-owner-fixture)
   (let* ((authority (hermes-chat--capture-terminal-owner-authority))
          (owners-bad (plist-put (copy-tree authority) :owners '(malformed)))
          (unknown '((hermes-chat-test--unexpected . ignore)))
          (registries (list nil unknown
                            (reverse hermes-chat--terminal-owner-functions)
                            (cons (car hermes-chat--terminal-owner-functions)
                                  hermes-chat--terminal-owner-functions)
                            (cons (car hermes-chat--terminal-owner-functions) 'dotted)))
          (calls 0))
     (cl-letf (((symbol-function 'hermes-chat-test--unexpected)
                (lambda () (setq calls (1+ calls))))
               ((symbol-function 'hermes-chat--take-terminal-prompts)
                (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'remhash) (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'cancel-timer) (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'hermes-chat--restore-prompt-response)
                (lambda (&rest _) (setq calls (1+ calls)))))
       (dolist (malformed (list nil '(dotted . authority) owners-bad))
         (should-not (hermes-chat--terminal-owner-authority-current-p malformed)))
       (dolist (registry registries)
         (let ((hermes-chat--terminal-owner-functions registry))
           (should-not (hermes-chat--terminal-owner-authority-current-p authority))))
       (let ((hermes-chat--auto-prompt-keys 'invalid-a3a-state))
         (should-not (hermes-chat--terminal-owner-authority-current-p authority)))
       (should (= calls 0))))))

(ert-deftest hermes-chat-terminal-owner-authority-contains-capture-quit ()
  "Capture and validation fail closed when an exact owner capture quits."
  (hermes-test-with-chat-buffer
   (let* ((fixture (hermes-chat-test--terminal-owner-fixture))
          (authority (hermes-chat--capture-terminal-owner-authority))
          (table hermes-chat--auto-prompt-keys)
          (claim (plist-get fixture :claim))
          (calls 0))
     (cl-letf (((symbol-function 'hermes-chat--capture-terminal-prompts)
                (lambda () (signal 'quit nil)))
               ((symbol-function 'hermes-chat--take-terminal-prompts)
                (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'remhash)
                (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'cancel-timer)
                (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'hermes-chat--restore-prompt-response)
                (lambda (&rest _) (setq calls (1+ calls)))))
       (should-not
        (condition-case nil
            (hermes-chat--capture-terminal-owner-authority)
          (quit 'signaled)))
       (should-not
        (condition-case nil
            (hermes-chat--terminal-owner-authority-current-p authority)
          (quit 'signaled))))
     (should (= calls 0))
     (should (eq table hermes-chat--auto-prompt-keys))
     (should (eq claim (gethash "clarify" table))))))

(provide 'hermes-chat-terminal-tests)
;;; hermes-chat-terminal-tests.el ends here
