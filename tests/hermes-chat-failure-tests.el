;;; hermes-chat-failure-tests.el --- Classified turn failures -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for structured turn failures: transport normalization of
;; `error_surface', `billing' and `failure_reason', the reducer's failure
;; effect, the compact failure block per class, which recovery actions it
;; offers, and that Retry resends through the existing /retry path.  Plain
;; failures without structured fields keep their ordinary rendering.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(defconst hermes-test--rate-limit-surface
  '((layer . "provider") (code . "rate_limit") (retryable . t)
    (provider . "openrouter") (provider_label . "OpenRouter")
    (model . "gpt-5"))
  "A retryable provider surface as the released backend sends it.")

(defun hermes-test--failure-complete (&rest fields)
  "Return the normalized event of a failed `message.complete' with FIELDS."
  (car (hermes-test--dashboard-events
        (cons "message.complete"
              (append '((text . "The model provider is rate-limiting requests. \
Your message was not answered.\nDetails: 429 Too Many Requests\nSend /retry.")
                        (status . "error")
                        (error . "429 Too Many Requests")
                        (recoverable . t))
                      fields)))))

;;;; Normalization

(ert-deftest hermes-chat-failure-normalizes-error-surface ()
  "A failed completion keeps the surface, its identity and the raw detail."
  (let* ((event (hermes-test--failure-complete
                 (cons 'error_surface hermes-test--rate-limit-surface)
                 '(failure_reason . "rate_limit")))
         (failure (plist-get event :failure)))
    (should (eq (plist-get event :type) 'error))
    (should (equal (plist-get failure :surface)
                   '(:layer "provider" :code "rate_limit" :retryable t
                            :provider "openrouter" :provider-label "OpenRouter"
                            :model "gpt-5" :message nil)))
    (should-not (plist-get failure :billing))
    (should (equal (plist-get failure :reason) "rate_limit"))
    (should (equal (plist-get failure :detail) "429 Too Many Requests"))
    (should-not (plist-get failure :partial-text))))

(ert-deftest hermes-chat-failure-normalizes-billing-and-partial ()
  "A billing wall keeps only an http(s) link; a partial reply is retained."
  (let* ((event (hermes-test--failure-complete
                 '(error_surface . ((layer . "billing") (code . "billing")
                                    (retryable . :false)))
                 '(billing . ((provider . "openai") (provider_label . "OpenAI")
                              (model . "gpt-5") (is_nous . :false)
                              (billing_url . "https://platform.openai.com/billing")
                              (message . "Add credit to continue.")))
                 '(failure_reason . "billing")
                 '(partial . t)))
         (failure (plist-get event :failure)))
    (should (eq (plist-get (plist-get failure :surface) :retryable) nil))
    (should (equal (plist-get failure :billing)
                   '(:provider "openai" :provider-label "OpenAI" :model "gpt-5"
                               :message "Add credit to continue."
                               :url "https://platform.openai.com/billing"
                               :nous nil)))
    (should (string-prefix-p "The model provider is rate-limiting"
                             (plist-get failure :partial-text))))
  (let ((failure (plist-get (hermes-test--failure-complete
                             '(billing . ((provider_label . "Evil")
                                          (billing_url . "javascript:alert(1)"))))
                            :failure)))
    (should-not (plist-get failure :surface))
    (should-not (plist-get (plist-get failure :billing) :url))))

(ert-deftest hermes-chat-failure-normalization-ignores-absent-or-malformed ()
  "Plain or malformed failures carry no structured facts; success never does."
  (dolist (fields '(nil
                    ((error_surface . "provider"))
                    ((error_surface . ((code . "rate_limit"))))
                    ((error_surface . ((layer . ""))))
                    ((billing . "no credit"))
                    ((failure_reason . "rate_limit"))))
    (let ((event (apply #'hermes-test--failure-complete fields)))
      (should (eq (plist-get event :type) 'error))
      (should-not (plist-member event :failure))))
  (let ((surface (hermes-transport-error-surface '((layer . "gateway")))))
    (should (equal (plist-get surface :code) "unknown"))
    (should (eq (plist-get surface :retryable) t)))
  (let ((event (car (hermes-test--dashboard-events
                     (cons "message.complete"
                           `((text . "ok") (status . "complete")
                             (error_surface . ,hermes-test--rate-limit-surface)))))))
    (should (eq (plist-get event :type) 'done))
    (should-not (plist-member event :failure))))

;;;; Reducer

(ert-deftest hermes-chat-failure-reducer-marks-structured-failures ()
  "A structured failure settles through `mark-failure'; plain ones append."
  (let* ((failure '(:surface (:layer "provider" :code "timeout" :retryable t)))
         (event (list :type 'error :content "prose" :failure failure)))
    (should (member (cons 'mark-failure (cons failure 'error))
                    (hermes-chat--turn-error-effects event '(:status ready))))
    (should-not (assq 'append-error
                      (hermes-chat--turn-error-effects event '(:status ready))))
    (should (equal (plist-get (hermes-chat--turn-header-props event) :activity)
                   "The model provider did not answer in time")))
  (should (member '(append-error "boom" . error)
                  (hermes-chat--turn-error-effects
                   '(:type error :content "boom") '(:status ready)))))

;;;; Presentation

(ert-deftest hermes-chat-failure-titles-come-from-surface ()
  "Titles derive from code, then layer, then billing; never from prose."
  (should (equal (hermes-chat-failure-title
                  '(:surface (:layer "provider" :code "rate_limit"
                                     :provider-label "OpenRouter")))
                 "The model provider is rate-limiting requests (OpenRouter)"))
  (should (equal (hermes-chat-failure-title
                  '(:surface (:layer "streaming" :code "unknown")))
                 "The connection to the model provider dropped mid-reply"))
  (should (equal (hermes-chat-failure-title
                  '(:surface (:layer "runtime" :code "agent_init_failed")))
                 "The request failed"))
  (should (equal (hermes-chat-failure-title
                  '(:billing (:provider-label "OpenAI")))
                 "Out of credits · OpenAI"))
  (should (equal (hermes-chat-failure-title '(:billing (:nous t)))
                 "Out of Nous credits")))

(ert-deftest hermes-chat-failure-actions-follow-the-surface ()
  "Only applicable recovery actions are offered."
  (dolist (case
           '(((:surface (:layer "provider" :code "rate_limit" :retryable t))
              (retry switch-model))
             ((:surface (:layer "provider" :code "model_not_found" :retryable nil))
              (switch-model))
             ((:surface (:layer "streaming" :code "unknown" :retryable t))
              (retry))
             ((:surface (:layer "gateway" :code "loop_error" :retryable nil))
              nil)
             ((:surface (:layer "billing" :code "billing" :retryable nil)
                        :billing (:url "https://example.com/billing"))
              (switch-model billing))
             ((:billing (:url nil)) (switch-model))))
    (should (equal (hermes-chat-failure-actions (car case)) (cadr case)))))

(defun hermes-test--fail-turn (failure)
  "Insert an assistant turn in the current chat and fail it with FAILURE.
Return the assistant entry id."
  (let* ((node (hermes-chat--insert-entry
                (hermes-chat--make-entry 'assistant "" 'streaming)))
         (id (plist-get (ewoc-data node) :id)))
    (hermes-chat--mark-assistant-failure id failure 'error)
    id))

(defun hermes-test--failure-transcript ()
  "Return the transcript text above the composer."
  (buffer-substring-no-properties (point-min) hermes-chat--input-marker))

(ert-deftest hermes-chat-failure-renders-compact-block-with-actions ()
  "The block shows title, shadowed cause, a Details disclosure and buttons."
  (hermes-test-with-chat-buffer
    (let ((id (hermes-test--fail-turn
               '(:surface (:layer "provider" :code "rate_limit" :retryable t
                                  :provider-label "OpenRouter" :model "gpt-5")
                          :detail "429 Too Many Requests"))))
      (let ((text (hermes-test--failure-transcript)))
        (should (string-match-p
                 (concat "  ! The model provider is rate-limiting requests (OpenRouter)\n"
                         "    provider · rate_limit · gpt-5\n"
                         "    \\[Retry\\] \\[Switch model\\]  ▸ Details\n")
                 text))
        (should-not (string-match-p "429" text))
        (should-not (string-match-p "Your message was not answered" text)))
      (hermes-test--should-have-face "rate_limit · gpt-5" 'shadow)
      (hermes-test--push-button-labeled "▸ Details")
      (should (string-match-p "▾ Details\n    429 Too Many Requests\n"
                              (hermes-test--failure-transcript)))
      (should (eq (get-text-property
                   (text-property-any (point-min) (point-max)
                                      'hermes-chat-failure-id id)
                   'keymap)
                  hermes-chat-failure-map)))))

(ert-deftest hermes-chat-failure-renders-billing-link-only-when-given ()
  "A billing wall with a link offers it; without one only Switch model."
  (hermes-test-with-chat-buffer
    (hermes-test--fail-turn
     '(:surface (:layer "billing" :code "billing" :retryable nil)
                :billing (:provider-label "OpenAI" :url "https://example.com/b")))
    (let ((text (hermes-test--failure-transcript)))
      (should (string-match-p "! Out of credits · OpenAI\n" text))
      (should (string-match-p "\\[Switch model\\] \\[Open billing page\\]" text))
      (should-not (string-match-p "\\[Retry\\]" text))
      (should-not (string-match-p "Details" text))))
  (hermes-test-with-chat-buffer
    (hermes-test--fail-turn '(:billing (:nous nil :url nil) :reason "billing"))
    (let ((text (hermes-test--failure-transcript)))
      (should (string-match-p "    billing\n    \\[Switch model\\]\n" text))
      (should-not (string-match-p "billing page" text))))
  (hermes-test-with-chat-buffer
    (hermes-test--fail-turn '(:surface (:layer "gateway" :code "unknown"
                                               :retryable nil)))
    (should (string-match-p
             "  ! Hermes hit an internal error while running this turn\n    gateway\\'"
             (string-trim-right (hermes-test--failure-transcript) "[ \n]+")))))

(ert-deftest hermes-chat-failure-keeps-partial-reply-and-plain-failures ()
  "A partial reply stays above the block; a plain failure renders as before."
  (hermes-test-with-chat-buffer
    (hermes-test--fail-turn
     '(:surface (:layer "streaming" :code "unknown" :retryable t)
                :partial-text "Half an answer"))
    (should (string-match-p
             "Half an answer\n  ! The connection to the model provider dropped mid-reply\n"
             (hermes-test--failure-transcript))))
  (hermes-test-with-chat-buffer
    (let* ((node (hermes-chat--insert-entry
                  (hermes-chat--make-entry 'assistant "" 'streaming)))
           (id (plist-get (ewoc-data node) :id)))
      (hermes-chat--append-assistant-content id "Error: boom" 'error)
      (let ((text (hermes-test--failure-transcript)))
        (should (string-match-p "Error: boom" text))
        (should-not (string-match-p "!\\|\\[Retry\\]" text))))))

;;;; Partial replies

(defun hermes-test--fail-streamed-turn (streamed partial)
  "Stream STREAMED into a new assistant turn, then fail it with PARTIAL.
Return the settled assistant entry."
  (let* ((node (hermes-chat--insert-entry
                (hermes-chat--make-entry 'assistant "" 'streaming)))
         (id (plist-get (ewoc-data node) :id)))
    (when streamed
      (hermes-chat--append-assistant-content id streamed 'streaming))
    (hermes-chat--mark-assistant-failure
     id (list :surface '(:layer "streaming" :code "stream_drop" :retryable t)
              :partial-text partial)
     'error)
    (ewoc-data node)))

(ert-deftest hermes-chat-failure-partial-text-is-authoritative ()
  "The retained partial replaces streamed text instead of repeating it."
  (dolist (case '(("Half an answer" "Half an answer" "Half an answer")
                  (nil "Half an answer" "Half an answer")
                  ("Half an ans" "Half an answer." "Half an answer.")))
    (hermes-test-with-chat-buffer
      (pcase-let* ((`(,streamed ,partial ,expected) case)
                   (entry (hermes-test--fail-streamed-turn streamed partial))
                   (text (hermes-test--failure-transcript)))
        (should (equal (plist-get entry :content) expected))
        (should (eq (plist-get entry :status) 'error))
        (should (hermes-chat--entry-failure entry))
        (with-temp-buffer
          (insert text)
          (should (= 1 (how-many (regexp-quote "Half an ans")
                                 (point-min) (point-max)))))))))

;;;; Actions

(ert-deftest hermes-chat-failure-retry-dispatches-slash-retry ()
  "Retry resends via /retry, only for the latest retryable failure."
  (hermes-test-with-chat-buffer
    (let ((calls nil)
          (id (hermes-test--fail-turn
               '(:surface (:layer "provider" :code "timeout" :retryable t)))))
      (cl-letf (((symbol-function 'hermes-chat--dashboard-slash-exec)
                 (lambda (&rest args) (push args calls))))
        (hermes-test--push-button-labeled "[Retry]")
        (should (equal calls '(("retry" "" "retry"))))
        (setq calls nil)
        (goto-char (text-property-any (point-min) (point-max)
                                      'hermes-chat-failure-id id))
        (should (hermes-chat--failure-action-p 'retry))
        (call-interactively (keymap-lookup hermes-chat-failure-map "r"))
        (should (equal calls '(("retry" "" "retry"))))
        (setq calls nil)
        (hermes-chat--insert-entry (hermes-chat--make-entry 'user "later" 'done))
        (should-error (hermes-chat-failure-retry id) :type 'user-error)
        (should-not calls)))))

(ert-deftest hermes-chat-failure-actions-refuse-when-not-applicable ()
  "Commands refuse actions the failure does not offer; billing opens on demand."
  (hermes-test-with-chat-buffer
    (let ((calls nil)
          (browsed nil)
          (id (hermes-test--fail-turn
               '(:surface (:layer "billing" :code "billing" :retryable nil)
                          :billing (:url "https://example.com/b")))))
      (cl-letf (((symbol-function 'hermes-chat--dashboard-slash-exec)
                 (lambda (&rest args) (push args calls)))
                ((symbol-function 'browse-url)
                 (lambda (url &rest _) (push url browsed)))
                ((symbol-function 'hermes-chat-switch-model)
                 (lambda (&rest _) (interactive) (push 'model calls))))
        (should-error (hermes-chat-failure-retry id) :type 'user-error)
        (should-not calls)
        (should-not browsed)
        (hermes-test--push-button-labeled "[Open billing page]")
        (should (equal browsed '("https://example.com/b")))
        (hermes-test--push-button-labeled "[Switch model]")
        (should (equal calls '(model)))))))

(provide 'hermes-chat-failure-tests)
;;; hermes-chat-failure-tests.el ends here
