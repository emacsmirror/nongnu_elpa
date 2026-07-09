;;; hermes-transport.el --- Async Hermes transport  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Pure event/model normalization for hermes-el: shared field accessors and
;; the normalize family that turn a raw gateway frame into an event plist.
;; No I/O lives here; the CLI fallback subprocess is `hermes-transport-cli'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst hermes-transport-event-types
  '(delta done error status tool progress commentary diff unknown)
  "Event types emitted by `hermes-transport-normalize-event'.")

(defun hermes-transport--plist-p (object)
  "Return non-nil if OBJECT is a property list."
  (and (consp object) (keywordp (car object))))

(defun hermes-transport--alist-p (object)
  "Return non-nil if OBJECT is an association list."
  (and (consp object)
       (consp (car object))
       (let ((key (caar object)))
         (or (symbolp key) (stringp key)))))

(defun hermes-transport--object-p (object)
  "Return non-nil if OBJECT can hold named event fields."
  (or (hash-table-p object)
      (hermes-transport--plist-p object)
      (hermes-transport--alist-p object)))

(defun hermes-transport--event-list-p (object)
  "Return non-nil if OBJECT is a list of event objects."
  (and (consp object)
       (not (hermes-transport--object-p object))
       (cl-every #'hermes-transport--object-p object)))

(defun hermes-transport--key-name (key)
  "Return KEY's field name as a string."
  (cond
   ((keywordp key) (substring (symbol-name key) 1))
   ((symbolp key) (symbol-name key))
   ((stringp key) key)))

(defun hermes-transport--key-candidates (key)
  "Return likely field-key representations for KEY."
  (let ((name (hermes-transport--key-name key)))
    (when name
      (list key name (intern name) (intern (concat ":" name))))))

(defconst hermes-transport--missing (make-symbol "hermes-transport-missing")
  "Unique marker returned when an object has no value for a field key.")

(defun hermes-transport--member-value (object candidate)
  "Return OBJECT's value for the single CANDIDATE key.
OBJECT may be a hash table, plist, or alist.  Return
`hermes-transport--missing' when CANDIDATE is absent."
  (cond
   ((hash-table-p object)
    (gethash candidate object hermes-transport--missing))
   ((hermes-transport--plist-p object)
    (let ((tail (plist-member object candidate)))
      (if tail (cadr tail) hermes-transport--missing)))
   ((hermes-transport--alist-p object)
    (let ((cell (assoc candidate object)))
      (if cell (cdr cell) hermes-transport--missing)))
   (t hermes-transport--missing)))

(defun hermes-transport--get (object key)
  "Return OBJECT's value for KEY across plist, alist, or hash forms."
  (catch 'found
    (dolist (candidate (hermes-transport--key-candidates key))
      (let ((value (hermes-transport--member-value object candidate)))
        (unless (eq value hermes-transport--missing)
          (throw 'found value))))))

(defun hermes-transport--get-any (object keys)
  "Return the first present value in OBJECT for KEYS."
  (catch 'found
    (dolist (key keys)
      (dolist (candidate (hermes-transport--key-candidates key))
        (let ((value (hermes-transport--member-value object candidate)))
          (unless (eq value hermes-transport--missing)
            (throw 'found value)))))))

(defun hermes-transport--scalar-string (value)
  "Return VALUE as a display string when VALUE is scalar."
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   ((numberp value) (number-to-string value))))

(defun hermes-transport--field (object key)
  "Return OBJECT's KEY as a scalar string, or nil when absent."
  (hermes-transport--scalar-string (hermes-transport--get object key)))

(defun hermes-transport--display-field (object key)
  "Return OBJECT's KEY as a display string, or an empty string when absent."
  (or (hermes-transport--field object key) ""))

(defun hermes-transport--non-empty-string (value)
  "Return VALUE when it is a non-empty string, else nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun hermes-transport--non-blank-string (value)
  "Return VALUE trimmed when it is non-blank, else nil."
  (and (stringp value)
       (let ((trimmed (string-trim value)))
         (and (not (string-empty-p trimmed)) trimmed))))

(defun hermes-transport--event-name (raw event-name)
  "Return RAW's event name, preferring EVENT-NAME."
  (hermes-transport--scalar-string
   (or event-name
       (and (hermes-transport--object-p raw)
            (hermes-transport--get-any raw '(event type))))))

(defun hermes-transport--phase (event-name)
  "Return the trailing phase component of EVENT-NAME."
  (when event-name
    (car (last (split-string event-name "\\." t)))))

(defun hermes-transport--event-kind (event-name)
  "Return EVENT-NAME normalized for kind comparisons."
  (downcase (replace-regexp-in-string "[._-]" "" (or event-name ""))))

(defun hermes-transport--canonical-type (event-name raw)
  "Return the canonical event type for EVENT-NAME and RAW."
  (let ((name (downcase (or event-name "")))
        (kind (hermes-transport--event-kind event-name)))
    (cond
     ((member name '("delta" "message.delta" "assistant.delta"
                     "messagechunk" "message_chunk"
                     "response.output_text.delta"))
      'delta)
     ((member name '("done" "message.done" "messagestop" "message_stop"
                     "assistant.completed" "run.completed"
                     "response.output_text.done" "response.completed"))
      'done)
     ((member name '("error" "run.failed" "run.cancelled" "response.failed"))
      'error)
     ((member name '("status" "run.started" "message.start" "message_start"
                     "message.started" "message_started"
                     "response.created" "response.in_progress"
                     "gatewaynotice" "gateway_notice" "approval.request"
                     "subagent.start" "subagent.spawn_requested"
                     "subagent.complete"))
      'status)
     ((or (member name '("tool" "tool.started" "tool.completed" "tool.failed"
                         "hermes.tool.progress"
                         "subagent.tool"
                         "toolcallchunk" "tool_call_chunk"
                         "toolcallfinished" "tool_call_finished"))
          (member kind '("toolcallchunk" "toolcallfinished"))
          (and (member name '("response.output_item.added"
                              "response.output_item.done"))
               (let* ((item (hermes-transport--get raw 'item))
                      (kind (hermes-transport--scalar-string
                             (hermes-transport--get item 'type))))
                 (member kind '("function_call" "function_call_output")))))
      'tool)
     ((member name '("progress" "tool.progress" "subagent.progress"
                     "longtoolhint" "long_tool_hint"))
      'progress)
     ((member name '("commentary" "message.commentary" "reasoning.available"
                     "subagent.thinking"))
      'commentary)
     ((string= name "diff")
      'diff)
     ((string-prefix-p "subagent." name)
      'status)
     ((string-empty-p name)
      nil)
     (t 'unknown))))

(defun hermes-transport--put-common-fields (event raw event-name)
  "Return EVENT with common metadata copied from RAW and EVENT-NAME."
  (let ((name (hermes-transport--scalar-string event-name)))
    (when (and name
               (not (string-empty-p name))
               (or (hermes-transport--get raw 'event)
                   (string-match-p "\\." name)
                   (not (member (downcase name)
                                '("delta" "done" "error" "status" "tool"
                                  "progress" "commentary" "diff")))))
      (setq event (plist-put event :event name)))
    (dolist (field '((run_id . :run-id)
                     (session_id . :session-id)
                     (message_id . :message-id)
                     (subagent_id . :subagent-id)
                     (parent_id . :parent-id)
                     (child_session_id . :child-session-id)
                     (seq . :seq)
                     (index . :index)
                     (task_index . :task-index)
                     (task_count . :task-count)
                     (tool_count . :tool-count)
                     (sequence_number . :seq)
                     (timestamp . :timestamp)
                     (ts . :timestamp)))
      (let ((value (hermes-transport--get raw (car field))))
        (when value
          (setq event (plist-put event (cdr field) value)))))
    event))

(defun hermes-transport--field-present-p (object key)
  "Return non-nil if OBJECT has KEY, even when its value is nil."
  (catch 'found
    (dolist (candidate (hermes-transport--key-candidates key))
      (unless (eq (hermes-transport--member-value object candidate)
                  hermes-transport--missing)
        (throw 'found t)))))

(defun hermes-transport--tool-event-status (raw item event-name)
  "Return normalized tool status for RAW, ITEM, and EVENT-NAME."
  (or (hermes-transport--scalar-string
       (or (hermes-transport--get raw 'status)
           (hermes-transport--get item 'status)))
      (pcase (hermes-transport--event-kind event-name)
        ((or "toolcallchunk" "subagenttool") "running")
        ("toolcallfinished"
         (if (and (hermes-transport--field-present-p raw 'ok)
                  (not (hermes-transport--get raw 'ok)))
             "failed"
           "completed"))
        (_ (hermes-transport--phase event-name)))))

(defun hermes-transport--content (raw)
  "Return RAW's primary text payload, or nil."
  (hermes-transport--scalar-string
   (hermes-transport--get-any raw '(content delta text output preview summary
                                            tool_preview))))

(defun hermes-transport--invalid-event (raw reason &optional event-name)
  "Return an error event for invalid RAW with REASON and EVENT-NAME."
  (let ((event (list :type 'error :content reason :raw raw)))
    (if event-name
        (plist-put event :event (hermes-transport--scalar-string event-name))
      event)))

(defun hermes-transport--normalize-delta (raw event-name)
  "Return RAW normalized as a delta EVENT-NAME."
  (plist-put
   (hermes-transport--put-common-fields (list :type 'delta) raw event-name)
   :content (or (hermes-transport--content raw) "")))

(defun hermes-transport--normalize-done (raw event-name)
  "Return RAW normalized as a done EVENT-NAME."
  (let ((event (hermes-transport--put-common-fields
                (list :type 'done) raw event-name))
        (content (hermes-transport--content raw)))
    (if content
        (plist-put event :content content)
      event)))

(defun hermes-transport--normalize-error (raw event-name)
  "Return RAW normalized as an error EVENT-NAME."
  (let* ((error-data (hermes-transport--get raw 'error))
         (response (hermes-transport--get raw 'response))
         (response-error (and (hermes-transport--object-p response)
                              (hermes-transport--get response 'error)))
         (content (or (hermes-transport--content raw)
                      (hermes-transport--scalar-string
                       (hermes-transport--get raw 'message))
                      (and (hermes-transport--object-p error-data)
                           (or (hermes-transport--content error-data)
                               (hermes-transport--scalar-string
                                (hermes-transport--get error-data 'message))))
                      (and (hermes-transport--object-p response-error)
                           (or (hermes-transport--content response-error)
                               (hermes-transport--scalar-string
                                (hermes-transport--get response-error 'message))))
                      (hermes-transport--scalar-string error-data)
                      "Hermes transport error")))
    (plist-put
     (hermes-transport--put-common-fields (list :type 'error) raw event-name)
     :content content)))

(defun hermes-transport--normalize-status (raw event-name)
  "Return RAW normalized as a status EVENT-NAME."
  (let* ((event (hermes-transport--put-common-fields
                 (list :type 'status) raw event-name))
         (status (or (hermes-transport--scalar-string
                      (hermes-transport--get raw 'status))
                     (pcase (downcase (or event-name ""))
                       ((or "message.start" "message_start"
                            "message.started" "message_started")
                        "started")
                       (_ (hermes-transport--phase event-name)))))
         (content (hermes-transport--content raw)))
    (when status
      (setq event (plist-put event :status status)))
    (when content
      (setq event (plist-put event :content content)))
    event))

(defun hermes-transport--put-present-fields (event pairs)
  "Return EVENT with each (KEY . VALUE) of PAIRS set when VALUE is non-nil."
  (dolist (pair pairs event)
    (when (cdr pair)
      (setq event (plist-put event (car pair) (cdr pair))))))

(defun hermes-transport--normalize-progress (raw event-name)
  "Return RAW normalized as a progress EVENT-NAME."
  (hermes-transport--put-present-fields
   (hermes-transport--put-common-fields (list :type 'progress) raw event-name)
   (list (cons :name (hermes-transport--scalar-string
                      (hermes-transport--get-any raw '(tool_name tool name kind))))
         (cons :content (hermes-transport--content raw))
         (cons :progress (hermes-transport--get raw 'progress)))))

(defun hermes-transport--normalize-tool (raw event-name)
  "Return RAW normalized as a tool EVENT-NAME."
  (let* ((item (or (hermes-transport--get raw 'item) raw))
         (event (hermes-transport--put-common-fields
                 (list :type 'tool) raw event-name))
         (name (hermes-transport--scalar-string
                (or (hermes-transport--get-any raw '(tool_name tool name))
                    (hermes-transport--get item 'name))))
         (status (hermes-transport--tool-event-status raw item event-name))
         (preview (or (hermes-transport--content raw)
                      (hermes-transport--content item)
                      (hermes-transport--scalar-string
                       (hermes-transport--get raw 'label))))
         (args (or (hermes-transport--get raw 'args)
                   (hermes-transport--get raw 'arguments)
                   (hermes-transport--get item 'arguments)))
         (duration (hermes-transport--get raw 'duration))
         (error-value (hermes-transport--get raw 'error))
         (tool-call-id (hermes-transport--get-any
                        raw '(toolCallId tool_call_id call_id)))
         (emoji (hermes-transport--get raw 'emoji)))
    (hermes-transport--put-present-fields
     event
     (list (cons :name name) (cons :status status) (cons :preview preview)
           (cons :args args) (cons :duration duration)
           (cons :error error-value) (cons :tool-call-id tool-call-id)
           (cons :emoji emoji)))))

(defun hermes-transport--normalize-commentary (raw event-name)
  "Return RAW normalized as a commentary EVENT-NAME."
  (plist-put
   (hermes-transport--put-common-fields (list :type 'commentary) raw event-name)
   :content (or (hermes-transport--content raw) "")))

(defun hermes-transport--normalize-diff (raw event-name)
  "Return RAW normalized as an optional diff EVENT-NAME."
  (plist-put
   (hermes-transport--put-common-fields (list :type 'diff) raw event-name)
   :content (or (hermes-transport--content raw) "")))

(defun hermes-transport--normalize-unknown (raw event-name)
  "Return RAW normalized as an unknown EVENT-NAME."
  (plist-put
   (hermes-transport--put-common-fields (list :type 'unknown) raw event-name)
   :raw raw))

(defun hermes-transport-normalize-event (raw &optional event-name)
  "Normalize RAW transport data into a plist event.
EVENT-NAME supplies an SSE event name when RAW came from an `event:' line."
  (cond
   ((stringp raw)
    (car (hermes-transport-parse-events raw event-name)))
   ((hermes-transport--object-p raw)
    (let* ((name (hermes-transport--event-name raw event-name))
           (type (hermes-transport--canonical-type name raw)))
      (pcase type
        ('delta (hermes-transport--normalize-delta raw name))
        ('done (hermes-transport--normalize-done raw name))
        ('error (hermes-transport--normalize-error raw name))
        ('status (hermes-transport--normalize-status raw name))
        ('progress (hermes-transport--normalize-progress raw name))
        ('tool (hermes-transport--normalize-tool raw name))
        ('commentary (hermes-transport--normalize-commentary raw name))
        ('diff (hermes-transport--normalize-diff raw name))
        ('unknown (hermes-transport--normalize-unknown raw name))
        (_ (hermes-transport--invalid-event
            raw "Invalid Hermes transport event" name)))))
   (t
    (hermes-transport--invalid-event
     raw "Invalid Hermes transport event" event-name))))

(defun hermes-transport-json-parse (string)
  "Parse STRING as JSON with the shared alist/list option set.
All hermes-el JSON parsing goes through this so objects, arrays, null, and
false decode identically everywhere."
  (json-parse-string string
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun hermes-transport--json-read (string)
  "Parse STRING as JSON and return (t . VALUE), or nil on failure."
  (condition-case nil
      (cons t (hermes-transport-json-parse string))
    (error nil)))

(defun hermes-transport--structured-json-looking-p (string)
  "Return non-nil if STRING is shaped like structured event JSON."
  (and (string-match-p "\\`[[:space:]]*[{[]" string)
       (string-match-p "\"\\(?:event\\|type\\)\"" string)))

(defun hermes-transport--events-from-value (value &optional event-name raw-text)
  "Return normalized events from VALUE, EVENT-NAME, and RAW-TEXT."
  (cond
   ((hermes-transport--event-list-p value)
    (apply #'append
           (mapcar (lambda (event)
                     (hermes-transport--events-from-value event event-name))
                   value)))
   ((hermes-transport--object-p value)
    (if (or event-name
            (hermes-transport--get-any value '(event type)))
        (list (hermes-transport-normalize-event value event-name))
      (list (list :type 'delta :content (or raw-text "")))))
   (event-name
    (list (hermes-transport-normalize-event
           (list :event event-name :content value) event-name)))
   (raw-text
    (list (list :type 'delta :content raw-text)))
   (t
    (list (hermes-transport--invalid-event
           value "Invalid Hermes transport event" event-name)))))

(defun hermes-transport--sse-line-value (line prefix)
  "Return LINE's value after PREFIX, removing one optional space."
  (when (string-prefix-p prefix line)
    (let ((value (substring line (length prefix))))
      (if (string-prefix-p " " value)
          (substring value 1)
        value))))

(defun hermes-transport--parse-sse-frame (frame)
  "Return normalized events parsed from one SSE FRAME."
  (let (event data-lines)
    (dolist (line (split-string frame "\n"))
      (cond
       ((string-prefix-p ":" line) nil)
       ((string-prefix-p "event:" line)
        (setq event (hermes-transport--sse-line-value line "event:")))
       ((string-prefix-p "data:" line)
        (push (hermes-transport--sse-line-value line "data:") data-lines))))
    (when data-lines
      (let* ((data (string-join (nreverse data-lines) "\n"))
             (parsed (hermes-transport--json-read data)))
        (cond
         ((string= (string-trim data) "[DONE]")
          (list '(:type done)))
         (parsed
          (hermes-transport--events-from-value (cdr parsed) event data))
         (event
          (hermes-transport--events-from-value data event data))
         (t
          (list (hermes-transport--invalid-event
                 data "Invalid Hermes SSE event data" event))))))))

(defun hermes-transport--sse-stream-p (string)
  "Return non-nil if STRING begins with server-sent event syntax."
  (string-match-p "\\`[[:space:]\n\r]*\\(?::\\|event:\\|data:\\)" string))

(defun hermes-transport--parse-sse-events (string)
  "Return normalized events parsed from SSE STRING, or nil."
  (when (hermes-transport--sse-stream-p string)
    (mapcan #'hermes-transport--parse-sse-frame
            (split-string (replace-regexp-in-string "\r\n?" "\n" string)
                          "\n\n" t))))

(defun hermes-transport-parse-events (raw &optional event-name)
  "Parse RAW transport data and return normalized plist events.
EVENT-NAME supplies an explicit SSE event name when RAW is a data payload.
RAW may be a plist/alist/hash event, a list of events, JSON/SSE text, or plain
assistant text.  Plain text is returned as one `delta' event unless EVENT-NAME
names a structured event type."
  (cond
   ((stringp raw)
    (if (hermes-transport--sse-stream-p raw)
        (hermes-transport--parse-sse-events raw)
      (let ((parsed (hermes-transport--json-read raw)))
        (cond
         (parsed
          (hermes-transport--events-from-value (cdr parsed) event-name raw))
         ((hermes-transport--structured-json-looking-p raw)
          (list (hermes-transport--invalid-event
                 raw "Invalid Hermes transport JSON" event-name)))
         (event-name
          (hermes-transport--events-from-value raw event-name raw))
         (t
          (list (list :type 'delta :content raw)))))))
   ((hermes-transport--event-list-p raw)
    (apply #'append
           (mapcar (lambda (event)
                     (hermes-transport-parse-events event event-name))
                   raw)))
   ((hermes-transport--object-p raw)
    (list (hermes-transport-normalize-event raw event-name)))
   (t
    (list (hermes-transport--invalid-event
           raw "Invalid Hermes transport event" event-name)))))

(defun hermes-transport--emit (callback raw &optional event-name)
  "Normalize RAW/EVENT-NAME and invoke CALLBACK for each event."
  (dolist (event (hermes-transport-parse-events raw event-name))
    (funcall callback event)))

;;; Dashboard frame normalization

;; Pure JSON-RPC frame -> event helpers for the dashboard gateway.  They
;; moved here from hermes-dashboard-transport.el and keep their historical
;; `hermes-dashboard-transport--' prefix so callers and tests stay stable.

(defun hermes-dashboard-transport--frame-id (frame)
  "Return FRAME's JSON-RPC id as a string, or nil."
  (hermes-transport--scalar-string (hermes-transport--get frame 'id)))

(defun hermes-dashboard-transport--frame-kind (frame)
  "Return FRAME kind: response, error-response, event, or unknown."
  (cond
   ((and (hermes-dashboard-transport--frame-id frame)
         (hermes-transport--get frame 'error))
    'error-response)
   ((hermes-dashboard-transport--frame-id frame) 'response)
   ((equal (hermes-transport--get frame 'method) "event") 'event)
   (t 'unknown)))

(defun hermes-dashboard-transport--response-error-message (frame)
  "Return JSON-RPC error message from FRAME."
  (let ((error (hermes-transport--get frame 'error)))
    (or (and (hermes-transport--object-p error)
             (hermes-transport--scalar-string
              (hermes-transport--get error 'message)))
        (hermes-transport--scalar-string error)
        "Hermes dashboard request failed")))

(defun hermes-dashboard-transport--response-error-code (frame)
  "Return JSON-RPC error code from FRAME, if present."
  (let ((error (hermes-transport--get frame 'error)))
    (and (hermes-transport--object-p error)
         (hermes-transport--get error 'code))))

(defun hermes-dashboard-transport--payload-text (payload)
  "Return PAYLOAD's best display text, or nil."
  (hermes-transport--scalar-string
   (hermes-transport--get-any payload
                              '(text rendered content delta message context
                                     question prompt description command env_var
                                     summary result_text result preview))))

(defun hermes-dashboard-transport--event-base (type params payload)
  "Return base event plist for TYPE, PARAMS, and PAYLOAD."
  (let ((event (list :event type))
        (session-id (hermes-transport--get params 'session_id)))
    (when session-id
      (setq event (plist-put event :session-id session-id)))
    (when-let* ((request-id (hermes-transport--get payload 'request_id)))
      (setq event (plist-put event :request-id request-id)))
    event))

(defun hermes-dashboard-transport--status-event (type params payload status content)
  "Return a status event for TYPE, PARAMS, PAYLOAD, STATUS, and CONTENT."
  (let ((event (plist-put
                (hermes-dashboard-transport--event-base type params payload)
                :type 'status)))
    (when status
      (setq event (plist-put event :status status)))
    (when content
      (setq event (plist-put event :content content)))
    event))

(defun hermes-dashboard-transport--background-complete-event (type params payload)
  "Return a `background' event for `background.complete' TYPE/PARAMS/PAYLOAD.
The event carries the originating `:task-id' and the agent's full response as
`:content', so the chat layer can pair it with the launching task and render a
persistent result entry instead of a transient status line."
  (let ((event (plist-put
                (hermes-dashboard-transport--event-base type params payload)
                :type 'background))
        (task-id (hermes-transport--scalar-string
                  (hermes-transport--get payload 'task_id)))
        (content (hermes-dashboard-transport--payload-text payload)))
    (when task-id
      (setq event (plist-put event :task-id task-id)))
    (plist-put event :content (or content ""))))

(defun hermes-dashboard-transport--tool-event (type params payload status)
  "Return a tool event for TYPE, PARAMS, PAYLOAD, and STATUS."
  (let ((event (plist-put
                (hermes-dashboard-transport--event-base type params payload)
                :type 'tool))
        (preview (hermes-dashboard-transport--payload-text payload)))
    (dolist (field '((tool_id . :tool-call-id) (name . :name)
                     (args . :args) (args_text . :args)
                     (context . :context) (summary . :summary)
                     (result_text . :result-text) (result . :result)
                     (duration_s . :duration) (duration . :duration)))
      (when-let* ((value (hermes-transport--get payload (car field))))
        (setq event (plist-put event (cdr field) value))))
    (when preview
      (setq event (plist-put event :preview preview)))
    (plist-put event :status status)))

(defun hermes-dashboard-transport--inline-diff-event (type params payload)
  "Return a normalized inline diff event for TYPE/PARAMS/PAYLOAD, if any."
  (when-let* ((content (hermes-transport--scalar-string
			(hermes-transport--get payload 'inline_diff))))
    (plist-put
     (plist-put (hermes-dashboard-transport--event-base type params payload)
                :type 'diff)
     :content content)))

(defun hermes-dashboard-transport--tool-complete-events (type params payload)
  "Return normalized `tool.complete' events for TYPE/PARAMS/PAYLOAD."
  (let ((events (list (hermes-dashboard-transport--tool-event
                       type params payload "completed"))))
    (if-let* ((diff (hermes-dashboard-transport--inline-diff-event
                     type params payload)))
        (append events (list diff))
      events)))

(defun hermes-dashboard-transport--payload-event (type params payload kind)
  "Return a single transport event of KIND for TYPE/PARAMS/PAYLOAD."
  (let ((event (plist-put
                (hermes-dashboard-transport--event-base type params payload)
                :type kind))
        (content (hermes-dashboard-transport--payload-text payload)))
    (if content
        (plist-put event :content content)
      event)))

(defun hermes-dashboard-transport--message-complete-kind (payload)
  "Return the transport kind for a `message.complete' PAYLOAD."
  (let ((status (downcase
                 (or (hermes-transport--scalar-string
                      (hermes-transport--get payload 'status))
                     "complete"))))
    (if (member status '("complete" "completed" "done" "success" "ok"))
        'done
      'error)))

(defun hermes-dashboard-transport--usage-plist (payload)
  "Return an :input/:output token usage plist from PAYLOAD's usage, or nil.
The backend nests per-turn token counts under PAYLOAD's `usage' object, like
`hermes-dashboard-transport--context-plist' reads the context fields.  Only
positive token counts are reported, so an empty turn shows no gauge."
  (let* ((usage (hermes-transport--get payload 'usage))
         (input (hermes-transport--get usage 'input))
         (output (hermes-transport--get usage 'output)))
    (and (or (and (numberp input) (> input 0))
             (and (numberp output) (> output 0)))
         (list :input input :output output))))

(defun hermes-dashboard-transport--context-plist (payload)
  "Return a context-window plist from PAYLOAD's usage, or nil.
The plist holds :used, :max, and :percent for the model's context window."
  (when-let* ((usage (hermes-transport--get payload 'usage))
              (max (hermes-transport--get usage 'context_max))
              ((and (numberp max) (> max 0))))
    (list :used (or (hermes-transport--get usage 'context_used) 0)
          :max max
          :percent (or (hermes-transport--get usage 'context_percent) 0))))

(defun hermes-dashboard-transport--message-complete-event (type params payload)
  "Return a normalized `message.complete' event for TYPE/PARAMS/PAYLOAD."
  (let* ((status (hermes-transport--scalar-string
                  (hermes-transport--get payload 'status)))
         (usage (hermes-dashboard-transport--usage-plist payload))
         (context (hermes-dashboard-transport--context-plist payload))
         (warning (hermes-transport--non-empty-string
                   (hermes-transport--get payload 'warning)))
         (event (hermes-dashboard-transport--payload-event
                 type params payload
                 (hermes-dashboard-transport--message-complete-kind payload))))
    (when usage (setq event (plist-put event :usage usage)))
    (when context (setq event (plist-put event :context context)))
    (when status (setq event (plist-put event :status status)))
    (when warning (setq event (plist-put event :warning warning)))
    event))

(defun hermes-dashboard-transport--prompt-title (prompt-type)
  "Return human title for PROMPT-TYPE."
  (pcase prompt-type
    ("approval" "Approval requested")
    ("clarify" "Clarification requested")
    ("sudo" "Sudo password requested")
    ("secret" "Secret requested")
    ("terminal" "Terminal read requested")
    (_ (format "%s requested" prompt-type))))

(defun hermes-dashboard-transport--prompt-content (prompt-type payload)
  "Return redacted display content for PROMPT-TYPE and PAYLOAD."
  (let ((title (hermes-dashboard-transport--prompt-title prompt-type)))
    (pcase prompt-type
      ("approval"
       (string-join
        (delq nil (list title
                        (hermes-transport--scalar-string
                         (hermes-transport--get payload 'description))
                        (hermes-transport--scalar-string
                         (hermes-transport--get payload 'command))))
        ": "))
      ("secret"
       (string-join
        (delq nil (list title
                        (hermes-transport--scalar-string
                         (hermes-transport--get payload 'prompt))
                        (hermes-transport--scalar-string
                         (hermes-transport--get payload 'env_var))))
        ": "))
      ("terminal"
       (let ((start (hermes-transport--get payload 'start))
             (count (hermes-transport--get payload 'count)))
         (if (or start count)
             (format "%s (start %s, count %s)" title
                     (or start "0") (or count "all"))
           title)))
      (_
       (or (hermes-dashboard-transport--payload-text payload) title)))))

(defun hermes-dashboard-transport--copy-prompt-fields (event payload)
  "Copy safe prompt request fields from PAYLOAD into EVENT."
  (dolist (field '((question . :question) (choices . :choices)
                   (prompt . :prompt) (env_var . :env-var)
                   (command . :command) (description . :description)
                   (pattern_key . :pattern-key)
                   (pattern_keys . :pattern-keys)
                   (start . :start) (count . :count)))
    (when-let* ((value (hermes-transport--get payload (car field))))
      (setq event (plist-put event (cdr field) value))))
  (when (hermes-transport--field-present-p payload 'allow_permanent)
    (setq event (plist-put event :allow-permanent
                           (hermes-transport--get payload 'allow_permanent))))
  event)

(defun hermes-dashboard-transport--prompt-request-event (type params payload)
  "Return a redacted prompt request status event for TYPE/PARAMS/PAYLOAD."
  (let* ((prompt-type (car (split-string type "\\." t)))
         (event (hermes-dashboard-transport--status-event
                 type params payload "requested"
                 (hermes-dashboard-transport--prompt-content
                  prompt-type payload))))
    (setq event (plist-put event :prompt-type prompt-type))
    (setq event (plist-put event :prompt-request-p t))
    (hermes-dashboard-transport--copy-prompt-fields event payload)))

(defun hermes-dashboard-transport--payload-object (payload)
  "Return PAYLOAD as an object suitable for normalization."
  (cond
   ((hermes-transport--object-p payload) payload)
   ((null payload) nil)
   (t `((content . ,payload)))))

(defun hermes-dashboard-transport--session-info-content (payload)
  "Return a compact display string for a `session.info' PAYLOAD."
  (let ((model (hermes-transport--scalar-string
                (hermes-transport--get payload 'model)))
        (provider (hermes-transport--scalar-string
                   (hermes-transport--get payload 'provider)))
        (warning (hermes-transport--scalar-string
                  (hermes-transport--get payload 'config_warning))))
    (string-join
     (delq nil
           (list (cond
                  ((and model provider)
                   (format "Session ready: %s via %s" model provider))
                  (model (format "Session ready: %s" model))
                  (provider (format "Session ready via %s" provider))
                  (t "Session ready"))
                 warning))
     " — ")))

(defun hermes-dashboard-transport--session-info-event (type params payload)
  "Return a normalized `session.info' status event for TYPE/PARAMS/PAYLOAD.
Surface the session's model and profile (agent) name so the chat header can
show them."
  (let ((event (hermes-dashboard-transport--status-event
                type params payload "ready"
                (hermes-dashboard-transport--session-info-content payload)))
        (model (hermes-transport--scalar-string
                (hermes-transport--get payload 'model)))
        (agent (hermes-transport--scalar-string
                (hermes-transport--get payload 'profile_name)))
        (context (hermes-dashboard-transport--context-plist payload)))
    (when model (setq event (plist-put event :model model)))
    (when agent (setq event (plist-put event :agent-name agent)))
    (when context (setq event (plist-put event :context context)))
    event))

(defun hermes-dashboard-transport--generic-event (type params payload)
  "Return generic normalized event for TYPE/PARAMS/PAYLOAD."
  (let* ((object (or (hermes-dashboard-transport--payload-object payload) '()))
         (session-id (hermes-transport--get params 'session_id))
         (raw (if session-id
                  (append object `((session_id . ,session-id)))
                object)))
    (list (hermes-transport-normalize-event raw type))))

(defun hermes-dashboard-transport--tool-generating-event (type params payload)
  "Return a header-only `thinking' event for a `tool.generating' PAYLOAD.
TYPE and PARAMS supply event metadata.  The gateway emits `tool.generating'
once when the model starts streaming a tool call, before the authoritative
`tool.start'; surface it as transient header activity so a large tool payload
does not look like a frozen screen.  It is intentionally not a transcript
entry."
  (let ((event (plist-put
                (hermes-dashboard-transport--event-base type params payload)
                :type 'thinking))
        (name (hermes-transport--scalar-string
               (hermes-transport--get payload 'name))))
    (plist-put event :content
               (if name (format "Calling %s" name) "Calling tool"))))

(defun hermes-dashboard-transport--prettify-event-name (type)
  "Return a human label for gateway event TYPE, e.g. \"Background Complete\"."
  (let ((words (replace-regexp-in-string "[._]" " " (or type ""))))
    (if (string-empty-p words) "Event" (capitalize words))))

(defun hermes-dashboard-transport--display-fallback-event (type params payload)
  "Return a status or progress event for an unclassified TYPE/PARAMS/PAYLOAD.
The trailing phase of TYPE picks the channel and PAYLOAD's text -- or a label
derived from TYPE when it carries none -- is the body, so any event the gateway
adds still renders as a labelled line instead of an error."
  (let* ((kind (if (member (hermes-transport--phase type)
                           '("progress" "generating"))
                   'progress
                 'status))
         (text (hermes-dashboard-transport--payload-text payload))
         (event (plist-put
                 (hermes-dashboard-transport--event-base type params payload)
                 :type kind)))
    (when (eq kind 'status)
      (setq event (plist-put event :status "notification")))
    (plist-put event :content
               (or text (hermes-dashboard-transport--prettify-event-name type)))))

(defun hermes-dashboard-transport--generic-display-event (type params payload)
  "Return display events for an otherwise-unhandled TYPE/PARAMS/PAYLOAD.
Try the structured classifier first so `subagent.*' and SSE-style events keep
their rich typing; fall back to a labelled status/progress line for anything it
cannot classify, so a newly added gateway event renders instead of surfacing as
an Unknown error."
  (let ((event (car (hermes-dashboard-transport--generic-event
                     type params payload))))
    (if (and event (not (eq (plist-get event :type) 'unknown)))
        (list event)
      (list (hermes-dashboard-transport--display-fallback-event
             type params payload)))))

(defun hermes-dashboard-transport--normalize-event-frame (frame)
  "Return normalized transport events for JSON-RPC event FRAME."
  (let* ((params (hermes-transport--get frame 'params))
         (type (hermes-transport--scalar-string
                (hermes-transport--get params 'type)))
         (payload (or (hermes-transport--get params 'payload) '())))
    (pcase type
      ("gateway.ready"
       (list (hermes-dashboard-transport--status-event
              type params payload "ready" "Hermes dashboard connected")))
      ("session.info"
       (list (hermes-dashboard-transport--session-info-event
              type params payload)))
      ("message.delta"
       (list (hermes-dashboard-transport--payload-event type params payload 'delta)))
      ("message.complete"
       (list (hermes-dashboard-transport--message-complete-event
              type params payload)))
      ("error"
       (list (hermes-dashboard-transport--payload-event type params payload 'error)))
      ("status.update"
       (let ((status (hermes-transport--scalar-string
                      (or (hermes-transport--get payload 'kind)
                          (hermes-transport--get payload 'status)))))
         (list (hermes-dashboard-transport--status-event
                type params payload status
                (hermes-dashboard-transport--payload-text payload)))))
      ("tool.start"
       (list (hermes-dashboard-transport--tool-event
              type params payload "running")))
      ("tool.complete"
       (hermes-dashboard-transport--tool-complete-events
        type params payload))
      ("tool.generating"
       (list (hermes-dashboard-transport--tool-generating-event
              type params payload)))
      ("reasoning.delta"
       (list (hermes-dashboard-transport--payload-event
              type params payload 'commentary)))
      ;; `thinking.delta' carries the kawaii spinner status (face + verb), not
      ;; real reasoning.  Surface it as a header-only `thinking' event so the
      ;; live verb (e.g. "Pondering...") rotates in the status line; the chat
      ;; layer never turns a `thinking' event into a transcript entry.
      ("thinking.delta"
       (list (hermes-dashboard-transport--payload-event
              type params payload 'thinking)))
      ((or "approval.request" "clarify.request" "sudo.request"
           "secret.request" "terminal.read.request")
       (list (hermes-dashboard-transport--prompt-request-event
              type params payload)))
      ;; Voice mode and skin changes are client-UI concerns, not chat transcript
      ;; content; drop them so they do not render at all.
      ((or "voice.status" "voice.transcript" "skin.changed")
       nil)
      ;; `review.summary' is a self-improvement notification; show it as a status
      ;; line in the transcript rather than as an Unknown event.
      ("review.summary"
       (list (hermes-dashboard-transport--status-event
              type params payload "notification"
              (hermes-dashboard-transport--payload-text payload))))
      ;; A `/btw' background task finishing in its own session.  Keep it as a
      ;; dedicated `background' event so the chat layer renders a persistent
      ;; result entry rather than letting it decay into a transient status line.
      ("background.complete"
       (list (hermes-dashboard-transport--background-complete-event
              type params payload)))
      (_
       (if (and type (string-prefix-p "notification." type))
           (list (hermes-dashboard-transport--status-event
                  type params payload "notification"
                  (hermes-dashboard-transport--payload-text payload)))
         (hermes-dashboard-transport--generic-display-event
          type params payload))))))

(provide 'hermes-transport)
;;; hermes-transport.el ends here
