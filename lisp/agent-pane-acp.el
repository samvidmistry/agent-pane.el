;;; agent-pane-acp.el --- ACP protocol wiring for agent-pane -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;;; Commentary:
;;
;; This module contains the ACP (Agent Client Protocol) client wiring:
;;
;; - creating and maintaining an `acp.el` client
;; - handshake (`initialize`/`authenticate`/`session/new`/`session/set_mode`)
;; - prompt sending and streaming notification handling
;; - tool call + permission request plumbing
;; - follow-up queueing while a turn is in progress
;;
;;; Code:
(require 'agent-pane-config)
(require 'agent-pane-state)
(require 'agent-pane-transcript)
(require 'agent-pane-ui)
(require 'agent-pane-util)
(require 'agent-pane-diff)
(require 'acp)
(require 'cl-lib)
(require 'map)
(require 'subr-x)
(defvar agent-pane-permission-rules-file)
(defvar agent-pane-acp-provider)
(defvar agent-pane-session-model-id)
(defvar agent-pane-acp-request-timeout-seconds)
(defvar agent-pane-permission-review-diff)
(defvar agent-pane--tool-output-full-mode)
(declare-function agent-pane--acp-command-and-params "agent-pane-util")
(declare-function agent-pane--acp-environment "agent-pane-util")
(declare-function agent-pane--set-message-queued "agent-pane-state")
(defvar agent-pane--permission-project-rules-cache :uninitialized
  "Cached project permission rules loaded from `agent-pane-permission-rules-file'.")
(defun agent-pane--append-acp-event (text &optional raw)
  "Append a low-level ACP event if configured.
TEXT is the human-readable summary.  RAW is the structured payload."
  (when agent-pane-show-raw-events
    (agent-pane--append-message* :role 'acp :text text :raw raw)
    (agent-pane--rerender)))
(defun agent-pane--auth-failure-help (err)
  "Return actionable help text for ACP authentication failure ERR."
  (let ((msg (or (map-elt err 'message) err)))
    (string-join
     (append
      (list (format "authenticate failed: %s" msg)
            "Possible next steps:")
      (pcase agent-pane-acp-provider
        ('codex
         '("- Ensure Codex is authenticated (for example, run `codex login`)."
           "- Or configure API-key auth (`OPENAI_API_KEY` or `CODEX_API_KEY`)."))
        ('copilot
         '("- Ensure Copilot CLI is signed in (for example, `copilot auth login`)."
           "- Verify `copilot --acp` works in your shell."))
        ('claude-code
         '("- Ensure Claude Code ACP is installed and authenticated."
           "- Verify `claude-code-acp` starts successfully in your shell."))
        (_
         '("- Verify the configured ACP command is installed and authenticated.")))
      '("- Re-open `agent-pane` and submit again after auth is fixed."))
     "\n")))
(defun agent-pane--fail-current-assistant (fmt &rest args)
  "Record a non-fatal ACP error in the UI.
FMT and ARGS are passed to `format'.
If there is an in-progress assistant message, write into it.  Otherwise,
append a system message."
  (let* ((idx (map-elt agent-pane--state :streaming-assistant-index))
         (text (apply #'format (concat "ACP error: " fmt) args)))
    (map-put! agent-pane--state :prompt-in-flight nil)
    (map-put! agent-pane--state :in-progress nil)
    (cond
     ((numberp idx)
      (agent-pane--set-message-text idx text)
      (agent-pane--ui-set-message-text idx text))
     (t
      (agent-pane--append-message 'system text)
      (agent-pane--ui-sync-transcript)
      (agent-pane--transcript-role-heading 'system "error")
      (agent-pane--transcript-log-session-id)
      (agent-pane--transcript-append (concat text "\n\n"))))
    (map-put! agent-pane--state :streaming-assistant-index nil)
    (agent-pane--rerender)
    (agent-pane--pump-prompt-queue)))
(defun agent-pane--ensure-acp-client ()
  "Ensure an ACP client exists in state."
  (agent-pane--ensure-state)
  (unless (map-elt agent-pane--state :client)
    (map-put! agent-pane--state :client
              (if (functionp agent-pane-acp-client-maker)
                  (funcall agent-pane-acp-client-maker (current-buffer))
                (let* ((cmd+params (agent-pane--acp-command-and-params))
                       (cmd (car cmd+params))
                       (params (cdr cmd+params)))
                  (acp-make-client
                   :context-buffer (current-buffer)
                   :command cmd
                   :command-params params
                   :environment-variables (agent-pane--acp-environment)))))))
(cl-defun agent-pane--acp-send-request
    (&key client request buffer on-success on-failure timeout-seconds)
  "Send an ACP REQUEST using CLIENT.
If CLIENT is a fake (marked with `:agent-pane-fake'), avoid starting external
processes and call the client request sender directly.
When TIMEOUT-SECONDS is a positive number, run ON-FAILURE if no response
arrives before the timeout."
  (unless client
    (error "Missing required argument: :client"))
  (unless request
    (error "Missing required argument: :request"))
  (let* ((method (map-elt request :method))
         (done nil)
         timer
         (finish-success
          (lambda (resp)
            (unless done
              (setq done t)
              (when timer
                (cancel-timer timer))
              (agent-pane--append-acp-event
               (format "← %s (ok)" method)
               resp)
              (when (functionp on-success)
                (funcall on-success resp)))))
         (finish-failure
          (lambda (err &optional raw)
            (unless done
              (setq done t)
              (when timer
                (cancel-timer timer))
              (agent-pane--append-acp-event
               (format "← %s (error): %s" method (or (map-elt err 'message) err))
               (or raw err))
              (when (functionp on-failure)
                (let ((arity (cdr (func-arity on-failure))))
                  (if (>= arity 2)
                      (funcall on-failure err raw)
                    (funcall on-failure err))))))))
    (agent-pane--append-acp-event
     (format "→ %s" method)
     request)
    (when (and (numberp timeout-seconds)
               (> timeout-seconds 0))
      (setq timer
            (run-at-time
             timeout-seconds
             nil
             (lambda ()
               (funcall finish-failure
                        `((message . ,(format "%s timed out after %ss"
                                              method
                                              timeout-seconds))
                          (code . "request_timeout")
                          (method . ,method)
                          (timeoutSeconds . ,timeout-seconds)))))))
    (if (map-elt client :agent-pane-fake)
        ;; Fake sender does not accept :buffer or :sync.
        (funcall (map-elt client :request-sender)
                 :client client
                 :request request
                 :on-success finish-success
                 :on-failure finish-failure)
      (acp-send-request
       :client client
       :request request
       :buffer buffer
       :on-success finish-success
       :on-failure finish-failure))))
(cl-defun agent-pane--acp-send-notification (&key client notification)
  "Send an ACP NOTIFICATION using CLIENT.
If CLIENT is fake (marked with `:agent-pane-fake'), call the client's
notification sender directly."
  (unless client
    (error "Missing required argument: :client"))
  (unless notification
    (error "Missing required argument: :notification"))
  (agent-pane--append-acp-event
   (format "→ %s" (map-elt notification :method))
   notification)
  (if (map-elt client :agent-pane-fake)
      (funcall (map-elt client :notification-sender)
               :client client
               :notification notification)
    (acp-send-notification
     :client client
     :notification notification)))
(cl-defun agent-pane--acp-send-response (&key client response)
  "Send an ACP RESPONSE using CLIENT.
If CLIENT is fake (marked with `:agent-pane-fake'), call the response sender
directly (fake clients typically no-op)."
  (unless client
    (error "Missing required argument: :client"))
  (unless response
    (error "Missing required argument: :response"))
  (agent-pane--append-acp-event "→ response" response)
  (if (map-elt client :agent-pane-fake)
      (funcall (map-elt client :response-sender)
               :response response)
    (acp-send-response
     :client client
     :response response)))
(defun agent-pane--subscribe ()
  "Subscribe to ACP notifications/requests/errors once."
  (agent-pane--ensure-acp-client)
  (unless (map-elt agent-pane--state :subscribed)
    (map-put! agent-pane--state :subscribed t)
    (acp-subscribe-to-errors
     :client (map-elt agent-pane--state :client)
     :buffer (current-buffer)
     :on-error (lambda (err)
                 (agent-pane--append-message*
                  :role 'system
                  :title "ACP error"
                  :text (format "%s"
                                (or (map-elt err 'message)
                                    (map-elt err 'data)
                                    err))
                  :raw err)
                 (agent-pane--rerender)))
    (acp-subscribe-to-notifications
     :client (map-elt agent-pane--state :client)
     :buffer (current-buffer)
     :on-notification (lambda (notification)
                        (agent-pane--on-notification notification)))
    (acp-subscribe-to-requests
     :client (map-elt agent-pane--state :client)
     :buffer (current-buffer)
     :on-request (lambda (request)
                   (agent-pane--on-request request)))))

(defun agent-pane--map-elt-safe (obj key)
  "Return OBJ value at KEY, or nil when OBJ is not map-like."
  (condition-case nil
      (map-elt obj key)
    (error nil)))

(defun agent-pane--map-nested-elt-safe (obj keys)
  "Return nested OBJ value at KEYS, or nil on shape mismatch."
  (condition-case nil
      (map-nested-elt obj keys)
    (error nil)))

(defun agent-pane--map-contains-key-safe (obj key)
  "Return non-nil when OBJ is map-like and has KEY."
  (condition-case nil
      (map-contains-key obj key)
    (error nil)))

(defun agent-pane--nonempty-value-p (value)
  "Return non-nil when VALUE has meaningful content."
  (cond
   ((null value) nil)
   ((stringp value) (not (string-empty-p (string-trim value))))
   ((vectorp value) (> (length value) 0))
   ((listp value) (not (null value)))
   (t t)))

(defun agent-pane--first-nonempty-value (values)
  "Return the first non-empty entry from VALUES, or nil."
  (cl-loop for value in values
           when (agent-pane--nonempty-value-p value)
           return value))

(defun agent-pane--normalize-nonempty-string (value)
  "Return VALUE as a trimmed non-empty string, or nil."
  (when (stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
        trimmed))))

(defun agent-pane--dedupe-strings (values)
  "Return VALUES without duplicates, preserving order."
  (let ((seen (make-hash-table :test 'equal))
        out)
    (dolist (value values (nreverse out))
      (when (and (stringp value)
                 (not (gethash value seen)))
        (puthash value t seen)
        (push value out)))))

(defun agent-pane--model-id-from-option (option)
  "Return a model id string from ACP config OPTION, or nil."
  (cond
   ((stringp option)
    (agent-pane--normalize-nonempty-string option))
   ((listp option)
    (agent-pane--normalize-nonempty-string
     (agent-pane--first-nonempty-value
      (list (agent-pane--map-elt-safe option 'value)
            (agent-pane--map-elt-safe option 'id)
            (agent-pane--map-elt-safe option 'modelId)
            (agent-pane--map-elt-safe option 'model_id)))))
   (t nil)))

(defun agent-pane--model-id-from-entry (entry)
  "Return a model id string from model ENTRY, or nil."
  (cond
   ((stringp entry)
    (agent-pane--normalize-nonempty-string entry))
   ((listp entry)
    (agent-pane--normalize-nonempty-string
     (agent-pane--first-nonempty-value
      (list (agent-pane--map-elt-safe entry 'id)
            (agent-pane--map-elt-safe entry 'modelId)
            (agent-pane--map-elt-safe entry 'model_id)
            (agent-pane--map-elt-safe entry 'value)))))
   (t nil)))

(defun agent-pane--extract-model-metadata (payload)
  "Extract model metadata from ACP PAYLOAD.
Return plist keys:
- `:has-config-options' when `configOptions' is present.
- `:config-options' raw config options (or nil).
- `:has-model-fields' when model metadata fields are present.
- `:ids' normalized model id list.
- `:current' normalized current model id (or nil)."
  (let* ((has-config-options
          (agent-pane--map-contains-key-safe payload 'configOptions))
         (config-options
          (agent-pane--map-elt-safe payload 'configOptions))
         (model-ids nil)
         (current-model-id nil))
    (dolist (cfg (agent-pane--listify config-options))
      (let* ((cfg-id (agent-pane--normalize-nonempty-string
                      (agent-pane--map-elt-safe cfg 'id)))
             (category (agent-pane--normalize-nonempty-string
                        (agent-pane--map-elt-safe cfg 'category)))
             (is-model-option (or (equal cfg-id "model")
                                  (equal category "model"))))
        (when is-model-option
          (let ((current
                 (agent-pane--normalize-nonempty-string
                  (agent-pane--first-nonempty-value
                   (list (agent-pane--map-elt-safe cfg 'currentValue)
                         (agent-pane--map-elt-safe cfg 'current_value)
                         (agent-pane--map-elt-safe cfg 'value))))))
            (when current
              (push current model-ids)
              (unless current-model-id
                (setq current-model-id current))))
          (dolist (option (agent-pane--listify
                           (agent-pane--first-nonempty-value
                            (list (agent-pane--map-elt-safe cfg 'options)
                                  (agent-pane--map-elt-safe cfg 'values)
                                  (agent-pane--map-elt-safe cfg 'allowedValues)))))
            (when-let ((id (agent-pane--model-id-from-option option)))
              (push id model-ids))))))
    (let* ((models (agent-pane--map-elt-safe payload 'models))
           (has-model-fields
            (or (agent-pane--map-contains-key-safe payload 'models)
                (agent-pane--map-contains-key-safe payload 'available_models)
                (agent-pane--map-contains-key-safe payload 'availableModels)
                (agent-pane--map-contains-key-safe payload 'current_model_id)
                (agent-pane--map-contains-key-safe payload 'currentModelId)
                (agent-pane--map-contains-key-safe models 'available_models)
                (agent-pane--map-contains-key-safe models 'availableModels)
                (agent-pane--map-contains-key-safe models 'current_model_id)
                (agent-pane--map-contains-key-safe models 'currentModelId)))
           (current
            (agent-pane--normalize-nonempty-string
             (agent-pane--first-nonempty-value
              (list (agent-pane--map-elt-safe payload 'current_model_id)
                    (agent-pane--map-elt-safe payload 'currentModelId)
                    (agent-pane--map-elt-safe models 'current_model_id)
                    (agent-pane--map-elt-safe models 'currentModelId)))))
           (available
            (agent-pane--first-nonempty-value
             (list (agent-pane--map-elt-safe payload 'available_models)
                   (agent-pane--map-elt-safe payload 'availableModels)
                   (agent-pane--map-elt-safe models 'available_models)
                   (agent-pane--map-elt-safe models 'availableModels)))))
      (when current
        (push current model-ids)
        (unless current-model-id
          (setq current-model-id current)))
      (dolist (entry (agent-pane--listify available))
        (when-let ((id (agent-pane--model-id-from-entry entry)))
          (push id model-ids)))
      (list :has-config-options has-config-options
            :config-options config-options
            :has-model-fields has-model-fields
            :ids (agent-pane--dedupe-strings (nreverse model-ids))
            :current current-model-id))))

(defun agent-pane--update-model-metadata (payload)
  "Update model-related state fields from ACP PAYLOAD."
  (let* ((meta (agent-pane--extract-model-metadata payload))
         (has-config-options (plist-get meta :has-config-options))
         (has-model-fields (plist-get meta :has-model-fields))
         (ids (plist-get meta :ids))
         (current (plist-get meta :current)))
    (when has-config-options
      (map-put! agent-pane--state :session-config-options
                (plist-get meta :config-options)))
    (when (or has-config-options has-model-fields)
      (map-put! agent-pane--state :available-model-ids ids)
      (map-put! agent-pane--state :current-model-id current))))

(defun agent-pane--available-model-ids ()
  "Return known ACP model ids for the current chat buffer."
  (agent-pane--ensure-state)
  (agent-pane--listify (map-elt agent-pane--state :available-model-ids)))

(defun agent-pane--tool-call-content-items (content)
  "Normalize tool call CONTENT into a list of content items."
  (cond
   ((null content) nil)
   ((vectorp content) (append content nil))
   ((stringp content) (list content))
   ;; List of content entries.
   ((and (listp content)
         (listp (car-safe content))
         (or (null (car content))
             (consp (car (car content)))
             (keywordp (car (car content)))))
    content)
   ((listp content) (list content))
   (t (list content))))

(defun agent-pane--content-field-text (obj keys)
  "Return textual representation for first non-empty KEY in KEYS from OBJ."
  (let ((value
         (agent-pane--first-nonempty-value
          (mapcar (lambda (key) (agent-pane--map-elt-safe obj key)) keys))))
    (when value
      (agent-pane--tool-call-content->string value))))

(defun agent-pane--tool-call-content->string (content)
  "Format tool call CONTENT into a plain string.
Structured diff entries are omitted from textual output and handled separately."
  (let ((items (agent-pane--tool-call-content-items content)))
    (string-join
     (delq nil
           (mapcar
            (lambda (item)
              (cond
               ((null item) nil)
               ((stringp item) item)
               ;; Avoid dumping structured diff objects into tool output text.
               ((and (listp item)
                     (equal (agent-pane--map-elt-safe item 'type) "diff"))
                nil)
               ((stringp (agent-pane--map-nested-elt-safe item '(content text)))
                (agent-pane--map-nested-elt-safe item '(content text)))
               ((stringp (agent-pane--map-elt-safe item 'text))
                (agent-pane--map-elt-safe item 'text))
               ((stringp (agent-pane--map-elt-safe item 'thinkingText))
                (agent-pane--map-elt-safe item 'thinkingText))
               ((stringp (agent-pane--map-elt-safe item 'message))
                (agent-pane--map-elt-safe item 'message))
               ((let ((fallback
                       (agent-pane--content-field-text
                        item
                        '(summary thinking reasoning body delta output result))))
                  (and (stringp fallback)
                       (not (string-empty-p (string-trim fallback)))
                       fallback)))
               ;; Some servers put the text under (content (text . "..."))
               ((let ((c (and (listp item) (agent-pane--map-elt-safe item 'content))))
                  (cond
                   ((stringp c) c)
                   ((and (listp c) (equal (agent-pane--map-elt-safe c 'type) "diff")) nil)
                   ((and (listp c) (stringp (agent-pane--map-elt-safe c 'text)))
                    (agent-pane--map-elt-safe c 'text))
                   ((null c) nil)
                   (t (agent-pane--tool-call-content->string c)))))
               (t (agent-pane--pp-to-string item))))
            items))
     "\n\n")))

(defun agent-pane--session-update-content->text (update)
  "Extract text content from session UPDATE.
Handles wrapped list/vector payload forms used by different ACP providers."
  (let* ((direct-text
          (agent-pane--first-nonempty-value
           (list (agent-pane--map-elt-safe update 'text)
                 (agent-pane--map-elt-safe update 'thinkingText)
                 (agent-pane--map-elt-safe update 'message)
                 (agent-pane--map-elt-safe update 'summary))))
         (content (agent-pane--map-elt-safe update 'content))
         (text (or (and (stringp direct-text) direct-text)
                   (and content
                        (agent-pane--tool-call-content->string content)))))
    (when (and (stringp text)
               (not (string-empty-p (string-trim text))))
      text)))

(defun agent-pane--tool-raw-input (update)
  "Return raw input payload from UPDATE, if present."
  (or (agent-pane--map-elt-safe update 'rawInput)
      (agent-pane--map-elt-safe update 'raw_input)
      (agent-pane--map-elt-safe update 'input)))

(defun agent-pane--tool-raw-input-command (update)
  "Return tool command from UPDATE raw input when available."
  (let ((raw-input (agent-pane--tool-raw-input update)))
    (agent-pane--first-nonempty-value
     (list (agent-pane--map-elt-safe raw-input 'command)
           (agent-pane--map-elt-safe raw-input 'tool)
           (agent-pane--map-elt-safe raw-input 'name)
           (agent-pane--map-nested-elt-safe raw-input '(parameters command))
           (agent-pane--map-nested-elt-safe raw-input '(params command))
           (agent-pane--map-nested-elt-safe raw-input '(arguments command))
           (agent-pane--map-nested-elt-safe raw-input '(args command))
           (agent-pane--map-nested-elt-safe raw-input '(input command))))))

(defun agent-pane--tool-raw-input-description (update)
  "Return tool description from UPDATE raw input when available."
  (let ((raw-input (agent-pane--tool-raw-input update)))
    (agent-pane--first-nonempty-value
     (list (agent-pane--map-elt-safe raw-input 'description)
           (agent-pane--map-elt-safe raw-input 'desc)
           (agent-pane--map-nested-elt-safe raw-input '(parameters description))
           (agent-pane--map-nested-elt-safe raw-input '(params description))
           (agent-pane--map-nested-elt-safe raw-input '(arguments description))
           (agent-pane--map-nested-elt-safe raw-input '(args description))
           (agent-pane--map-nested-elt-safe raw-input '(input description))))))

(defun agent-pane--tool-raw-input-extra-params (raw-input)
  "Return RAW-INPUT fields that look like tool parameters."
  (let ((pairs
         (cond
          ((and (listp raw-input)
                (consp (car-safe raw-input))
                (symbolp (caar raw-input)))
           raw-input)
          ((and (consp raw-input)
                (symbolp (car raw-input)))
           (list raw-input))
          (t nil))))
    (when pairs
      (let ((filtered
             (cl-remove-if
              (lambda (pair)
                (memq (car-safe pair)
                      '(command description desc tool name title kind type)))
              pairs)))
        (when (agent-pane--nonempty-value-p filtered)
          filtered)))))

(defun agent-pane--tool-raw-input-params (update)
  "Extract structured tool parameters from UPDATE raw input."
  (let* ((raw-input (agent-pane--tool-raw-input update))
         (explicit
          (agent-pane--first-nonempty-value
           (list (agent-pane--map-elt-safe raw-input 'parameters)
                 (agent-pane--map-elt-safe raw-input 'params)
                 (agent-pane--map-elt-safe raw-input 'arguments)
                 (agent-pane--map-elt-safe raw-input 'args)
                 (agent-pane--map-elt-safe raw-input 'input))))
         (extra (agent-pane--tool-raw-input-extra-params raw-input)))
    (or explicit
        extra
        (and (stringp raw-input)
             (not (string-empty-p (string-trim raw-input)))
             raw-input))))

(defun agent-pane--tool-params->string (params)
  "Format tool invocation PARAMS into display text."
  (cond
   ((null params) nil)
   ((stringp params)
    (let ((trimmed (string-trim params)))
      (unless (string-empty-p trimmed)
        trimmed)))
   (t (agent-pane--pp-to-string params))))

(defun agent-pane--format-locations (locations)
  "Format LOCATIONS (ACP locations list) as a compact string."
  (let ((locs (agent-pane--listify locations)))
    (string-join
     (delq nil
           (mapcar (lambda (loc)
                     (or (map-elt loc 'path)
                         (map-elt loc 'uri)
                         (and loc (agent-pane--pp-to-string loc))))
                    locs))
     ", ")))

(defun agent-pane--opaque-tool-call-id-p (value)
  "Return non-nil when VALUE is an opaque call id."
  (and (stringp value)
       (string-match-p (rx string-start (or "call_" "call-") (+ any) string-end)
                       value)))

(defun agent-pane--tool-entry-command (entry)
  "Return best-effort command string extracted from tool-call ENTRY."
  (let* ((cmd0 (and (stringp (plist-get entry :command))
                    (string-trim (plist-get entry :command))))
         (params (plist-get entry :params)))
    (or (and cmd0 (not (string-empty-p cmd0)) cmd0)
        (and (listp params)
             (agent-pane--first-nonempty-value
              (list (agent-pane--map-elt-safe params 'command)
                    (agent-pane--map-elt-safe params 'tool)
                    (agent-pane--map-elt-safe params 'name)
                    (agent-pane--map-nested-elt-safe params '(input command))
                    (agent-pane--map-nested-elt-safe params '(args command))
                    (agent-pane--map-nested-elt-safe params '(arguments command))
                    (agent-pane--map-nested-elt-safe params '(params command))
                    (agent-pane--map-nested-elt-safe params '(parameters command))))))))

(defun agent-pane--tool-entry-label (entry)
  "Return best-effort short label extracted from tool-call ENTRY."
  (let* ((id (or (plist-get entry :toolCallId) ""))
         (title0 (string-trim (or (plist-get entry :title) "")))
         (kind0 (string-trim (or (plist-get entry :kind) "")))
         (title (and (not (string-empty-p title0))
                     (not (equal title0 id))
                     (not (agent-pane--opaque-tool-call-id-p title0))
                     title0))
         (kind (and (not (string-empty-p kind0)) kind0)))
    (or title kind)))

(defun agent-pane--tool-call-display-title (entry)
  "Return a compact tool-call title for ENTRY.
This is what we show in the UI label after `Tool'.
Prefer showing the command (or `tool: command') over opaque ids like
`call_...'."
  (let* ((id (or (plist-get entry :toolCallId) ""))
         (title (or (agent-pane--tool-entry-label entry) ""))
         (cmd0 (agent-pane--tool-entry-command entry))
         (cmd (if (stringp cmd0) (string-trim cmd0) "")))
    (cond
     ((and (not (string-empty-p title)) (not (string-empty-p cmd)))
      (if (string-match-p (regexp-quote cmd) title)
          title
        (format "%s: %s" title cmd)))
     ((not (string-empty-p cmd)) cmd)
     ((not (string-empty-p title)) title)
     ((agent-pane--opaque-tool-call-id-p id) "tool call")
     (t id))))
(defvar agent-pane-tool-output-preview-lines)

(defun agent-pane--tool-tail-lines (text n)
  "Return plist with last N lines from TEXT and omitted count."
  (let* ((lines (split-string (or text "") "\n" nil))
         (total (length lines))
         (keep (max 0 (min n total)))
         (start (- total keep))
         (tail (if (> keep 0) (nthcdr start lines) nil))
         (omitted (max 0 start)))
    (list :tail (string-join tail "\n")
          :omitted omitted)))

(defun agent-pane--merge-tool-output (existing incoming)
  "Merge EXISTING tool output with INCOMING chunk or snapshot.

When INCOMING looks like a full snapshot (starts with EXISTING), use it.
When INCOMING is a true suffix chunk, append it.
Empty INCOMING keeps EXISTING unchanged."
  (let ((old (or existing ""))
        (new (or incoming "")))
    (cond
     ((string-empty-p new) old)
     ((string-empty-p old) new)
     ((string-prefix-p old new) new)
     ((string-prefix-p new old) old)
     (t (concat old new)))))

(defun agent-pane--tool-invocation-string (entry)
  "Return concise invocation text for tool-call ENTRY."
  (let* ((tool0 (or (agent-pane--tool-entry-label entry)
                    "tool"))
         (tool (string-trim tool0))
         (cmd0 (agent-pane--tool-entry-command entry))
         (args (string-trim (or cmd0
                                (plist-get entry :description)
                                ""))))
    (if (string-empty-p args)
        tool
      (format "%s %s" tool args))))

(defun agent-pane--tool-call-entry-to-text (entry &optional force-full-output)
  "Render a concise tool-call ENTRY plist as a message text.
When FORCE-FULL-OUTPUT is non-nil, include the complete output body."
  (let* ((diff (plist-get entry :diff))
         (params (agent-pane--tool-params->string (plist-get entry :params)))
         (output (string-trim-right (or (plist-get entry :output) "")))
         (show-full (or force-full-output agent-pane--tool-output-full-mode))
         (preview (unless show-full
                    (agent-pane--tool-tail-lines output agent-pane-tool-output-preview-lines)))
         (tail (or (plist-get preview :tail) ""))
         (omitted (or (plist-get preview :omitted) 0)))
    (string-trim-right
     (string-join
      (delq nil
            (list (format "tool: %s" (agent-pane--tool-invocation-string entry))
                  (when params
                    (concat "params:\n" params))
                  (when diff
                    (format "changes: %s (view: C-c C-d)"
                            (agent-pane--diff-summary diff)))
                  (when (not (string-empty-p output))
                    (if show-full
                        (concat "output (full):\n" output)
                      (concat "output (latest "
                              (number-to-string agent-pane-tool-output-preview-lines)
                              " lines"
                              (if (> omitted 0)
                                  (format ", %d omitted" omitted)
                                "")
                              "):\n"
                              (if (> omitted 0)
                                  (concat "...\n" tail)
                                tail))))))
      "\n\n"))))

(defun agent-pane--refresh-tool-call-messages ()
  "Re-render all tool-call message bodies from current tool-call state."
  (let ((idx-table (map-elt agent-pane--state :tool-call-msg-index))
        (tool-table (map-elt agent-pane--state :tool-calls))
        (messages (map-elt agent-pane--state :messages)))
    (when (and (hash-table-p idx-table) (hash-table-p tool-table))
      (maphash
       (lambda (tool-id idx)
         (when (and (numberp idx) (nth idx messages))
           (when-let ((entry (gethash tool-id tool-table)))
             (let* ((old-title (map-elt (nth idx messages) :title))
                    (new-title (agent-pane--tool-call-display-title entry))
                    (body (agent-pane--tool-call-entry-to-text entry)))
               (agent-pane--set-message-title idx new-title)
               (agent-pane--set-message-text idx body)
               (if (equal old-title new-title)
                   (agent-pane--ui-set-message-text idx body)
                 (agent-pane--ui-rerender-message idx))))))
       idx-table))))
(defun agent-pane--upsert-tool-call-message (tool-call-id)
  "Ensure there is a message for TOOL-CALL-ID.
Return a cons cell (IDX . CREATEDP)."
  (let* ((idx-table (map-elt agent-pane--state :tool-call-msg-index))
         (existing (and (hash-table-p idx-table)
                        (gethash tool-call-id idx-table))))
    (if (numberp existing)
        (cons existing nil)
      (let ((idx (agent-pane--append-message* :role 'tool :title tool-call-id :text "")))
        (puthash tool-call-id idx idx-table)
        (cons idx t)))))
(defun agent-pane--on-notification (notification)
  "Handle ACP NOTIFICATION."
  (let-alist notification
    (cond
     ((equal .method "session/update")
      (let* ((sid (map-nested-elt notification '(params sessionId)))
             (update (map-nested-elt notification '(params update)))
             (kind (map-elt update 'sessionUpdate))
             (prev-kind (map-elt agent-pane--state :last-session-update-kind)))
        (map-put! agent-pane--state :last-notification-session-id sid)
        ;; Only transition to streaming for updates tied to an in-flight prompt.
        ;; Codex can emit session updates during handshake (for example config
        ;; updates) before the first prompt is sent.
        (when (and (eq (map-elt agent-pane--state :in-progress) 'waiting)
                   (map-elt agent-pane--state :prompt-in-flight))
          (map-put! agent-pane--state :in-progress 'streaming)
          (agent-pane--ui-update-status-line))
        (map-put! agent-pane--state :last-session-update-kind kind)
        (cond
         ;; Agent message chunks (main visible output).
         ((equal kind "agent_message_chunk")
          (let* ((text (agent-pane--session-update-content->text update))
                 (idx (map-elt agent-pane--state :streaming-assistant-index)))
            (when (stringp text)
              ;; Create a new assistant message when starting a new chunk group.
              ;; This keeps messages interleaved in arrival order.
              (unless (and (numberp idx)
                           (equal prev-kind "agent_message_chunk"))
                (setq idx (agent-pane--append-message* :role 'assistant :text "" :raw update))
                (map-put! agent-pane--state :streaming-assistant-index idx)
                (agent-pane--ui-sync-transcript)
                (agent-pane--transcript-role-heading 'assistant)
                (agent-pane--transcript-log-session-id))
              (agent-pane--append-to-message-text idx text)
              (agent-pane--set-message-raw idx update)
              (agent-pane--ui-append-to-message-text idx text)
              (agent-pane--transcript-append text))))
         ;; Agent thought chunks (debug / reasoning stream).
         ((equal kind "agent_thought_chunk")
          (when agent-pane-show-thoughts
            (let* ((text (agent-pane--session-update-content->text update))
                   (idx (map-elt agent-pane--state :streaming-thought-index)))
              (when (stringp text)
                (unless (and (numberp idx)
                             (equal prev-kind "agent_thought_chunk"))
                  (setq idx (agent-pane--append-message* :role 'thought :text "" :raw update))
                  (map-put! agent-pane--state :streaming-thought-index idx)
                  (agent-pane--ui-sync-transcript)
                  (when agent-pane-transcript-include-thoughts
                    (agent-pane--transcript-role-heading 'thought)
                    (agent-pane--transcript-log-session-id)))
                (agent-pane--append-to-message-text idx text)
                (agent-pane--set-message-raw idx update)
                (agent-pane--ui-append-to-message-text idx text)
                (when agent-pane-transcript-include-thoughts
                  (agent-pane--transcript-append text))))))
         ;; Server-provided user echo.
         ((equal kind "user_message_chunk")
          (let ((text (agent-pane--session-update-content->text update)))
            (when (stringp text)
              (agent-pane--append-message* :role 'user :title "server" :text text :raw update)
              (agent-pane--ui-sync-transcript)
              (agent-pane--transcript-role-heading 'user "server")
              (agent-pane--transcript-log-session-id)
              (agent-pane--transcript-append (concat text "\n\n")))))
         ;; Tool calls.
         ((equal kind "tool_call")
          (when agent-pane-show-tool-calls
            (let* ((id (map-elt update 'toolCallId))
                   (tool-table (map-elt agent-pane--state :tool-calls))
                   (entry (or (and id (gethash id tool-table)) (list :toolCallId id)))
                   (title (map-elt update 'title))
                   (status (map-elt update 'status))
                   (tool-kind (map-elt update 'kind))
                   (command (agent-pane--tool-raw-input-command update))
                   (desc (agent-pane--tool-raw-input-description update))
                   (params (agent-pane--tool-raw-input-params update))
                   (locations (map-elt update 'locations))
                   (diff (agent-pane--extract-diff-info update)))
              (setq entry (plist-put entry :toolCallId id))
              (setq entry (plist-put entry :title title))
              (setq entry (plist-put entry :status status))
              (setq entry (plist-put entry :kind tool-kind))
               (setq entry (plist-put entry :command command))
               (setq entry (plist-put entry :description desc))
               (when params
                 (setq entry (plist-put entry :params params)))
               (setq entry (plist-put entry :locations locations))
               (when diff
                 (setq entry (plist-put entry :diff diff)))
              (setq entry (plist-put entry :raw update))
              (puthash id entry tool-table)
              (pcase-let* ((`(,idx . ,created) (agent-pane--upsert-tool-call-message id))
                           (old-title (map-elt (nth idx (map-elt agent-pane--state :messages)) :title))
                           (new-title (agent-pane--tool-call-display-title entry))
                           (body (agent-pane--tool-call-entry-to-text entry)))
                (when created
                  (agent-pane--ui-sync-transcript))
                (agent-pane--set-message-title idx new-title)
                (agent-pane--set-message-raw idx update)
                (agent-pane--set-message-text idx body)
                (if (or created (not (equal old-title new-title)))
                    (agent-pane--ui-rerender-message idx)
                  (agent-pane--ui-set-message-text idx body))))))
         ((equal kind "tool_call_update")
          (when agent-pane-show-tool-calls
            (let* ((id (map-elt update 'toolCallId))
                   (tool-table (map-elt agent-pane--state :tool-calls))
                   (entry (or (and id (gethash id tool-table)) (list :toolCallId id)))
                   (status (map-elt update 'status))
                   (content (map-elt update 'content))
                   (output (agent-pane--tool-call-content->string content))
                   (command (agent-pane--tool-raw-input-command update))
                   (desc (agent-pane--tool-raw-input-description update))
                   (params (agent-pane--tool-raw-input-params update))
                   (diff (agent-pane--extract-diff-info update)))
               (setq entry (plist-put entry :toolCallId id))
               (setq entry (plist-put entry :status status))
               (when command
                 (setq entry (plist-put entry :command command)))
               (when desc
                 (setq entry (plist-put entry :description desc)))
               (when params
                 (setq entry (plist-put entry :params params)))
               (when diff
                 (setq entry (plist-put entry :diff diff)))
              (setq entry (plist-put entry :output
                                     (agent-pane--merge-tool-output
                                      (plist-get entry :output)
                                      output)))
              (setq entry (plist-put entry :raw update))
              ;; If the tool call finished, log once to transcript.
              (when (and (stringp status)
                         (member status '("completed" "failed"))
                         (not (plist-get entry :transcript-logged)))
                (setq entry (plist-put entry :transcript-logged t))
                (agent-pane--transcript-role-heading
                 'tool
                 (format "%s: %s" status (or (plist-get entry :title) id)))
                (agent-pane--transcript-log-session-id)
                (agent-pane--transcript-append
                 (concat (agent-pane--tool-call-entry-to-text entry t) "\n\n")))
              (puthash id entry tool-table)
              (pcase-let* ((`(,idx . ,created) (agent-pane--upsert-tool-call-message id))
                           (old-title (map-elt (nth idx (map-elt agent-pane--state :messages)) :title))
                           (new-title (agent-pane--tool-call-display-title entry))
                           (body (agent-pane--tool-call-entry-to-text entry)))
                (when created
                  (agent-pane--ui-sync-transcript))
                (agent-pane--set-message-title idx new-title)
                (agent-pane--set-message-raw idx update)
                (agent-pane--set-message-text idx body)
                (if (or created (not (equal old-title new-title)))
                    (agent-pane--ui-rerender-message idx)
                  (agent-pane--ui-set-message-text idx body))))))
         ;; Plan.
         ((equal kind "plan")
          (let ((entries (map-elt update 'entries)))
            (agent-pane--append-message* :role 'system
                                         :title "Plan"
                                         :text (agent-pane--pp-to-string entries)
                                         :raw update)
            (agent-pane--ui-sync-transcript)))
         ;; Command updates.
         ((equal kind "available_commands_update")
          (let ((cmds (map-elt update 'availableCommands)))
            (map-put! agent-pane--state :available-commands cmds)
            (agent-pane--append-message* :role 'acp
                                         :title "available_commands_update"
                                         :text (agent-pane--pp-to-string cmds)
                                         :raw update)
            (agent-pane--ui-sync-transcript)))
         ;; Config option updates.
         ((equal kind "config_options_update")
          (let* ((config-options (agent-pane--first-nonempty-value
                                  (list (agent-pane--map-elt-safe update 'configOptions)
                                        (agent-pane--map-elt-safe update 'config_options))))
                 (payload (if config-options
                              `((configOptions . ,config-options))
                            update)))
            (agent-pane--update-model-metadata payload)
            (agent-pane--append-message* :role 'acp
                                         :title "config_options_update"
                                         :text (agent-pane--pp-to-string config-options)
                                         :raw update)
            (agent-pane--ui-sync-transcript)))
         ;; Fallback: show update kind + raw payload.
         (t
          (agent-pane--append-message*
           :role 'acp
           :title (or kind "session/update")
           :text (agent-pane--pp-to-string update)
           :raw update)
          (agent-pane--ui-sync-transcript)))))
     (t
      (agent-pane--append-message*
       :role 'acp
       :title (or .method "notification")
       :text (agent-pane--pp-to-string notification)
       :raw notification)
      (agent-pane--ui-sync-transcript)))))
(defun agent-pane--permission-request->text (request &optional chosen)
  "Format a session/request_permission REQUEST.
CHOSEN, when non-nil, is the selected optionId (or the symbol `cancel')."
  (let* ((tool-call (map-nested-elt request '(params toolCall)))
         (options (agent-pane--listify (map-nested-elt request '(params options))))
         (tool-id (map-elt tool-call 'toolCallId))
         (title (map-elt tool-call 'title))
         (kind (map-elt tool-call 'kind))
         (status (map-elt tool-call 'status))
         (diff (agent-pane--extract-diff-info tool-call))
         (opts-text
          (string-join
           (mapcar (lambda (opt)
                     (format "- %s (%s) optionId=%s"
                             (or (map-elt opt 'name) "")
                             (or (map-elt opt 'kind) "")
                             (or (map-elt opt 'optionId) "")))
                   options)
           "\n")))
    (string-join
     (delq nil
           (list (format "toolCallId: %s" tool-id)
                 (when title (format "title: %s" title))
                 (when kind (format "kind: %s" kind))
                 (when status (format "status: %s" status))
                 (when diff (format "changes: %s (view: C-c C-d)"
                                    (agent-pane--diff-summary diff)))
                 (when chosen (format "chosen: %s" chosen))
                 "\noptions:"
                 opts-text))
     "\n")))
(defun agent-pane--permission-preferred-allow-option-id (opts)
  "Pick the best allow option id from OPTS, or nil."
  (or (cl-loop for kind in '("allow_always" "allow_once")
               thereis (cl-loop for opt in opts
                                when (equal (map-elt opt 'kind) kind)
                                return (map-elt opt 'optionId)))
      (map-elt (car-safe opts) 'optionId)))
(defun agent-pane--permission-tool-key (tool-call)
  "Build a stable key for TOOL-CALL used in permission caches."
  (let ((kind (string-trim (or (map-elt tool-call 'kind) "")))
        (title (string-trim (or (map-elt tool-call 'title) "")))
        (id (string-trim (or (map-elt tool-call 'toolCallId) ""))))
    (cond
     ((not (string-empty-p kind)) (format "kind:%s" kind))
     ((not (string-empty-p title)) (format "title:%s" title))
     ((not (string-empty-p id)) (format "id:%s" id))
     (t "tool:unknown"))))
(defun agent-pane--permission-project-root ()
  "Return canonical project root used for permission rule scoping."
  (directory-file-name (expand-file-name default-directory)))
(defun agent-pane--permission-load-project-rules ()
  "Load project permission rules from disk if needed."
  (when (eq agent-pane--permission-project-rules-cache :uninitialized)
    (setq agent-pane--permission-project-rules-cache
          (condition-case _err
              (if (file-exists-p agent-pane-permission-rules-file)
                  (with-temp-buffer
                    (insert-file-contents agent-pane-permission-rules-file)
                    (goto-char (point-min))
                    (let ((obj (read (current-buffer))))
                      (if (listp obj) obj nil)))
                nil)
            (error nil))))
  agent-pane--permission-project-rules-cache)
(defun agent-pane--permission-save-project-rules ()
  "Persist project permission rules to `agent-pane-permission-rules-file'."
  (make-directory (file-name-directory agent-pane-permission-rules-file) t)
  (with-temp-file agent-pane-permission-rules-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 (or agent-pane--permission-project-rules-cache nil) (current-buffer))
      (insert "\n"))))
(defun agent-pane--permission-project-rule-option-id (project-root tool-key)
  "Return stored option id for PROJECT-ROOT and TOOL-KEY, if any."
  (let ((rules (agent-pane--permission-load-project-rules)))
    (when-let ((rule (cl-find-if
                      (lambda (r)
                        (and (equal (plist-get r :project-root) project-root)
                             (equal (plist-get r :tool-key) tool-key)))
                      rules)))
      (plist-get rule :option-id))))
(defun agent-pane--permission-upsert-project-rule (project-root tool-key option-id)
  "Insert or update a rule for PROJECT-ROOT and TOOL-KEY with OPTION-ID."
  (let* ((rules (copy-sequence (agent-pane--permission-load-project-rules)))
         (existing (cl-find-if
                    (lambda (r)
                      (and (equal (plist-get r :project-root) project-root)
                           (equal (plist-get r :tool-key) tool-key)))
                    rules))
         (updated (if existing
                      (mapcar (lambda (r)
                                (if (eq r existing)
                                    (plist-put (copy-sequence r) :option-id option-id)
                                  r))
                              rules)
                    (append rules (list (list :project-root project-root
                                              :tool-key tool-key
                                              :option-id option-id))))))
    (setq agent-pane--permission-project-rules-cache updated)
    (agent-pane--permission-save-project-rules)))
(defun agent-pane--permission-prompt-decision (request opts)
  "Prompt user for a permission decision for REQUEST with OPTS.
Return plist keys:
- `:action' => `allow' or `cancel'
- `:option-id' when action is `allow'
- optional `:persist' => `session' or `project'"
  (let* ((tool-call (map-nested-elt request '(params toolCall)))
         (tool-key (agent-pane--permission-tool-key tool-call))
         (allow-option-id (agent-pane--permission-preferred-allow-option-id opts))
         (diff (agent-pane--extract-diff-info tool-call))
         (review-label "Review diff in Emacs")
         (cancel-value (list :action 'cancel :source 'prompt-cancel)))
    (catch 'done
      (while t
        (let* ((choices
                (append
                 (mapcar (lambda (opt)
                           (let* ((name (or (map-elt opt 'name) "option"))
                                  (kind (or (map-elt opt 'kind) ""))
                                  (id (or (map-elt opt 'optionId) ""))
                                  (label (format "Choose %s (%s, optionId=%s)" name kind id)))
                             (cons label (list :action 'allow :option-id id :source 'prompt))))
                         opts)
                 (when allow-option-id
                   (list (cons "Always allow this tool in current session"
                               (list :action 'allow :option-id allow-option-id
                                     :persist 'session :source 'session-rule))
                         (cons "Always allow this tool in this project"
                               (list :action 'allow :option-id allow-option-id
                                     :persist 'project :source 'project-rule))))
                 (when (and agent-pane-permission-review-diff diff)
                   (list (cons review-label :review-diff)))
                 (list (cons "Cancel" cancel-value))))
               (picked (completing-read
                        (format "Permission (%s): " tool-key)
                        (mapcar #'car choices)
                        nil t nil nil "Cancel"))
               (picked-value (cdr (assoc picked choices))))
          (if (eq picked-value :review-diff)
              (condition-case err
                  (agent-pane--show-diff
                   diff
                   (format "Permission review: %s" (agent-pane--diff-summary diff)))
                (error
                 (message "Failed to open diff: %s" err)))
            (throw 'done (or picked-value cancel-value))))))))
(defun agent-pane--permission-decide (request opts)
  "Return permission decision plist for REQUEST and OPTS."
  (let* ((tool-call (map-nested-elt request '(params toolCall)))
         (tool-key (agent-pane--permission-tool-key tool-call))
         (session-table (map-elt agent-pane--state :permission-session-rules))
         (session-option (and (hash-table-p session-table)
                              (gethash tool-key session-table)))
         (project-root (agent-pane--permission-project-root))
         (project-option (agent-pane--permission-project-rule-option-id project-root tool-key))
         (allow-option (agent-pane--permission-preferred-allow-option-id opts)))
    (cond
     ((eq agent-pane-permission-policy 'auto-cancel)
      (list :action 'cancel :source 'policy-auto-cancel))
     ((and (eq agent-pane-permission-policy 'auto-allow) allow-option)
      (list :action 'allow :option-id allow-option :source 'policy-auto-allow))
     ((and (eq agent-pane-permission-policy 'prompt) session-option)
      (list :action 'allow :option-id session-option :source 'session-rule))
     ((and (eq agent-pane-permission-policy 'prompt) project-option)
      (list :action 'allow :option-id project-option :source 'project-rule))
     ((eq agent-pane-permission-policy 'prompt)
      (if noninteractive
          (list :action 'cancel :source 'noninteractive)
        (agent-pane--permission-prompt-decision request opts)))
     ((and allow-option)
      (list :action 'allow :option-id allow-option :source 'fallback-auto))
     (t
      (list :action 'cancel :source 'fallback-cancel)))))
(defun agent-pane--permission-apply-persistence (decision request)
  "Persist DECISION rules for REQUEST when requested."
  (let ((persist (plist-get decision :persist))
        (option-id (plist-get decision :option-id))
        (tool-call (map-nested-elt request '(params toolCall))))
    (when (and (eq (plist-get decision :action) 'allow)
               option-id
               persist)
      (pcase persist
        ('session
         (let ((table (map-elt agent-pane--state :permission-session-rules))
               (tool-key (agent-pane--permission-tool-key tool-call)))
           (when (hash-table-p table)
             (puthash tool-key option-id table))))
        ('project
         (agent-pane--permission-upsert-project-rule
          (agent-pane--permission-project-root)
          (agent-pane--permission-tool-key tool-call)
          option-id))))))
(defun agent-pane--on-request (request)
  "Handle ACP server-to-client REQUEST."
  (let-alist request
    (cond
     ((equal .method "session/request_permission")
      (condition-case err
          (let* ((client (map-elt agent-pane--state :client))
                 (opts (agent-pane--listify (map-nested-elt request '(params options))))
                 (tool-call (map-nested-elt request '(params toolCall)))
                 (tool-call-id (map-elt tool-call 'toolCallId))
                 (tool-table (map-elt agent-pane--state :tool-calls))
                 (idx-table (map-elt agent-pane--state :permission-msg-index))
                 (msg-idx (and tool-call-id (gethash tool-call-id idx-table)))
                 (decision (agent-pane--permission-decide request opts))
                 (decision-action (plist-get decision :action))
                 (decision-option-id (plist-get decision :option-id))
                 (decision-source (or (plist-get decision :source) 'unknown))
                 (chosen-text (if (eq decision-action 'allow)
                                  (format "%s (%s)" decision-option-id decision-source)
                                  (format "cancel (%s)" decision-source))))
            ;; Permission requests often carry richer tool metadata than earlier
            ;; updates; merge it so tool labels don't fall back to opaque call ids.
            (when (and agent-pane-show-tool-calls
                       (hash-table-p tool-table)
                       (stringp tool-call-id))
              (let* ((entry (or (gethash tool-call-id tool-table)
                                (list :toolCallId tool-call-id)))
                     (title (map-elt tool-call 'title))
                     (status (map-elt tool-call 'status))
                     (tool-kind (map-elt tool-call 'kind))
                     (command (agent-pane--tool-raw-input-command tool-call))
                     (desc (agent-pane--tool-raw-input-description tool-call))
                     (params (agent-pane--tool-raw-input-params tool-call))
                     (diff (agent-pane--extract-diff-info tool-call)))
                (setq entry (plist-put entry :toolCallId tool-call-id))
                (when title
                  (setq entry (plist-put entry :title title)))
                (when status
                  (setq entry (plist-put entry :status status)))
                (when tool-kind
                  (setq entry (plist-put entry :kind tool-kind)))
                (when command
                  (setq entry (plist-put entry :command command)))
                (when desc
                  (setq entry (plist-put entry :description desc)))
                (when params
                  (setq entry (plist-put entry :params params)))
                (when diff
                  (setq entry (plist-put entry :diff diff)))
                (setq entry (plist-put entry :raw tool-call))
                (puthash tool-call-id entry tool-table)
                (pcase-let* ((`(,tool-idx . ,created) (agent-pane--upsert-tool-call-message tool-call-id))
                             (old-title (map-elt (nth tool-idx (map-elt agent-pane--state :messages)) :title))
                             (new-title (agent-pane--tool-call-display-title entry))
                             (body (agent-pane--tool-call-entry-to-text entry)))
                  (when created
                    (agent-pane--ui-sync-transcript))
                  (agent-pane--set-message-title tool-idx new-title)
                  (agent-pane--set-message-raw tool-idx tool-call)
                  (agent-pane--set-message-text tool-idx body)
                  (if (or created (not (equal old-title new-title)))
                      (agent-pane--ui-rerender-message tool-idx)
                    (agent-pane--ui-set-message-text tool-idx body)))))
            (unless (numberp msg-idx)
              (setq msg-idx
                    (agent-pane--append-message*
                     :role 'system
                     :title "Permission request"
                     :text (agent-pane--permission-request->text request)
                     :raw request))
              (when tool-call-id
                (puthash tool-call-id msg-idx idx-table))
              (agent-pane--ui-sync-transcript))
            (agent-pane--permission-apply-persistence decision request)
            (let ((body (agent-pane--permission-request->text request chosen-text)))
              (agent-pane--set-message-raw msg-idx request)
              (agent-pane--set-message-text msg-idx body)
              (agent-pane--ui-set-message-text msg-idx body)
              (agent-pane--transcript-role-heading 'system "permission")
              (agent-pane--transcript-log-session-id)
              (agent-pane--transcript-append (concat body "\n\n")))
            (agent-pane--acp-send-response
             :client client
             :response
             (if (eq decision-action 'allow)
                 (acp-make-session-request-permission-response
                  :request-id .id
                  :option-id decision-option-id)
               (acp-make-session-request-permission-response
                :request-id .id
                :cancelled t))))
        (error
         (agent-pane--append-message
          'system
          (format "Failed to send permission response: %S" err))
         (agent-pane--ui-sync-transcript))))
     (t
      ;; Surface the request so it doesn't disappear silently.
      (agent-pane--append-message*
       :role 'acp
       :title (or .method "request")
       :text (agent-pane--pp-to-string request)
       :raw request)
      (agent-pane--ui-sync-transcript)))))
(defun agent-pane--make-session-new-request (&optional resume-session-id)
  "Build a `session/new' request.
When RESUME-SESSION-ID is non-nil, include it to request server-side resume."
  (let ((params `((cwd . ,(expand-file-name default-directory))
                  (mcpServers . []))))
    (when (and (stringp resume-session-id)
               (not (string-empty-p (string-trim resume-session-id))))
      (setq params (append params `((sessionId . ,(string-trim resume-session-id))))))
    `((:method . "session/new")
      (:params . ,params))))

(defun agent-pane--handshake (on-ready)
  "Ensure ACP session exists, then call ON-READY.
Non-fatal on error: writes an error into the current assistant message."
  (agent-pane--ensure-state)
  (when agent-pane-enable-acp-logging
    (setq acp-logging-enabled t))
  (cond
   ((map-elt agent-pane--state :session-id)
    (funcall on-ready))
   ((map-elt agent-pane--state :connecting)
    (let ((queued (map-elt agent-pane--state :queued-prompts)))
      (unless (memq on-ready queued)
        (map-put! agent-pane--state :queued-prompts
                  (append queued (list on-ready))))))
   (t
    (map-put! agent-pane--state :connecting t)
    (agent-pane--ensure-acp-client)
    (agent-pane--subscribe)
    (let ((client (map-elt agent-pane--state :client))
          (request-timeout agent-pane-acp-request-timeout-seconds))
      (cl-labels
          ((finalize ()
             (map-put! agent-pane--state :connecting nil)
             (agent-pane--rerender)
             (let ((queued (map-elt agent-pane--state :queued-prompts)))
               (map-put! agent-pane--state :queued-prompts nil)
               (dolist (cb (append queued (list on-ready)))
                 (ignore-errors (funcall cb))))))
        (condition-case err
            (agent-pane--acp-send-request
             :client client
             :buffer (current-buffer)
             :request (acp-make-initialize-request
                       :protocol-version 1
                       ;; We intentionally do not advertise fs capabilities until we
                       ;; actually implement `fs/read_text_file' and `fs/write_text_file'.
                       :read-text-file-capability nil
                       :write-text-file-capability nil)
             :timeout-seconds request-timeout
             :on-success
             (lambda (init-result)
               (map-put! agent-pane--state :init-result init-result)
               (agent-pane--rerender)
               (cl-labels
                   ((start-session-new (&optional resume-session-id)
                      (agent-pane--acp-send-request
                       :client client
                       :buffer (current-buffer)
                       :request (agent-pane--make-session-new-request resume-session-id)
                       :timeout-seconds request-timeout
                       :on-success
                       (lambda (resp)
                         (map-put! agent-pane--state :session-new-result resp)
                         (agent-pane--update-model-metadata resp)
                         (let ((session-id (map-elt resp 'sessionId)))
                           (map-put! agent-pane--state :session-id session-id)
                           (when session-id
                             (map-put! agent-pane--state :resume-session-id session-id))
                           (agent-pane--transcript-log-session-id)
                           (agent-pane--rerender)
                           (cl-labels
                               ((start-session-model ()
                                  (let* ((model-id0 (and (stringp agent-pane-session-model-id)
                                                         (string-trim agent-pane-session-model-id)))
                                         (model-id (and model-id0 (not (string-empty-p model-id0)) model-id0)))
                                    (if model-id
                                        (agent-pane--acp-send-request
                                         :client client
                                         :buffer (current-buffer)
                                         :request (acp-make-session-set-model-request
                                                   :session-id session-id
                                                   :model-id model-id)
                                         :timeout-seconds request-timeout
                                         :on-success (lambda (r)
                                                       (map-put! agent-pane--state :session-model-result r)
                                                       (agent-pane--update-model-metadata r)
                                                       (finalize))
                                         :on-failure (lambda (e &optional _raw)
                                                       (agent-pane--append-message
                                                        'system
                                                        (format "Warning: failed to set session model (%s): %s"
                                                                model-id
                                                                (or (map-elt e 'message) e)))
                                                       (finalize)))
                                      (finalize))))
                                (start-session-mode ()
                                  (let* ((mode-id0 (and (stringp agent-pane-session-mode-id)
                                                        (string-trim agent-pane-session-mode-id)))
                                         (mode-id (and mode-id0 (not (string-empty-p mode-id0)) mode-id0)))
                                    (if mode-id
                                        (agent-pane--acp-send-request
                                         :client client
                                         :buffer (current-buffer)
                                         :request (acp-make-session-set-mode-request
                                                   :session-id session-id
                                                   :mode-id mode-id)
                                         :timeout-seconds request-timeout
                                         :on-success (lambda (r)
                                                       (map-put! agent-pane--state :session-mode-result r)
                                                       (agent-pane--update-model-metadata r)
                                                       (start-session-model))
                                         :on-failure (lambda (e &optional _raw)
                                                       (agent-pane--append-message
                                                        'system
                                                        (format "Warning: failed to set session mode (%s): %s"
                                                                mode-id
                                                                (or (map-elt e 'message) e)))
                                                       (start-session-model)))
                                      (start-session-model)))))
                             (start-session-mode))))
                       :on-failure
                       (lambda (e &optional _raw)
                         (if resume-session-id
                             (progn
                               (agent-pane--append-message
                                'system
                                (format "Warning: failed to resume ACP session %s (%s); starting a new session instead"
                                        resume-session-id
                                        (or (map-elt e 'message) e)))
                               (agent-pane--ui-sync-transcript)
                               (start-session-new nil))
                           (map-put! agent-pane--state :connecting nil)
                           (agent-pane--fail-current-assistant
                            "session/new failed: %s"
                            (or (map-elt e 'message) e))))))
                    (start-auth-or-session ()
                      (let* ((caps (and (listp init-result)
                                        (map-elt init-result 'agentCapabilities)))
                             (supports-load-session (and (listp caps)
                                                         (map-elt caps 'loadSession)))
                             (resume-id0 (map-elt agent-pane--state :resume-session-id))
                             (resume-id1 (and (stringp resume-id0)
                                              (string-trim resume-id0)))
                             (resume-id (and resume-id1
                                             (not (string-empty-p resume-id1))
                                             resume-id1))
                             (resume-target (and supports-load-session resume-id resume-id)))
                        (when (and resume-id (not supports-load-session))
                          (agent-pane--append-message
                           'system
                           "Note: ACP server does not advertise `loadSession'; starting a new session")
                          (agent-pane--ui-sync-transcript))
                        (if (and agent-pane-auth-method-id
                                 (stringp agent-pane-auth-method-id)
                                 (not (string-empty-p agent-pane-auth-method-id)))
                            (agent-pane--acp-send-request
                             :client client
                             :buffer (current-buffer)
                             :request (acp-make-authenticate-request
                                       :method-id agent-pane-auth-method-id)
                             :timeout-seconds request-timeout
                             :on-success
                             (lambda (auth-result)
                               (map-put! agent-pane--state :auth-result auth-result)
                               (agent-pane--rerender)
                               (start-session-new resume-target))
                             :on-failure
                             (lambda (e &optional _raw)
                               (map-put! agent-pane--state :connecting nil)
                               (agent-pane--fail-current-assistant
                                "%s"
                                (agent-pane--auth-failure-help e))))
                          (start-session-new resume-target)))))
                 (start-auth-or-session)))
             :on-failure
             (lambda (e &optional _raw)
               (map-put! agent-pane--state :connecting nil)
               (agent-pane--fail-current-assistant
                "initialize failed: %s"
                (or (map-elt e 'message) e))))
          (error
           (map-put! agent-pane--state :connecting nil)
           (agent-pane--fail-current-assistant "initialize error: %S" err))))))))
(defun agent-pane--set-session-model (model-id &optional on-success on-failure)
  "Send `session/set_model' for MODEL-ID on the active ACP session.
ON-SUCCESS is called with response payload.
ON-FAILURE is called with error payload and optional raw response."
  (let ((client (map-elt agent-pane--state :client))
        (session-id (map-elt agent-pane--state :session-id))
        (model-id* (and (stringp model-id) (string-trim model-id))))
    (unless (and client session-id)
      (error "No active ACP client/session"))
    (unless (and model-id* (not (string-empty-p model-id*)))
      (error "MODEL-ID must be a non-empty string"))
    (agent-pane--acp-send-request
     :client client
     :buffer (current-buffer)
     :request (acp-make-session-set-model-request
               :session-id session-id
               :model-id model-id*)
     :on-success (lambda (resp)
                   (map-put! agent-pane--state :session-model-result resp)
                   (agent-pane--update-model-metadata resp)
                   (when (functionp on-success)
                     (funcall on-success resp)))
     :on-failure (lambda (err &optional raw)
                   (when (functionp on-failure)
                     (funcall on-failure err raw))))))

(defun agent-pane--begin-turn ()
  "Reset per-turn streaming state.
This must only be called when starting a new prompt, not while a previous turn
is still streaming."
  (map-put! agent-pane--state :streaming-assistant-index nil)
  (map-put! agent-pane--state :streaming-thought-index nil)
  (map-put! agent-pane--state :last-session-update-kind nil))
(defun agent-pane--enqueue-prompt (text &optional message-idx)
  "Add TEXT to the prompt queue.
When MESSAGE-IDX is numeric, it is aligned with this queued prompt and will be
used to clear queued UI state once the prompt starts."
  (agent-pane--ensure-state)
  (map-put! agent-pane--state :prompt-queue
            (append (map-elt agent-pane--state :prompt-queue)
                    (list text)))
  (map-put! agent-pane--state :prompt-queue-message-indices
            (append (map-elt agent-pane--state :prompt-queue-message-indices)
                    (list (and (numberp message-idx) message-idx)))))
(defun agent-pane--pump-prompt-queue ()
  "If possible, send the next queued prompt to ACP."
  (agent-pane--ensure-state)
  (when (and agent-pane-enable-acp (not noninteractive))
    (let* ((queue (map-elt agent-pane--state :prompt-queue))
           (queue-msg-idxs (map-elt agent-pane--state :prompt-queue-message-indices))
           (in-progress (map-elt agent-pane--state :in-progress))
           (busy (or (map-elt agent-pane--state :prompt-in-flight)
                     (memq in-progress '(streaming cancelling)))))
      (cond
       ((or busy (null queue))
        nil)
       ((map-elt agent-pane--state :connecting)
        (agent-pane--handshake #'agent-pane--pump-prompt-queue))
       ((not (map-elt agent-pane--state :session-id))
        (agent-pane--handshake #'agent-pane--pump-prompt-queue))
       (t
        (let ((prompt (car queue))
              (prompt-msg-idx (car queue-msg-idxs)))
          (map-put! agent-pane--state :prompt-queue (cdr queue))
          (map-put! agent-pane--state :prompt-queue-message-indices (cdr queue-msg-idxs))
          (when (numberp prompt-msg-idx)
            (agent-pane--set-message-queued prompt-msg-idx nil)
            (agent-pane--ui-rerender-message prompt-msg-idx))
          (agent-pane--begin-turn)
          (map-put! agent-pane--state :prompt-in-flight t)
          (map-put! agent-pane--state :in-progress 'waiting)
          (agent-pane--rerender)
          (agent-pane--send-prompt prompt)))))))
(defun agent-pane--send-prompt (text)
  "Send TEXT to the active ACP session."
  (let ((client (map-elt agent-pane--state :client))
        (session-id (map-elt agent-pane--state :session-id)))
    (unless (and client session-id)
      (error "Missing ACP client/session"))
    (condition-case err
        (agent-pane--acp-send-request
         :client client
         :buffer (current-buffer)
         :request (acp-make-session-prompt-request
                   :session-id session-id
                   :prompt (list `((type . "text")
                                   (text . ,(substring-no-properties text)))))
         :on-success (lambda (resp)
                       (map-put! agent-pane--state :last-prompt-result resp)
                       (if (map-elt resp 'stopReason)
                           (progn
                             (map-put! agent-pane--state :prompt-in-flight nil)
                             (map-put! agent-pane--state :in-progress nil)
                             (let ((idx (map-elt agent-pane--state :streaming-assistant-index)))
                               (cond
                                ((numberp idx)
                                 (when (string-empty-p
                                        (or (map-elt (nth idx (map-elt agent-pane--state :messages)) :text) ""))
                                   (agent-pane--set-message-text idx "[no output]")
                                   (agent-pane--ui-set-message-text idx "[no output]")))
                                (t
                                 (agent-pane--append-message 'assistant "[no output]")))
                               (map-put! agent-pane--state :streaming-assistant-index nil)
                               (map-put! agent-pane--state :streaming-thought-index nil)
                               (agent-pane--rerender)
                               (agent-pane--pump-prompt-queue)))
                         (map-put! agent-pane--state :prompt-in-flight nil)
                         (when (eq (map-elt agent-pane--state :in-progress) 'waiting)
                           (map-put! agent-pane--state :in-progress 'streaming))
                         (agent-pane--rerender)))
         :on-failure (lambda (e &optional _raw)
                       (map-put! agent-pane--state :prompt-in-flight nil)
                       (map-put! agent-pane--state :in-progress nil)
                       (agent-pane--fail-current-assistant
                        "prompt failed: %s"
                        (or (map-elt e 'message) e))
                       (agent-pane--pump-prompt-queue)))
      (error
       (map-put! agent-pane--state :prompt-in-flight nil)
       (map-put! agent-pane--state :in-progress nil)
       (agent-pane--fail-current-assistant "prompt error: %S" err)
       (agent-pane--pump-prompt-queue)))))
(provide 'agent-pane-acp)
;;; agent-pane-acp.el ends here
