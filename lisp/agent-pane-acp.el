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

(require 'acp)

(require 'cl-lib)

(require 'map)

(require 'subr-x)

(defun agent-pane--append-acp-event (text &optional raw)

  "Append a low-level ACP event if configured.

TEXT is the human-readable summary.  RAW is the structured payload."

  (when agent-pane-show-raw-events

    (agent-pane--append-message* :role 'acp :text text :raw raw)

    (agent-pane--rerender)))

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

                (let* ((cmd+params (agent-pane--codex-command-and-params))

                       (cmd (car cmd+params))

                       (params (cdr cmd+params)))

                  (acp-make-client

                   :context-buffer (current-buffer)

                   :command cmd

                   :command-params params

                   :environment-variables agent-pane-codex-environment))))))

(cl-defun agent-pane--acp-send-request (&key client request buffer on-success on-failure)

  "Send an ACP REQUEST using CLIENT.

If CLIENT is a fake (marked with `:agent-pane-fake'), avoid starting external

processes and call the client request sender directly."

  (unless client

    (error "Missing required argument: :client"))

  (unless request

    (error "Missing required argument: :request"))

  (agent-pane--append-acp-event

   (format "→ %s" (map-elt request :method))

   request)

  (let ((wrapped-success

         (lambda (resp)

           (agent-pane--append-acp-event

            (format "← %s (ok)" (map-elt request :method))

            resp)

           (when (functionp on-success)

             (funcall on-success resp))))

        (wrapped-failure

         (lambda (err &optional raw)

           (agent-pane--append-acp-event

            (format "← %s (error): %s" (map-elt request :method) (or (map-elt err 'message) err))

            (or raw err))

           (when (functionp on-failure)

             (let ((arity (cdr (func-arity on-failure))))

               (if (>= arity 2)

                   (funcall on-failure err raw)

                 (funcall on-failure err)))))))

    (if (map-elt client :agent-pane-fake)

        ;; Fake sender does not accept :buffer or :sync.

        (funcall (map-elt client :request-sender)

                 :client client

                 :request request

                 :on-success wrapped-success

                 :on-failure wrapped-failure)

      (acp-send-request

       :client client

       :request request

       :buffer buffer

       :on-success wrapped-success

       :on-failure wrapped-failure))))

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

(defun agent-pane--tool-call-content->string (content)

  "Format tool call CONTENT into a plain string."

  (let ((items (agent-pane--listify content)))

    (string-join

     (delq nil

           (mapcar

            (lambda (item)

              (cond

               ((null item) nil)

               ((stringp item) item)

               ((stringp (map-nested-elt item '(content text)))

                (map-nested-elt item '(content text)))

               ((stringp (map-elt item 'text))

                (map-elt item 'text))

               ;; Some servers put the text under (content (text . "..."))

               ((let ((c (and (listp item) (map-elt item 'content))))

                  (cond

                   ((stringp c) c)

                   ((and (listp c) (stringp (map-elt c 'text)))

                    (map-elt c 'text))

                   ((null c) nil)

                   (t (agent-pane--pp-to-string c)))))

               (t (agent-pane--pp-to-string item))))

            items))

     "\n\n")))

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

(defun agent-pane--tool-call-display-title (entry)

  "Return a compact tool-call title for ENTRY.

This is what we show in the UI label after `Tool'.

Prefer showing the command (or `tool: command') over opaque ids like

`call_...'."

  (let* ((id (or (plist-get entry :toolCallId) ""))

         (title0 (string-trim (or (plist-get entry :title) "")))

         (title (if (or (string-empty-p title0)

                        (equal title0 id)

                        (string-match-p (rx string-start (or "call_" "call-")) title0))

                    ""

                  title0))

         (cmd (string-trim (or (plist-get entry :command) ""))))

    (cond

     ((and (not (string-empty-p title)) (not (string-empty-p cmd)))

      (if (string-match-p (regexp-quote cmd) title)

          title

        (format "%s: %s" title cmd)))

     ((not (string-empty-p cmd)) cmd)

     ((not (string-empty-p title)) title)

     (t id))))

(defun agent-pane--tool-call-entry-to-text (entry)

  "Render a tool call ENTRY plist as a message text."

  (let* ((id (plist-get entry :toolCallId))

         (title (string-trim (or (plist-get entry :title) "")))

         (status (or (plist-get entry :status) ""))

         (kind (or (plist-get entry :kind) ""))

         (command (or (plist-get entry :command) ""))

         (desc (or (plist-get entry :description) ""))

         (locations (plist-get entry :locations))

         (output (plist-get entry :output)))

    (string-trim-right

     (string-join

      (delq nil

            (list (format "id: %s" id)

                  (when (not (string-empty-p title)) (format "title: %s" title))

                  (when (not (string-empty-p kind)) (format "kind: %s" kind))

                  (when (not (string-empty-p status)) (format "status: %s" status))

                  (when (not (string-empty-p command)) (format "command: %s" command))

                  (when (not (string-empty-p desc)) (format "description: %s" desc))

                  (when-let ((locs (and locations (agent-pane--format-locations locations))))

                    (unless (string-empty-p locs)

                      (format "locations: %s" locs)))

                  (when (and output (not (string-empty-p output)))

                    (concat "\noutput:\n" output))))

      "\n"))))

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

        ;; If we were waiting for the agent, flip to streaming on first update.

        (when (eq (map-elt agent-pane--state :in-progress) 'waiting)

          (map-put! agent-pane--state :in-progress 'streaming)

          (agent-pane--ui-update-status-line))

        (map-put! agent-pane--state :last-session-update-kind kind)

        (cond

         ;; Agent message chunks (main visible output).

         ((equal kind "agent_message_chunk")

          (let* ((text (map-nested-elt update '(content text)))

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

            (let* ((text (map-nested-elt update '(content text)))

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

          (let ((text (map-nested-elt update '(content text))))

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

                   (command (map-nested-elt update '(rawInput command)))

                   (desc (map-nested-elt update '(rawInput description)))

                   (locations (map-elt update 'locations)))

              (setq entry (plist-put entry :toolCallId id))

              (setq entry (plist-put entry :title title))

              (setq entry (plist-put entry :status status))

              (setq entry (plist-put entry :kind tool-kind))

              (setq entry (plist-put entry :command command))

              (setq entry (plist-put entry :description desc))

              (setq entry (plist-put entry :locations locations))

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

                   (command (map-nested-elt update '(rawInput command))))

              (setq entry (plist-put entry :toolCallId id))

              (setq entry (plist-put entry :status status))

              (when command

                (setq entry (plist-put entry :command command)))

              (setq entry (plist-put entry :output (string-trim output)))

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

                 (concat (agent-pane--tool-call-entry-to-text entry) "\n\n")))

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

                 (when chosen (format "chosen: %s" chosen))

                 "\noptions:"

                 opts-text))

     "\n")))

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

                 (idx-table (map-elt agent-pane--state :permission-msg-index))

                 (msg-idx (and tool-call-id (gethash tool-call-id idx-table)))

                 (option-id

                  (when (and (eq agent-pane-permission-policy 'auto-allow)

                             (listp opts))

                    (or (cl-loop for kind in '("allow_always" "allow_once")

                                 thereis (cl-loop for opt in opts

                                                  when (equal (map-elt opt 'kind) kind)

                                                  return (map-elt opt 'optionId)))

                        (map-elt (car-safe opts) 'optionId)))))

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

            (let ((body (agent-pane--permission-request->text

                         request

                         (cond

                          ((and (eq agent-pane-permission-policy 'auto-allow) option-id) option-id)

                          (t 'cancel)))))

              (agent-pane--set-message-raw msg-idx request)

              (agent-pane--set-message-text msg-idx body)

              (agent-pane--ui-set-message-text msg-idx body)

              (agent-pane--transcript-role-heading 'system "permission")

              (agent-pane--transcript-log-session-id)

              (agent-pane--transcript-append (concat body "\n\n")))

            (agent-pane--acp-send-response

             :client client

             :response

             (if (and (eq agent-pane-permission-policy 'auto-allow) option-id)

                 (acp-make-session-request-permission-response

                  :request-id .id

                  :option-id option-id)

               (acp-make-session-request-permission-response

                :request-id .id

                :cancelled t))))

        (error

         (agent-pane--append-message 'system (format "Failed to send permission response: %S" err))

         (agent-pane--ui-sync-transcript))))

     (t

      ;; Surface the request so it doesn't disappear silently.

      (agent-pane--append-message*

       :role 'acp

       :title (or .method "request")

       :text (agent-pane--pp-to-string request)

       :raw request)

      (agent-pane--ui-sync-transcript)))))

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

    (map-put! agent-pane--state :queued-prompts

              (append (map-elt agent-pane--state :queued-prompts)

                      (list on-ready))))

   (t

    (map-put! agent-pane--state :connecting t)

    (agent-pane--ensure-acp-client)

    (agent-pane--subscribe)

    (let ((client (map-elt agent-pane--state :client)))

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

             :on-success

             (lambda (init-result)

               (map-put! agent-pane--state :init-result init-result)

               (agent-pane--rerender)

               (agent-pane--acp-send-request

                :client client

                :buffer (current-buffer)

                :request (acp-make-authenticate-request

                          :method-id agent-pane-auth-method-id)

                :on-success

                (lambda (auth-result)

                  (map-put! agent-pane--state :auth-result auth-result)

                  (agent-pane--rerender)

                  (agent-pane--acp-send-request

                   :client client

                   :buffer (current-buffer)

                   :request (acp-make-session-new-request

                             :cwd default-directory

                             :mcp-servers nil)

                   :on-success

                   (lambda (resp)

                     (map-put! agent-pane--state :session-new-result resp)

                     (let ((session-id (map-elt resp 'sessionId)))

                       (map-put! agent-pane--state :session-id session-id)

                       (agent-pane--transcript-log-session-id)

                       (agent-pane--rerender)

                       (if (and agent-pane-session-mode-id (stringp agent-pane-session-mode-id))

                           (agent-pane--acp-send-request

                            :client client

                            :buffer (current-buffer)

                            :request (acp-make-session-set-mode-request

                                      :session-id session-id

                                      :mode-id agent-pane-session-mode-id)

                            :on-success (lambda (r)

                                          (map-put! agent-pane--state :session-mode-result r)

                                          (finalize))

                            :on-failure (lambda (e &optional _raw)

                                          (agent-pane--append-message

                                           'system

                                           (format "Warning: failed to set session mode (%s): %s"

                                                   agent-pane-session-mode-id

                                                   (or (map-elt e 'message) e)))

                                          (finalize)))

                         (finalize))))

                   :on-failure

                   (lambda (e &optional _raw)

                     (map-put! agent-pane--state :connecting nil)

                     (agent-pane--fail-current-assistant

                      "session/new failed: %s"

                      (or (map-elt e 'message) e)))))

                :on-failure

                (lambda (e &optional _raw)

                  (map-put! agent-pane--state :connecting nil)

                  (agent-pane--fail-current-assistant

                   "authenticate failed: %s"

                   (or (map-elt e 'message) e)))))

             :on-failure

             (lambda (e &optional _raw)

               (map-put! agent-pane--state :connecting nil)

               (agent-pane--fail-current-assistant

                "initialize failed: %s"

                (or (map-elt e 'message) e))))

          (error

           (map-put! agent-pane--state :connecting nil)

           (agent-pane--fail-current-assistant "initialize error: %S" err))))))))

(defun agent-pane--begin-turn ()

  "Reset per-turn streaming state.

This must only be called when starting a new prompt, not while a previous turn

is still streaming."

  (map-put! agent-pane--state :streaming-assistant-index nil)

  (map-put! agent-pane--state :streaming-thought-index nil)

  (map-put! agent-pane--state :last-session-update-kind nil))

(defun agent-pane--enqueue-prompt (text)

  "Add TEXT to the prompt queue."

  (agent-pane--ensure-state)

  (map-put! agent-pane--state :prompt-queue

            (append (map-elt agent-pane--state :prompt-queue)

                    (list text))))

(defun agent-pane--pump-prompt-queue ()

  "If possible, send the next queued prompt to ACP."

  (agent-pane--ensure-state)

  (when (and agent-pane-enable-acp (not noninteractive))

    (let* ((queue (map-elt agent-pane--state :prompt-queue))

           (in-progress (map-elt agent-pane--state :in-progress))

           (busy (or (map-elt agent-pane--state :prompt-in-flight)

                     (memq in-progress '(streaming cancelling)))))

      (cond

       ((or busy (null queue))

        nil)

       ((map-elt agent-pane--state :connecting)

        nil)

       ((not (map-elt agent-pane--state :session-id))

        (agent-pane--handshake #'agent-pane--pump-prompt-queue))

       (t

        (let ((prompt (car queue)))

          (map-put! agent-pane--state :prompt-queue (cdr queue))

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

