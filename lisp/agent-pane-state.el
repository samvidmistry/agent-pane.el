;;; agent-pane-state.el --- State and message model for agent-pane -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;;; Commentary:
;;
;; This module defines the buffer-local state object (`agent-pane--state`) and
;; provides small helpers to mutate the message model.
;;
;; Design notes:
;; - State is an alist keyed by keywords, accessed via `map.el`.
;; - Messages are small alists with keys `:role`, `:title`, `:text`, `:raw`.
;; - Rendering happens elsewhere; this module should not mutate buffers.
;;
;;; Code:
(require 'agent-pane-config)
(require 'cl-lib)
(require 'map)
(require 'subr-x)
(defvar-local agent-pane--state nil
  "Buffer-local state alist for `agent-pane-mode'.")

(defun agent-pane--conversation-init-from-messages (messages)
  "Return plist with conversation tree fields initialized from MESSAGES."
  (let ((nodes (make-hash-table :test 'eql))
        (next-id 1)
        (head 0))
    (puthash 0 (list :id 0 :parent nil :message nil :children nil) nodes)
    (dolist (msg messages)
      (let* ((id next-id)
             (parent-node (gethash head nodes)))
        (setq next-id (1+ next-id))
        (puthash id (list :id id :parent head :message msg :children nil) nodes)
        (puthash head
                 (plist-put parent-node :children
                            (append (plist-get parent-node :children) (list id)))
                 nodes)
        (setq head id)))
    (list :nodes nodes :head head :next-id next-id)))

(cl-defun agent-pane--make-state (&key messages)
  "Create an agent-pane state alist.
MESSAGES is a list of message alists."
  (let* ((msgs (or messages nil))
         (conv (agent-pane--conversation-init-from-messages msgs)))
    (list (cons :messages msgs)
          ;; Conversation tree (for rewind/branching).
          (cons :conversation-nodes (plist-get conv :nodes))
          (cons :conversation-head (plist-get conv :head))
          (cons :conversation-next-id (plist-get conv :next-id))
          (cons :client nil)
          (cons :session-id nil)
          (cons :connecting nil)
          (cons :subscribed nil)
          (cons :in-progress nil)
          ;; Pending user prompts that should be sent to the ACP session in order.
          (cons :prompt-queue nil)
          ;; Message indexes aligned with `:prompt-queue' entries.
          ;; Entries are either nil (not a queued-visible message) or a numeric
          ;; user message index that should be unmarked from queued once sent.
          (cons :prompt-queue-message-indices nil)
          (cons :prompt-in-flight nil)
          ;; Human-friendly session title (usually derived from the first user turn).
          (cons :session-title nil)
          (cons :transcript-title-logged nil)
          ;; Streaming indices (message list indexes).
          (cons :streaming-assistant-index nil)
          (cons :streaming-thought-index nil)
          (cons :last-session-update-kind nil)
          (cons :last-notification-session-id nil)
          ;; Handshake info (ACP response `result` payloads).
          (cons :init-result nil)
          (cons :auth-result nil)
          (cons :session-new-result nil)
          (cons :session-mode-result nil)
          (cons :session-model-result nil)
          (cons :session-config-options nil)
          (cons :available-model-ids nil)
          (cons :current-model-id nil)
          (cons :last-prompt-result nil)
          ;; Transcript.
          (cons :transcript-file nil)
          (cons :transcript-session-id-logged nil)
          ;; Number of messages already seen by the user in this buffer.
          (cons :session-seen-message-count (length msgs))
          ;; Resume support.
          (cons :resume-session-id nil)
          ;; Tool calls and permissions.
          (cons :tool-calls (make-hash-table :test 'equal))
          (cons :tool-call-msg-index (make-hash-table :test 'equal))
          (cons :permission-msg-index (make-hash-table :test 'equal))
          ;; Session-scoped permission choices (tool kind/title -> option id).
          (cons :permission-session-rules (make-hash-table :test 'equal))
          ;; Misc server state.
          (cons :available-commands nil)
          ;; Internal: prompts queued while connecting.
          (cons :queued-prompts nil)
          ;; Input UX state.
          (cons :input-history nil)
          (cons :input-history-index nil)
          (cons :input-history-draft nil))))
(defun agent-pane--conversation-node (node-id)
  "Return conversation node for NODE-ID, or nil."
  (let ((nodes (map-elt agent-pane--state :conversation-nodes)))
    (and (hash-table-p nodes)
         (gethash node-id nodes))))

(defun agent-pane--conversation-node-depth (node-id)
  "Return depth (message count from root) for NODE-ID."
  (let ((depth 0)
        (cur node-id))
    (while (and (numberp cur) (> cur 0))
      (setq depth (1+ depth))
      (setq cur (plist-get (agent-pane--conversation-node cur) :parent)))
    depth))

(defun agent-pane--conversation-current-path-node-ids ()
  "Return node ids from root to conversation head."
  (agent-pane--ensure-state)
  (let ((ids nil)
        (cur (map-elt agent-pane--state :conversation-head)))
    (while (numberp cur)
      (push cur ids)
      (setq cur (plist-get (agent-pane--conversation-node cur) :parent)))
    (nreverse ids)))

(defun agent-pane--conversation-current-path-messages ()
  "Return messages on current path from root to head."
  (delq nil
        (mapcar (lambda (id)
                  (plist-get (agent-pane--conversation-node id) :message))
                (agent-pane--conversation-current-path-node-ids))))

(defun agent-pane--conversation-rebuild-messages ()
  "Rebuild linear `:messages' from the current conversation path."
  (map-put! agent-pane--state :messages (agent-pane--conversation-current-path-messages)))

(defun agent-pane--conversation-append-message (msg)
  "Append MSG to conversation tree and current linear path.
Return message index in linear `:messages'."
  (agent-pane--ensure-state)
  (let* ((head (or (map-elt agent-pane--state :conversation-head) 0))
         (nodes (map-elt agent-pane--state :conversation-nodes))
         (next-id (or (map-elt agent-pane--state :conversation-next-id) 1))
         (msgs (map-elt agent-pane--state :messages))
         (idx (length msgs))
         (parent-node (or (gethash head nodes)
                          (list :id head :parent nil :message nil :children nil))))
    (puthash next-id (list :id next-id :parent head :message msg :children nil) nodes)
    (puthash head
             (plist-put parent-node :children
                        (append (plist-get parent-node :children) (list next-id)))
             nodes)
    (map-put! agent-pane--state :conversation-head next-id)
    (map-put! agent-pane--state :conversation-next-id (1+ next-id))
    (map-put! agent-pane--state :messages (append msgs (list msg)))
    idx))

(defun agent-pane--conversation-rewind-to-node (node-id)
  "Set conversation head to NODE-ID and rebuild linear messages.
Return new message count."
  (agent-pane--ensure-state)
  (unless (agent-pane--conversation-node node-id)
    (error "Unknown conversation node id: %S" node-id))
  (map-put! agent-pane--state :conversation-head node-id)
  (agent-pane--conversation-rebuild-messages)
  (length (map-elt agent-pane--state :messages)))

(defun agent-pane--ensure-state ()
  "Ensure `agent-pane--state' is initialized.
Note: `:messages' is a valid key whose value is often nil (empty transcript),
so we must check key presence, not value truthiness."
  (unless (and (listp agent-pane--state)
               (map-contains-key agent-pane--state :messages)
               (map-contains-key agent-pane--state :client)
               (hash-table-p (map-elt agent-pane--state :tool-calls)))
    (setq-local agent-pane--state (agent-pane--make-state)))
  (unless (map-contains-key agent-pane--state :session-seen-message-count)
    (map-put! agent-pane--state :session-seen-message-count
              (length (or (map-elt agent-pane--state :messages) nil))))
  (unless (and (hash-table-p (map-elt agent-pane--state :conversation-nodes))
               (integerp (map-elt agent-pane--state :conversation-head))
               (integerp (map-elt agent-pane--state :conversation-next-id)))
    (let* ((conv (agent-pane--conversation-init-from-messages
                  (map-elt agent-pane--state :messages))))
      (map-put! agent-pane--state :conversation-nodes (plist-get conv :nodes))
      (map-put! agent-pane--state :conversation-head (plist-get conv :head))
      (map-put! agent-pane--state :conversation-next-id (plist-get conv :next-id)))))

(cl-defun agent-pane--append-message* (&key role text title raw)
  "Append a message to state and return its index.
ROLE is a symbol like `user', `assistant', `system'.
TEXT is the message body.
TITLE is an optional short label shown next to the role.
RAW is an optional elisp object attached for inspection."
  (agent-pane--ensure-state)
  (let ((msg (list (cons :role role)
                   (cons :text (or text ""))
                   (cons :title title)
                   (cons :raw raw)
                   (cons :queued nil))))
    (agent-pane--conversation-append-message msg)))
(defun agent-pane--append-message (role text)
  "Append a message with ROLE and TEXT to state and return its index."
  (agent-pane--append-message* :role role :text text))
(defun agent-pane--append-to-message-text (idx chunk)
  "Append CHUNK to message at IDX."
  (agent-pane--ensure-state)
  (let* ((msgs (map-elt agent-pane--state :messages))
         (msg (nth idx msgs)))
    (when msg
      (map-put! msg :text (concat (or (map-elt msg :text) "") (or chunk ""))))))
(defun agent-pane--set-message-text (idx text)
  "Set message at IDX to TEXT."
  (agent-pane--ensure-state)
  (let* ((msgs (map-elt agent-pane--state :messages))
         (msg (nth idx msgs)))
    (when msg
      (map-put! msg :text (or text "")))))
(defun agent-pane--set-message-title (idx title)
  "Set message at IDX to TITLE."
  (agent-pane--ensure-state)
  (let* ((msgs (map-elt agent-pane--state :messages))
         (msg (nth idx msgs)))
    (when msg
      ;; Keep the key even if TITLE is nil; it simplifies in-place updates.
      (map-put! msg :title title))))
(defun agent-pane--set-message-raw (idx raw)
  "Set message at IDX to RAW."
  (agent-pane--ensure-state)
  (let* ((msgs (map-elt agent-pane--state :messages))
         (msg (nth idx msgs)))
    (when msg
      ;; Keep the key even if RAW is nil.
      (map-put! msg :raw raw))))

(defun agent-pane--set-message-queued (idx queuedp)
  "Set queued flag for message at IDX to QUEUEDP."
  (agent-pane--ensure-state)
  (let* ((msgs (map-elt agent-pane--state :messages))
         (msg (nth idx msgs)))
    (when msg
      (map-put! msg :queued (and queuedp t)))))

(defun agent-pane--mark-thread-seen ()
  "Mark all current messages in this chat buffer as seen by the user."
  (agent-pane--ensure-state)
  (let ((seen (or (map-elt agent-pane--state :session-seen-message-count) 0))
        (count (length (or (map-elt agent-pane--state :messages) nil))))
    (when (/= seen count)
      (map-put! agent-pane--state :session-seen-message-count count)
      t)))

(provide 'agent-pane-state)
;;; agent-pane-state.el ends here
