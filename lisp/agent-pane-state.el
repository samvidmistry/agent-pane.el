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

(cl-defun agent-pane--make-state (&key messages)

  "Create an agent-pane state alist.

MESSAGES is a list of message alists."

  (list (cons :messages (or messages nil))

        (cons :client nil)

        (cons :session-id nil)

        (cons :connecting nil)

        (cons :subscribed nil)

        (cons :in-progress nil)

        ;; Pending user prompts that should be sent to the ACP session in order.

        (cons :prompt-queue nil)

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

        (cons :last-prompt-result nil)

        ;; Transcript.

        (cons :transcript-file nil)

        (cons :transcript-session-id-logged nil)

        ;; Tool calls and permissions.

        (cons :tool-calls (make-hash-table :test 'equal))

        (cons :tool-call-msg-index (make-hash-table :test 'equal))

        (cons :permission-msg-index (make-hash-table :test 'equal))

        ;; Misc server state.

        (cons :available-commands nil)

        ;; Internal: prompts queued while connecting.

        (cons :queued-prompts nil)))

(defun agent-pane--ensure-state ()

  "Ensure `agent-pane--state' is initialized.

Note: `:messages' is a valid key whose value is often nil (empty transcript),

so we must check key presence, not value truthiness."

  (unless (and (listp agent-pane--state)

               (map-contains-key agent-pane--state :messages)

               (map-contains-key agent-pane--state :client)

               (hash-table-p (map-elt agent-pane--state :tool-calls)))

    (setq-local agent-pane--state (agent-pane--make-state))))

(cl-defun agent-pane--append-message* (&key role text title raw)

  "Append a message to state and return its index.

ROLE is a symbol like `user', `assistant', `system'.

TEXT is the message body.

TITLE is an optional short label shown next to the role.

RAW is an optional elisp object attached for inspection."

  (agent-pane--ensure-state)

  (let* ((msgs (map-elt agent-pane--state :messages))

         (idx (length msgs))

         ;; Keep :title and :raw keys present even when nil so later `map-put!'

         ;; updates can be done in-place.

         (msg (list (cons :role role)

                    (cons :text (or text ""))

                    (cons :title title)

                    (cons :raw raw))))

    (map-put! agent-pane--state :messages (append msgs (list msg)))

    idx))

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

(provide 'agent-pane-state)

;;; agent-pane-state.el ends here

