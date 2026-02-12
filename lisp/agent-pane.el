;;; agent-pane.el --- Codex-style UI for ACP-backed agents -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;;; Commentary:
;;
;; A Codex-app-like UI for interacting with agents via ACP.
;;
;; Implementation is split across multiple files for maintainability:
;;
;; - agent-pane-config.el     Customization and faces
;; - agent-pane-state.el      Buffer-local state + message model helpers
;; - agent-pane-ui.el         Incremental rendering (anti-flicker)
;; - agent-pane-markdown.el   Streaming-safe markdown-ish fontification
;; - agent-pane-transcript.el Transcript persistence
;; - agent-pane-diff.el       Diff extraction + diff/ediff viewers
;; - agent-pane-acp.el        ACP wiring + streaming/tool/permission handling
;; - agent-pane-sessions.el   Sessions sidebar
;;
;;; Code:
(require 'agent-pane-config)
(require 'agent-pane-state)
(require 'agent-pane-ui)
(require 'agent-pane-transcript)
(require 'agent-pane-acp)
(require 'agent-pane-sessions)
(require 'agent-pane-util)
(require 'agent-pane-diff)
(require 'acp)
(require 'map)
(require 'subr-x)
(defvar agent-pane-acp-provider)
(defvar agent-pane-session-model-id)
(defvar agent-pane-header-details-collapsed)
(defvar agent-pane-tool-output-preview-lines)
(defvar agent-pane--header-details-collapsed)
(defvar-local agent-pane--tool-output-full-mode nil
  "When non-nil, tool output blocks render full output in this chat buffer.")
(declare-function agent-pane--set-session-model "agent-pane-acp")
(declare-function agent-pane--refresh-tool-call-messages "agent-pane-acp")
(declare-function agent-pane--conversation-current-path-node-ids "agent-pane-state")
(declare-function agent-pane--conversation-node "agent-pane-state")
(declare-function agent-pane--conversation-node-depth "agent-pane-state")
(declare-function agent-pane--conversation-rewind-to-node "agent-pane-state")
(declare-function agent-pane--ui-full-render "agent-pane-ui")
(defun agent-pane-open-acp-traffic ()
  "Open the ACP traffic buffer for the current agent-pane client."
  (interactive)
  (agent-pane--ensure-acp-client)
  (let ((client (map-elt agent-pane--state :client)))
    (unless client
      (user-error "No ACP client"))
    (pop-to-buffer (acp-traffic-buffer :client client))))
(defun agent-pane-open-acp-logs ()
  "Open the ACP logs buffer for the current agent-pane client."
  (interactive)
  (agent-pane--ensure-acp-client)
  (let ((client (map-elt agent-pane--state :client)))
    (unless client
      (user-error "No ACP client"))
    (pop-to-buffer (acp-logs-buffer :client client))))
(defun agent-pane-open-acp-stderr ()
  "Open the ACP stderr buffer (if any) for the current agent-pane client."
  (interactive)
  (agent-pane--ensure-acp-client)
  (let* ((client (map-elt agent-pane--state :client))
         (buf (agent-pane--acp-stderr-buffer client)))
    (unless buf
      (user-error "No ACP stderr buffer (client may not be started yet)"))
    (pop-to-buffer buf)))
(defun agent-pane-show-state ()
  "Show internal agent-pane and acp.el state for the current buffer."
  (interactive)
  (agent-pane--ensure-state)
  (let* ((client (map-elt agent-pane--state :client))
         (buf (get-buffer-create "*agent-pane state*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "agent-pane--state:\n\n" (agent-pane--pp-to-string agent-pane--state) "\n\n")
      (when client
        (insert "acp client:\n\n" (agent-pane--pp-to-string client) "\n"))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buf)))
(defun agent-pane-show-raw-at-point ()
  "Pretty print the raw object attached at point (if any)."
  (interactive)
  (let ((raw (get-text-property (point) 'agent-pane-raw)))
    (unless raw
      (user-error "No raw object at point"))
    (let ((buf (get-buffer-create "*agent-pane raw*")))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (agent-pane--pp-to-string raw) "\n")
        (goto-char (point-min))
        (special-mode))
      (pop-to-buffer buf))))
(defun agent-pane-cancel ()
  "Cancel the current ACP run for this session."
  (interactive)
  (agent-pane--ensure-state)
  (unless (and agent-pane-enable-acp (not noninteractive))
    (user-error "ACP is not enabled"))
  (let ((client (map-elt agent-pane--state :client))
        (session-id (map-elt agent-pane--state :session-id)))
    (unless (and client session-id)
      (user-error "No active ACP session"))
    (condition-case err
        (progn
          (map-put! agent-pane--state :in-progress 'cancelling)
          (agent-pane--ui-update-status-line)
          (agent-pane--acp-send-notification
           :client client
           :notification (acp-make-session-cancel-notification
                          :session-id session-id
                          :reason "user cancel"))
          ;; We don't get an explicit completion ack for cancel; treat this as a
          ;; local state transition so queued follow-ups can proceed.
          (map-put! agent-pane--state :prompt-in-flight nil)
          (map-put! agent-pane--state :in-progress nil)
          (agent-pane--rerender)
          (agent-pane--pump-prompt-queue))
      (error
       (map-put! agent-pane--state :prompt-in-flight nil)
       (map-put! agent-pane--state :in-progress nil)
       (agent-pane--append-message 'system (format "Cancel failed: %S" err))
       (agent-pane--rerender)))))
(defun agent-pane--input-history-push (text)
  "Push TEXT into input history, avoiding immediate duplicates."
  (let ((s (string-trim (or text ""))))
    (unless (string-empty-p s)
      (let ((history (map-elt agent-pane--state :input-history)))
        (unless (equal s (car history))
          (map-put! agent-pane--state :input-history (cons s history))))))
  (map-put! agent-pane--state :input-history-index nil)
  (map-put! agent-pane--state :input-history-draft nil))
(defun agent-pane-input-history-prev ()
  "Replace input with the previous item from input history."
  (interactive)
  (agent-pane--ensure-state)
  (let ((history (map-elt agent-pane--state :input-history)))
    (unless history
      (user-error "No input history"))
    (let* ((idx0 (map-elt agent-pane--state :input-history-index))
           (idx (if (numberp idx0)
                    (min (1+ idx0) (1- (length history)))
                  0)))
      (unless (numberp idx0)
        (map-put! agent-pane--state :input-history-draft (agent-pane--input-text)))
      (map-put! agent-pane--state :input-history-index idx)
      (agent-pane--set-input-text (nth idx history))
      (agent-pane--goto-input)
      (goto-char (point-max)))))
(defun agent-pane-input-history-next ()
  "Replace input with the next item from input history, or draft text."
  (interactive)
  (agent-pane--ensure-state)
  (let ((idx0 (map-elt agent-pane--state :input-history-index)))
    (unless (numberp idx0)
      (user-error "Not browsing input history"))
    (if (> idx0 0)
        (let* ((history (map-elt agent-pane--state :input-history))
               (idx (1- idx0)))
          (map-put! agent-pane--state :input-history-index idx)
          (agent-pane--set-input-text (nth idx history)))
      (map-put! agent-pane--state :input-history-index nil)
      (agent-pane--set-input-text (or (map-elt agent-pane--state :input-history-draft) ""))
      (map-put! agent-pane--state :input-history-draft nil))
    (agent-pane--goto-input)
    (goto-char (point-max))))
(defun agent-pane--last-user-message-text ()
  "Return the last user message text from state, or nil."
  (cl-loop for m in (reverse (map-elt agent-pane--state :messages))
           when (eq (map-elt m :role) 'user)
           return (map-elt m :text)))
(defun agent-pane-copy-last-user-to-input ()
  "Copy the last user message into the input box for edit-and-resend."
  (interactive)
  (agent-pane--ensure-state)
  (let ((text (agent-pane--last-user-message-text)))
    (unless (and text (not (string-empty-p (string-trim text))))
      (user-error "No previous user message"))
    (agent-pane--set-input-text text)
    (agent-pane--goto-input)
    (goto-char (point-max))))
(defun agent-pane--last-tool-message-text ()
  "Return the last tool message text from state, or nil."
  (cl-loop for m in (reverse (map-elt agent-pane--state :messages))
           when (eq (map-elt m :role) 'tool)
           return (map-elt m :text)))
(defun agent-pane-copy-last-tool-output ()
  "Copy the last tool output to the kill ring.
If a tool message contains an `output:' section, copy only that section.
Otherwise copy the full tool message text."
  (interactive)
  (agent-pane--ensure-state)
  (let ((text (agent-pane--last-tool-message-text)))
    (unless (and text (not (string-empty-p (string-trim text))))
      (user-error "No previous tool output"))
    (let ((payload (cond
                    ((string-match (rx "\noutput:\n" (group (* anything))) text)
                     (match-string 1 text))
                    ((string-match (rx "\noutput (latest " (* nonl) "):\n" (group (* anything))) text)
                     (match-string 1 text))
                    (t text))))
      (kill-new (string-trim-right payload))
      (message "Copied last tool output"))))
(defun agent-pane-copy-code-block-at-point ()
  "Copy the fenced code block body at point to the kill ring."
  (interactive)
  (let* ((p0 (point))
         (p (cond
             ((get-text-property p0 'agent-pane-md-code-block) p0)
             ((and (> p0 (point-min))
                   (get-text-property (1- p0) 'agent-pane-md-code-block))
              (1- p0))
             (t nil))))
    (unless p
      (user-error "Point is not inside a rendered code block"))
    (let* ((beg (or (previous-single-property-change p 'agent-pane-md-code-block)
                    (point-min)))
           (end (or (next-single-property-change p 'agent-pane-md-code-block)
                    (point-max)))
           (text (buffer-substring-no-properties beg end)))
      (kill-new (string-trim-right text))
      (message "Copied code block"))))

(defun agent-pane--tool-call-id-by-message-index (idx)
  "Return tool-call id associated with message index IDX, or nil."
  (let ((table (map-elt agent-pane--state :tool-call-msg-index))
        found)
    (when (hash-table-p table)
      (maphash (lambda (tool-id msg-idx)
                 (when (and (numberp msg-idx)
                            (= msg-idx idx)
                            (null found))
                   (setq found tool-id)))
               table))
    found))

(defun agent-pane--diff-info-for-message (msg idx)
  "Return extracted diff info for transcript message MSG at index IDX."
  (let ((role (map-elt msg :role))
        (raw (map-elt msg :raw)))
    (pcase role
      ('tool
       (when-let* ((tool-id (agent-pane--tool-call-id-by-message-index idx))
                   (entry (gethash tool-id (map-elt agent-pane--state :tool-calls))))
         (plist-get entry :diff)))
      ('system
       (when (and (listp raw)
                  (equal (map-elt raw 'method) "session/request_permission"))
         (agent-pane--extract-diff-info (map-nested-elt raw '(params toolCall)))))
      (_
       (when (listp raw)
         (agent-pane--extract-diff-info raw))))))

(defun agent-pane-view-diff-at-point ()
  "Open a diff view for the change associated with the message at point."
  (interactive)
  (agent-pane--ensure-state)
  (let* ((msg (get-text-property (point) 'agent-pane-message))
         (idx (and msg (cl-position msg (map-elt agent-pane--state :messages) :test #'eq))))
    (unless (and msg (numberp idx))
      (user-error "Point is not on a message block"))
    (let ((diff (agent-pane--diff-info-for-message msg idx)))
      (unless diff
        (user-error "No structured file diff at point"))
      (agent-pane--show-diff diff (format "agent-pane diff: %s" (agent-pane--diff-summary diff))))))

(defun agent-pane--message-index-at-point ()
  "Return the message index at point, or nil if point is outside messages."
  (let ((msg (get-text-property (point) 'agent-pane-message)))
    (when msg
      (cl-position msg (map-elt agent-pane--state :messages) :test #'eq))))
(defun agent-pane-jump-next-message ()
  "Jump to the next transcript message block."
  (interactive)
  (agent-pane--ensure-state)
  (agent-pane--ui-sync-transcript)
  (let* ((msgs (map-elt agent-pane--state :messages))
         (idx (agent-pane--message-index-at-point))
         (next-idx (if (numberp idx)
                       (1+ idx)
                     0))
         (next-msg (nth next-idx msgs)))
    (unless next-msg
      (user-error "No next message"))
    (when-let* ((loc (agent-pane--ui-message-loc next-msg))
                (beg (plist-get loc :beg)))
      (goto-char beg))))
(defun agent-pane-jump-previous-message ()
  "Jump to the previous transcript message block."
  (interactive)
  (agent-pane--ensure-state)
  (agent-pane--ui-sync-transcript)
  (let* ((msgs (map-elt agent-pane--state :messages))
         (idx (agent-pane--message-index-at-point))
         (prev-idx (if (numberp idx)
                       (1- idx)
                     (1- (length msgs))))
         (prev-msg (nth prev-idx msgs)))
    (unless prev-msg
      (user-error "No previous message"))
    (when-let* ((loc (agent-pane--ui-message-loc prev-msg))
                (beg (plist-get loc :beg)))
      (goto-char beg))))
(defun agent-pane-toggle-fold-message-at-point ()
  "Toggle folding of the message body at point."
  (interactive)
  (agent-pane--ensure-state)
  (agent-pane--ui-sync-transcript)
  (let ((msg (get-text-property (point) 'agent-pane-message)))
    (unless msg
      (user-error "Point is not on a message block"))
    (unless (hash-table-p agent-pane--fold-overlays)
      (setq-local agent-pane--fold-overlays (make-hash-table :test 'eq)))
    (let* ((loc (agent-pane--ui-message-loc msg))
           (text-beg (plist-get loc :text-beg))
           (text-end (plist-get loc :text-end))
           (existing (gethash msg agent-pane--fold-overlays)))
      (unless (and (markerp text-beg) (markerp text-end))
        (user-error "Message body location unavailable"))
      (if (and (overlayp existing) (overlay-buffer existing))
          (progn
            (delete-overlay existing)
            (remhash msg agent-pane--fold-overlays))
        (let ((ov (make-overlay text-beg text-end (current-buffer) t nil)))
          (overlay-put ov 'invisible 'agent-pane-fold)
          (overlay-put ov 'isearch-open-invisible #'delete-overlay)
          (puthash msg ov agent-pane--fold-overlays))))))
(defvar-local agent-pane--input-editor-origin-buffer nil
  "Origin `agent-pane' buffer for the current input editor buffer.")
(defvar agent-pane-input-editor-buffer-name "*agent-pane input*"
  "Name of the dedicated input editor buffer.")
(defun agent-pane-input-editor-commit ()
  "Commit edited text back into the originating `agent-pane' buffer."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (origin agent-pane--input-editor-origin-buffer)
        (editor (current-buffer)))
    (unless (buffer-live-p origin)
      (user-error "Origin agent-pane buffer is no longer live"))
    (with-current-buffer origin
      (unless (derived-mode-p 'agent-pane-mode)
        (user-error "Origin buffer is not in agent-pane-mode"))
      (agent-pane--set-input-text text)
      (agent-pane--goto-input)
      (goto-char (point-max)))
    (when (buffer-live-p editor)
      (kill-buffer editor))
    (pop-to-buffer origin)))
(defun agent-pane-input-editor-cancel ()
  "Close the input editor without changing chat input text."
  (interactive)
  (let ((origin agent-pane--input-editor-origin-buffer)
        (editor (current-buffer)))
    (when (buffer-live-p editor)
      (kill-buffer editor))
    (when (buffer-live-p origin)
      (pop-to-buffer origin))))
(defvar agent-pane-input-editor-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m text-mode-map)
    (define-key m (kbd "C-c C-c") #'agent-pane-input-editor-commit)
    (define-key m (kbd "C-c C-k") #'agent-pane-input-editor-cancel)
    m)
  "Keymap for `agent-pane-input-editor-mode'.")
(define-derived-mode agent-pane-input-editor-mode text-mode "agent-pane-input"
  "Major mode for editing `agent-pane' input in a dedicated buffer.")
(defun agent-pane-edit-input ()
  "Open a dedicated multiline editor for the current chat input."
  (interactive)
  (agent-pane--ensure-state)
  (let ((origin (current-buffer))
        (text (or (agent-pane--input-text) ""))
        (buf (get-buffer-create agent-pane-input-editor-buffer-name)))
    (with-current-buffer buf
      (agent-pane-input-editor-mode)
      (setq-local agent-pane--input-editor-origin-buffer origin)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-max))))
    (pop-to-buffer buf)))
(defun agent-pane-set-acp-provider (provider)
  "Set ACP PROVIDER for new sessions and reset current connection state.

PROVIDER must be one of `codex', `copilot', `claude-code', or `custom'."
  (interactive
   (list (intern (completing-read "ACP provider: "
                                  '("codex" "copilot" "claude-code" "custom")
                                  nil t nil nil
                                  (symbol-name agent-pane-acp-provider)))))
  (setq agent-pane-acp-provider provider)
  (setq agent-pane-auth-method-id
        (pcase provider
          ('codex "chatgpt")
          ((or 'copilot 'claude-code) nil)
          (_ agent-pane-auth-method-id)))
  (agent-pane--ensure-state)
  ;; Force reconnect on next submit.
  (map-put! agent-pane--state :client nil)
  (map-put! agent-pane--state :session-id nil)
  (map-put! agent-pane--state :subscribed nil)
  (map-put! agent-pane--state :connecting nil)
  (agent-pane--rerender)
  (message "ACP provider set to %s" provider))

(defun agent-pane-set-session-model (model-id)
  "Set ACP MODEL-ID preference and apply it to the active session.
When MODEL-ID is empty, clear the preference and use provider defaults for
new sessions."
  (interactive
   (list (read-string "ACP model id (empty = provider default): "
                      (or agent-pane-session-model-id ""))))
  (let* ((trimmed (string-trim (or model-id "")))
         (model-id* (unless (string-empty-p trimmed) trimmed)))
    (setq agent-pane-session-model-id model-id*)
    (agent-pane--ensure-state)
    (if (null model-id*)
        (message "ACP model preference cleared (provider default for new sessions)")
      (let ((client (map-elt agent-pane--state :client))
            (session-id (map-elt agent-pane--state :session-id)))
        (if (and client session-id)
            (agent-pane--set-session-model
             model-id*
             (lambda (_resp)
               (agent-pane--rerender)
               (message "ACP model set to %s" model-id*))
             (lambda (err &optional _raw)
               (agent-pane--append-message
                'system
                (format "Warning: failed to set session model (%s): %s"
                        model-id*
                        (or (map-elt err 'message) err)))
               (agent-pane--rerender)
               (message "Failed to set ACP model: %s"
                        (or (map-elt err 'message) err))))
          (message "ACP model preference set to %s (applies to new sessions)" model-id*))))))

(defun agent-pane-toggle-header-details ()
  "Toggle verbose ACP header details in the current chat buffer."
  (interactive)
  (setq-local agent-pane--header-details-collapsed
              (not agent-pane--header-details-collapsed))
  (agent-pane--rerender)
  (message "ACP header details %s"
           (if agent-pane--header-details-collapsed "hidden" "shown")))

(defun agent-pane-toggle-tool-output-mode ()
  "Toggle tool output rendering between preview and full modes."
  (interactive)
  (agent-pane--ensure-state)
  (setq-local agent-pane--tool-output-full-mode
              (not agent-pane--tool-output-full-mode))
  (agent-pane--refresh-tool-call-messages)
  (agent-pane--rerender)
  (message "Tool output mode: %s"
           (if agent-pane--tool-output-full-mode
               "full"
             (format "preview (%d lines)" agent-pane-tool-output-preview-lines))))

(defun agent-pane--close-buffer-and-windows (buffer)
  "Close windows showing BUFFER, then kill BUFFER."
  (when (buffer-live-p buffer)
    (dolist (win (get-buffer-window-list buffer nil t))
      (when (window-live-p win)
        (if (one-window-p t win)
            (with-selected-window win
              (switch-to-buffer (other-buffer buffer t)))
          (delete-window win))))
    (kill-buffer buffer)))

(defun agent-pane-exit ()
  "Exit agent-pane by closing chat/sidebar buffers and stopping ACP client."
  (interactive)
  (let ((chat-buf (get-buffer agent-pane-buffer-name))
        (sessions-buf (get-buffer agent-pane-sessions-buffer-name))
        (input-buf (get-buffer agent-pane-input-editor-buffer-name))
        client)
    (when (buffer-live-p chat-buf)
      (with-current-buffer chat-buf
        (when (and (boundp 'agent-pane--state)
                   (listp agent-pane--state))
          (setq client (map-elt agent-pane--state :client)))))
    (when client
      (ignore-errors
        (acp-shutdown :client client)))
    (agent-pane--close-buffer-and-windows input-buf)
    (agent-pane--close-buffer-and-windows sessions-buf)
    (agent-pane--close-buffer-and-windows chat-buf)))

(defun agent-pane-submit ()
  "Submit current input.
This appends the user message to the transcript and, if possible,
sends it to Codex via ACP for a streamed response."
  (interactive)
  (agent-pane--ensure-state)
  (let ((text (agent-pane--input-text)))
    (when (or (null text) (string-empty-p text))
      (user-error "No input"))
    ;; Model update.
    (agent-pane--append-message 'user text)
    ;; Transcript.
    (agent-pane--transcript-log-session-id)
    (agent-pane--transcript-maybe-set-title text)
    (agent-pane--transcript-role-heading 'user)
    (agent-pane--transcript-append (concat text "\n\n"))
    ;; Input history + view update.
    (agent-pane--input-history-push text)
    (agent-pane--clear-input)
    ;; Queue prompt for ACP.
    (when (and agent-pane-enable-acp (not noninteractive))
      (agent-pane--enqueue-prompt text)
      ;; If we're idle, immediately show progress; if already streaming, keep
      ;; the current state and just show the queued count.
      (unless (map-elt agent-pane--state :in-progress)
        (map-put! agent-pane--state :in-progress 'waiting)))
    (agent-pane--rerender)
    (when (and agent-pane-enable-acp (not noninteractive))
      (agent-pane--pump-prompt-queue))))
(defvar agent-pane-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-pane-submit)
    (define-key m (kbd "C-c C-k") #'agent-pane-cancel)
    (define-key m (kbd "C-c C-q") #'agent-pane-exit)
    (define-key m (kbd "C-c C-t") #'agent-pane-open-acp-traffic)
    (define-key m (kbd "C-c C-l") #'agent-pane-open-acp-logs)
    (define-key m (kbd "C-c C-e") #'agent-pane-open-acp-stderr)
    (define-key m (kbd "C-c C-s") #'agent-pane-show-state)
    (define-key m (kbd "C-c C-r") #'agent-pane-show-raw-at-point)
    (define-key m (kbd "C-c C-a") #'agent-pane-set-acp-provider)
    (define-key m (kbd "C-c C-v") #'agent-pane-toggle-header-details)
    (define-key m (kbd "C-c v") #'agent-pane-toggle-header-details)
    (define-key m (kbd "C-c C-m") #'agent-pane-set-session-model)
    (define-key m (kbd "C-c C-d") #'agent-pane-view-diff-at-point)
    (define-key m (kbd "C-c C-o") #'agent-pane-toggle-tool-output-mode)
    (define-key m (kbd "C-c C-i") #'agent-pane-edit-input)
    (define-key m (kbd "C-c C-y") #'agent-pane-copy-last-user-to-input)
    (define-key m (kbd "C-c C-w") #'agent-pane-copy-last-tool-output)
    (define-key m (kbd "C-c C-b") #'agent-pane-copy-code-block-at-point)
    (define-key m (kbd "M-p") #'agent-pane-input-history-prev)
    (define-key m (kbd "M-n") #'agent-pane-input-history-next)
    (define-key m (kbd "C-c C-p") #'agent-pane-jump-previous-message)
    (define-key m (kbd "C-c C-n") #'agent-pane-jump-next-message)
    (define-key m (kbd "C-c C-f") #'agent-pane-toggle-fold-message-at-point)
    m)
  "Keymap for `agent-pane-mode'.")
(define-derived-mode agent-pane-mode fundamental-mode "agent-pane"
  "Major mode for agent-pane buffers."
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  ;; Hide markdown markup characters when configured.
  (when agent-pane-markdown-hide-markup
    (add-to-invisibility-spec 'agent-pane-markup))
  ;; Message folding uses its own invisibility symbol.
  (add-to-invisibility-spec 'agent-pane-fold)
  (setq-local agent-pane--state (agent-pane--make-state))
  (setq-local agent-pane--header-details-collapsed
              agent-pane-header-details-collapsed)
  (agent-pane--rerender)
  (agent-pane--goto-input))
(defun agent-pane-app ()
  "Open agent-pane with a left sidebar listing saved sessions."
  (interactive)
  (let ((chat-buf (get-buffer-create agent-pane-buffer-name))
        (sessions-buf (get-buffer-create agent-pane-sessions-buffer-name)))
    (delete-other-windows)
    (let* ((main (selected-window))
           (side (split-window main agent-pane-sessions-sidebar-width 'left)))
      (set-window-buffer side sessions-buf)
      (with-current-buffer sessions-buf
        (unless (derived-mode-p 'agent-pane-sessions-mode)
          (agent-pane-sessions-mode)))
      (set-window-dedicated-p side t)
      (select-window main)
      (pop-to-buffer chat-buf)
      (with-current-buffer chat-buf
        (unless (derived-mode-p 'agent-pane-mode)
          (agent-pane-mode))))))
;;;###autoload
(defun agent-pane ()
  "Open agent-pane.
When `agent-pane-use-sidebar' is non-nil, open the full app layout (with a
left sidebar listing saved sessions).  Otherwise, open only the chat buffer."
  (interactive)
  (if agent-pane-use-sidebar
      (agent-pane-app)
    (let ((buf (get-buffer-create agent-pane-buffer-name)))
      (pop-to-buffer buf)
      (with-current-buffer buf
        (unless (derived-mode-p 'agent-pane-mode)
          (agent-pane-mode))))))
(provide 'agent-pane)
;;; agent-pane.el ends here
