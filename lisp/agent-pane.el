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

(require 'acp)

(require 'map)

(require 'subr-x)

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

    ;; View update.

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

    (define-key m (kbd "C-c C-t") #'agent-pane-open-acp-traffic)

    (define-key m (kbd "C-c C-l") #'agent-pane-open-acp-logs)

    (define-key m (kbd "C-c C-e") #'agent-pane-open-acp-stderr)

    (define-key m (kbd "C-c C-s") #'agent-pane-show-state)

    (define-key m (kbd "C-c C-r") #'agent-pane-show-raw-at-point)

    m)

  "Keymap for `agent-pane-mode'.")

(define-derived-mode agent-pane-mode fundamental-mode "agent-pane"

  "Major mode for agent-pane buffers."

  (setq-local truncate-lines t)

  ;; Hide markdown markup characters when configured.

  (when agent-pane-markdown-hide-markup

    (add-to-invisibility-spec 'agent-pane-markup))

  (setq-local agent-pane--state (agent-pane--make-state))

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

