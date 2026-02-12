;;; agent-pane-dispatch.el --- Transient command menu for agent-pane -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1") (transient "0.4.0"))

;;; Commentary:
;;
;; Defines a transient menu that exposes common `agent-pane' commands in one
;; place.
;;
;;; Code:

(require 'transient)

(declare-function agent-pane-submit "agent-pane")
(declare-function agent-pane-cancel "agent-pane")
(declare-function agent-pane-exit "agent-pane")
(declare-function agent-pane-set-acp-provider "agent-pane")
(declare-function agent-pane-set-session-model "agent-pane")
(declare-function agent-pane-toggle-header-details "agent-pane")
(declare-function agent-pane-toggle-tool-output-mode "agent-pane")
(declare-function agent-pane-view-diff-at-point "agent-pane")
(declare-function agent-pane-show-raw-at-point "agent-pane")
(declare-function agent-pane-show-state "agent-pane")
(declare-function agent-pane-jump-previous-message "agent-pane")
(declare-function agent-pane-jump-next-message "agent-pane")
(declare-function agent-pane-toggle-fold-message-at-point "agent-pane")
(declare-function agent-pane-edit-input "agent-pane")
(declare-function agent-pane-copy-last-user-to-input "agent-pane")
(declare-function agent-pane-copy-last-tool-output "agent-pane")
(declare-function agent-pane-copy-code-block-at-point "agent-pane")
(declare-function agent-pane-open-acp-traffic "agent-pane")
(declare-function agent-pane-open-acp-logs "agent-pane")
(declare-function agent-pane-open-acp-stderr "agent-pane")
(declare-function agent-pane-sessions "agent-pane-sessions")

;;;###autoload
(transient-define-prefix agent-pane-dispatch ()
  "Open the `agent-pane' command menu."
  [["Run"
    ("RET" "Send" agent-pane-submit)
    ("k" "Cancel" agent-pane-cancel)
    ("q" "Exit app" agent-pane-exit)]
   ["Session"
    ("a" "Provider" agent-pane-set-acp-provider)
    ("m" "Model" agent-pane-set-session-model)
    ("v" "Header details" agent-pane-toggle-header-details)
    ("o" "Tool output mode" agent-pane-toggle-tool-output-mode)]
   ["Inspect"
    ("d" "View diff at point" agent-pane-view-diff-at-point)
    ("r" "Show raw at point" agent-pane-show-raw-at-point)
    ("s" "Show state" agent-pane-show-state)]]
  [["Navigate"
    ("p" "Prev message" agent-pane-jump-previous-message)
    ("n" "Next message" agent-pane-jump-next-message)
    ("f" "Fold at point" agent-pane-toggle-fold-message-at-point)
    ("i" "Edit input" agent-pane-edit-input)]
   ["Copy"
    ("y" "Copy last user to input" agent-pane-copy-last-user-to-input)
    ("w" "Copy last tool output" agent-pane-copy-last-tool-output)
    ("b" "Copy code block" agent-pane-copy-code-block-at-point)]
   ["Buffers"
    ("T" "ACP traffic" agent-pane-open-acp-traffic)
    ("L" "ACP logs" agent-pane-open-acp-logs)
    ("E" "ACP stderr" agent-pane-open-acp-stderr)
    ("S" "Sessions sidebar" agent-pane-sessions)]])

(provide 'agent-pane-dispatch)
;;; agent-pane-dispatch.el ends here
