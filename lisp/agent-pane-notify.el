;;; agent-pane-notify.el --- Built-in notification helpers -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;;; Commentary:
;;
;; Ready-made hook functions for `agent-pane' lifecycle hooks.
;; None are added to hooks by default; users opt in via their init file:
;;
;;   (add-hook 'agent-pane-turn-end-hook  #'agent-pane-notify-on-turn-end)
;;   (add-hook 'agent-pane-error-hook     #'agent-pane-notify-on-error)
;;   (add-hook 'agent-pane-cancel-hook    #'agent-pane-notify-on-cancel)
;;   (add-hook 'agent-pane-permission-request-hook
;;             #'agent-pane-notify-on-permission)
;;
;;; Code:
(require 'agent-pane-config)

(defvar agent-pane-notify-style)

(defun agent-pane-notify (text)
  "Display TEXT according to `agent-pane-notify-style'.
When style is `message', show TEXT in the echo area.
When style is `message+bell', also ring the terminal bell.
When style is nil, do nothing."
  (pcase agent-pane-notify-style
    ('message      (message "%s" text))
    ('message+bell (ding t) (message "%s" text))))

(defun agent-pane-notify-on-turn-end ()
  "Notify that the agent turn finished.
Suitable for `agent-pane-turn-end-hook'."
  (agent-pane-notify "agent-pane: turn complete"))

(defun agent-pane-notify-on-error (err-text)
  "Notify about an ACP error described by ERR-TEXT.
Suitable for `agent-pane-error-hook'."
  (agent-pane-notify (format "agent-pane: %s" err-text)))

(defun agent-pane-notify-on-permission (_params)
  "Notify that a permission request arrived.
PARAMS is the request params alist (ignored by the built-in notifier).
Suitable for `agent-pane-permission-request-hook'."
  (agent-pane-notify "agent-pane: permission requested"))

(defun agent-pane-notify-on-cancel ()
  "Notify that the user cancelled the current run.
Suitable for `agent-pane-cancel-hook'."
  (agent-pane-notify "agent-pane: run cancelled"))

(provide 'agent-pane-notify)
;;; agent-pane-notify.el ends here
