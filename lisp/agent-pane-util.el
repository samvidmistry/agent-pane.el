;;; agent-pane-util.el --- Small helpers for agent-pane -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;;; Commentary:
;;
;; Misc utilities shared across the agent-pane modules.
;;
;;; Code:
(require 'agent-pane-config)
(require 'map)
(require 'pp)
(require 'subr-x)
(defvar agent-pane-acp-provider)
(defvar agent-pane-codex-command)
(defvar agent-pane-copilot-command)
(defvar agent-pane-claude-code-command)
(defvar agent-pane-codex-config-overrides)
(defvar agent-pane-codex-environment)
(defvar agent-pane-copilot-environment)
(defvar agent-pane-claude-code-environment)
(defun agent-pane--acp-provider-command-base ()
  "Return command list for current `agent-pane-acp-provider'."
  (pcase agent-pane-acp-provider
    ('copilot agent-pane-copilot-command)
    ('claude-code agent-pane-claude-code-command)
    ((or 'codex 'custom) agent-pane-codex-command)
    (_ agent-pane-codex-command)))

(defun agent-pane--acp-command-and-params ()
  "Return a (COMMAND . PARAMS) pair for launching the selected ACP provider."
  (let* ((command (copy-sequence (or (agent-pane--acp-provider-command-base)
                                     agent-pane-codex-command
                                     '("codex-acp"))))
         (cmd (car command))
         (params (cdr command))
         (overrides (if (eq agent-pane-acp-provider 'codex)
                        (mapcan (lambda (s) (list "-c" s))
                                agent-pane-codex-config-overrides)
                      nil)))
    (cons cmd (append params overrides))))

(defun agent-pane--acp-environment ()
  "Return environment variable list for current ACP provider."
  (pcase agent-pane-acp-provider
    ('copilot agent-pane-copilot-environment)
    ('claude-code agent-pane-claude-code-environment)
    (_ agent-pane-codex-environment)))

(defun agent-pane--codex-command-and-params ()
  "Compatibility wrapper for older callers.

Prefer `agent-pane--acp-command-and-params'."
  (agent-pane--acp-command-and-params))
(defun agent-pane--listify (x)
  "Return X as a list.
If X is a vector, convert it to a list.  If X is nil, return nil."
  (cond
   ((null x) nil)
   ((vectorp x) (append x nil))
   ((listp x) x)
   (t (list x))))
(defun agent-pane--pp-to-string (obj)
  "Pretty-print OBJ to a string."
  (with-temp-buffer
    (let ((print-circle t)
          (print-level nil)
          (print-length nil))
      (pp obj (current-buffer))
      (string-trim-right (buffer-string)))))
(defun agent-pane--slugify (s)
  "Return a filesystem-friendly slug for S."
  (let ((s (downcase (or s ""))))
    (setq s (replace-regexp-in-string "[^[:alnum:]]+" "-" s))
    (setq s (replace-regexp-in-string "^-+" "" s))
    (replace-regexp-in-string "-+$" "" s)))
(defun agent-pane--session-title-from-text (text)
  "Derive a one-line session title from TEXT."
  (let* ((s (replace-regexp-in-string "[\n\t ]+" " " (or text "")))
         (s (string-trim s)))
    (truncate-string-to-width s 80 nil nil "…")))
(provide 'agent-pane-util)
;;; agent-pane-util.el ends here
