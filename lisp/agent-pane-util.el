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

(defun agent-pane--codex-command-and-params ()

  "Return a (COMMAND . PARAMS) pair for launching codex-acp."

  (let* ((cmd (car agent-pane-codex-command))

         (params (cdr agent-pane-codex-command))

         (overrides (mapcan (lambda (s) (list "-c" s))

                            agent-pane-codex-config-overrides)))

    (cons cmd (append params overrides))))

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

