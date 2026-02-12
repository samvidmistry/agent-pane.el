;;; ui-replay.el --- Batch UI replay for agent-pane -*- lexical-binding: t; -*-

;;; Commentary:
;; Invoked by scripts/ui-replay.sh.
;;
;; Replays an ACP traffic file via acp-fakes, renders `agent-pane' in a
;; temporary buffer, and writes the final rendered buffer text to an output
;; file. This gives agents a deterministic way to "see" the UI in headless
;; environments.

;;; Code:

(require 'agent-pane)
(require 'acp-fakes)
(require 'acp-traffic)

(defun agent-pane-ui-replay--usage ()
  "Print command usage and exit with a non-zero status."
  (princ "Usage: ui-replay.el <traffic-file> <output-file> [prompt]\n")
  (kill-emacs 2))

(let* ((args (if (equal (car command-line-args-left) "--")
                 (cdr command-line-args-left)
               command-line-args-left))
       (traffic-file (pop args))
       (output-file (pop args))
       (prompt (or (pop args) "hi")))
  (setq command-line-args-left nil)
  (unless (and traffic-file output-file)
    (agent-pane-ui-replay--usage))
  (let ((traffic (acp-traffic-read-file traffic-file)))
    (with-temp-buffer
      (agent-pane-mode)
      (let ((agent-pane-acp-client-maker
             (lambda (_buffer)
               (cons (cons :agent-pane-fake t)
                     (acp-fakes-make-client traffic)))))
        (agent-pane--handshake
         (lambda ()
           (agent-pane--send-prompt prompt))))
      (write-region (buffer-substring-no-properties (point-min) (point-max))
                    nil output-file nil 'quiet)
      (princ (format "Wrote UI snapshot: %s\n" output-file)))))

;;; ui-replay.el ends here
