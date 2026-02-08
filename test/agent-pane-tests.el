;;; agent-pane-tests.el --- Tests for agent-pane -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-pane)

;;; Code:

(ert-deftest agent-pane-rerender-creates-input-marker ()
  (with-temp-buffer
    (agent-pane-mode)
    (should (markerp agent-pane--input-marker))
    (should (<= (marker-position agent-pane--input-marker) (point-max)))))

(ert-deftest agent-pane-submit-appends-messages ()
  (with-temp-buffer
    (agent-pane-mode)
    (goto-char (point-max))
    (insert "hello")
    (agent-pane-submit)
    (let ((msgs (map-elt agent-pane--state :messages)))
      ;; In batch mode, agent-pane does not spawn ACP processes, so submitting
      ;; only adds the user message.
      (should (= (length msgs) 1))
      (should (eq (map-elt (nth 0 msgs) :role) 'user))
      (should (equal (map-elt (nth 0 msgs) :text) "hello")))))

(ert-deftest agent-pane-ensure-acp-client-applies-config-overrides ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((agent-pane-acp-client-maker nil)
          (agent-pane-codex-command '("codex-acp" "--flag"))
          (agent-pane-codex-config-overrides '("model_reasoning_effort=\"high\""
                                               "sandbox_mode=\"danger-full-access\"")))
      (map-put! agent-pane--state :client nil)
      (agent-pane--ensure-acp-client)
      (let ((client (map-elt agent-pane--state :client)))
        (should (equal (map-elt client :command) "codex-acp"))
        (should (equal (map-elt client :command-params)
                       '("--flag"
                         "-c" "model_reasoning_effort=\"high\""
                         "-c" "sandbox_mode=\"danger-full-access\"")))))))

(ert-deftest agent-pane-ui-status-line-shows-waiting ()
  (with-temp-buffer
    (agent-pane-mode)
    (map-put! agent-pane--state :in-progress 'waiting)
    (agent-pane--rerender)
    (let* ((start (marker-position agent-pane--status-beg-marker))
           (end (marker-position agent-pane--status-end-marker))
           (s (buffer-substring-no-properties start end)))
      (should (string-match-p "waiting for agent" s))
      (should (string-match-p "waiting for agent" (format "%s" header-line-format))))))

(ert-deftest agent-pane-prompt-queue-sends-followups-in-order ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((agent-pane-enable-acp t)
          (noninteractive nil)
          (sent nil))
      (map-put! agent-pane--state :client (list (cons :agent-pane-fake t)))
      (map-put! agent-pane--state :session-id "fake")
      (cl-letf (((symbol-function 'agent-pane--send-prompt)
                 (lambda (text)
                   ;; Assert we marked the prompt as in-flight before calling.
                   (should (map-elt agent-pane--state :prompt-in-flight))
                   (push text sent)
                   ;; Simulate end-of-turn.
                   (map-put! agent-pane--state :prompt-in-flight nil)
                   (map-put! agent-pane--state :in-progress nil)
                   (agent-pane--pump-prompt-queue))))
        (agent-pane--enqueue-prompt "first")
        (agent-pane--enqueue-prompt "second")
        (agent-pane--pump-prompt-queue)
        (should (equal (nreverse sent) '("first" "second")))
        (should (null (map-elt agent-pane--state :prompt-queue)))))))

(ert-deftest agent-pane-ui-streaming-does-not-erase-buffer ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((orig (symbol-function 'erase-buffer))
          (erase-count 0))
      (cl-letf (((symbol-function 'erase-buffer)
                 (lambda ()
                   (setq erase-count (1+ erase-count))
                   (funcall orig))))
        (setq erase-count 0)
        (agent-pane--on-notification
         '((jsonrpc . "2.0")
           (method . "session/update")
           (params . ((sessionId . "s1")
                     (update . ((sessionUpdate . "agent_message_chunk")
                                (content . ((type . "text")
                                            (text . "Hi")))))))))
        (should (= erase-count 0))
        (should (string-match-p "Hi" (buffer-string)))))))

(ert-deftest agent-pane-ui-footer-stays-at-bottom-while-streaming ()
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--on-notification
     '((jsonrpc . "2.0")
       (method . "session/update")
       (params . ((sessionId . "s1")
                 (update . ((sessionUpdate . "agent_message_chunk")
                            (content . ((type . "text")
                                        (text . "Hello")))))))))
    (let* ((s (buffer-string))
           (pos-msg (string-match "Hello" s))
           (pos-footer (string-match "Type your message below" s)))
      (should (numberp pos-msg))
      (should (numberp pos-footer))
      ;; Footer should come after transcript.
      (should (< pos-msg pos-footer)))))

(ert-deftest agent-pane-transcripts-saved-on-submit ()
  (let ((agent-pane-save-transcripts 'always)
        (dir (make-temp-file "agent-pane-transcripts" t)))
    (unwind-protect
        (let ((agent-pane-transcript-directory dir))
          (with-temp-buffer
            (agent-pane-mode)
            (goto-char (point-max))
            (insert "hello transcript")
            (agent-pane-submit)
            (let ((file (map-elt agent-pane--state :transcript-file)))
              (should (stringp file))
              (should (file-exists-p file))
              (with-temp-buffer
                (insert-file-contents file)
                (should (string-match-p "hello transcript" (buffer-string)))
                (should (string-match-p "## User" (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-markdown-assistant-strong-is-colored ()
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--append-message* :role 'assistant :text "Hi **there**")
    (agent-pane--rerender)
    (goto-char (point-min))
    (should (re-search-forward "\\*\\*there\\*\\*" nil t))
    (let ((beg (match-beginning 0))
          (end (match-end 0)))
      (should (eq (get-text-property beg 'face) 'agent-pane-markdown-delimiter))
      (should (eq (get-text-property beg 'invisible) 'agent-pane-markup))
      (should (eq (get-text-property (+ beg 2) 'face) 'agent-pane-markdown-strong-assistant))
      (should (eq (get-text-property (1- end) 'face) 'agent-pane-markdown-delimiter))
      (should (eq (get-text-property (1- end) 'invisible) 'agent-pane-markup)))))

(ert-deftest agent-pane-markdown-thought-strong-is-bold ()
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--append-message* :role 'thought :text "**bold**")
    (agent-pane--rerender)
    (goto-char (point-min))
    (should (re-search-forward "\\*\\*bold\\*\\*" nil t))
    (let ((beg (match-beginning 0)))
      (should (eq (get-text-property beg 'invisible) 'agent-pane-markup))
      (should (eq (get-text-property (+ beg 2) 'face) 'agent-pane-markdown-strong-thought)))))

(ert-deftest agent-pane-markdown-strong-spans-chunk-boundary ()
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--on-notification
     '((jsonrpc . "2.0")
       (method . "session/update")
       (params . ((sessionId . "s1")
                 (update . ((sessionUpdate . "agent_message_chunk")
                            (content . ((type . "text")
                                        (text . "**Bo")))))))))
    (agent-pane--on-notification
     '((jsonrpc . "2.0")
       (method . "session/update")
       (params . ((sessionId . "s1")
                 (update . ((sessionUpdate . "agent_message_chunk")
                            (content . ((type . "text")
                                        (text . "ld**")))))))))
    (goto-char (point-min))
    (should (search-forward "Bold" nil t))
    (should (eq (get-text-property (1- (point)) 'face)
                'agent-pane-markdown-strong-assistant))
    (goto-char (point-min))
    (re-search-forward "\\*\\*Bold\\*\\*" nil t)
    (should (eq (get-text-property (match-beginning 0) 'invisible)
                'agent-pane-markup))))

(provide 'agent-pane-tests)
;;; agent-pane-tests.el ends here
