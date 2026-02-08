;;; agent-shell-runner-tests.el --- Tests for agent-shell command runner functionality -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)

;;; Code:

(ert-deftest agent-shell--build-command-for-execution-test ()
  "Test `agent-shell--build-command-for-execution' function."

  ;; Test 1: No container runner configured (nil)
  (let ((agent-shell-container-command-runner nil))
    (should (equal (agent-shell--build-command-for-execution
                    '("claude-code-acp"))
                   '("claude-code-acp"))))

  ;; Test 2: Static list runner
  (let ((agent-shell-container-command-runner
         '("devcontainer" "exec" "--workspace-folder" ".")))
    (should (equal (agent-shell--build-command-for-execution
                    '("claude-code-acp"))
                   '("devcontainer" "exec" "--workspace-folder" "." "claude-code-acp"))))

  ;; Test 3, 4 & 5: Function runner with different agent identifiers
  (let ((agent-shell-container-command-runner
         (lambda (buffer)
           (let ((config (agent-shell-get-config buffer)))
             (pcase (map-elt config :identifier)
               ('claude-code '("docker" "exec" "claude-dev" "--"))
               ('gemini-cli '("docker" "exec" "gemini-dev" "--"))
               (_ '("devcontainer" "exec" ".")))))))
    (let ((test-cases '(((:identifier . claude-code)
                         (:command . ("claude-code-acp"))
                         (:expected-prefix . ("docker" "exec" "claude-dev" "--")))
                        ((:identifier . gemini-cli)
                         (:command . ("gemini"))
                         (:expected-prefix . ("docker" "exec" "gemini-dev" "--")))
                        ((:identifier . unknown-agent)
                         (:command . ("some-agent"))
                         (:expected-prefix . ("devcontainer" "exec" "."))))))
      (seq-doseq (test-case test-cases)
        (let* ((identifier (map-elt test-case :identifier))
               (command (map-elt test-case :command))
               (expected-prefix (map-elt test-case :expected-prefix))
               (test-buffer (generate-new-buffer (format "*test-%s*" identifier))))
          (unwind-protect
              (with-current-buffer test-buffer
                (setq-local agent-shell--state
                            (list (cons :agent-config
                                        `((:identifier . ,identifier)))))
                (should (equal (agent-shell--build-command-for-execution command)
                               (append expected-prefix command))))
            (kill-buffer test-buffer))))))

  ;; Test 6: Complex command with multiple arguments
  (let ((agent-shell-container-command-runner
         '("docker" "exec" "my-container")))
    (should (equal (agent-shell--build-command-for-execution
                    '("shell" "-c" "echo test"))
                   '("docker" "exec" "my-container" "shell" "-c" "echo test")))))

(provide 'agent-shell-runner-tests)
;;; agent-shell-runner-tests.el ends here
