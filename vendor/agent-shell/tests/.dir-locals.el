((nil . ((eval . (defun agent-shell-run-all-tests ()
                   "Run all agent-shell tests in batch mode."
                   (interactive)
                   (let ((test-dir (expand-file-name "tests/" (project-root (project-current t)))))
                     (dolist (file (directory-files-recursively test-dir "\\.el$"))
                       (unless (string-match-p "/\\." file)
                         (load file)))
                     (if noninteractive
                         (ert-run-tests-batch-and-exit "^agent-shell")
                       (ert "^agent-shell"))))))))
