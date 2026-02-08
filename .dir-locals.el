((nil . ((eval . (defun agent-pane-run-all-tests ()
                   "Run all agent-pane tests in batch mode.

Loads `test/*.el' and runs ERT tests starting with \"agent-pane\"."
                   (interactive)
                   (let* ((root (if (fboundp 'project-root)
                                    (project-root (project-current t))
                                  default-directory))
                          (test-dir (expand-file-name "test/" root)))
                     (let ((runner (file-truename (expand-file-name "test-runner.el" test-dir))))
                       (dolist (file (directory-files test-dir t "\\.el\\'"))
                        (unless (string-match-p "/\\." file)
                         (unless (equal (file-truename file) runner)
                           (load file)))))
                     (if noninteractive
                         (ert-run-tests-batch-and-exit "^agent-pane")
                       (ert "^agent-pane"))))))))
