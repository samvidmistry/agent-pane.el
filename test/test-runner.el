;;; test-runner.el --- Batch test runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Invoked by scripts/test.sh.

;;; Code:

(require 'ert)

(setq load-prefer-newer t)

(let* ((root (file-name-directory (directory-file-name (file-name-directory load-file-name))))
       (test-dir (expand-file-name "test" root)))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (add-to-list 'load-path (expand-file-name "vendor/acp.el" root))
  (let ((runner (file-truename (expand-file-name "test-runner.el" test-dir))))
    (dolist (f (directory-files test-dir t "\\.el\\'"))
      (unless (or (string-match-p "/\\." f)
                  (equal (file-truename f) runner))
        (load f)))))

(ert-run-tests-batch-and-exit "^agent-pane")

;;; test-runner.el ends here
