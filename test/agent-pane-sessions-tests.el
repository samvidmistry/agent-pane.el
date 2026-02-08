;;; agent-pane-sessions-tests.el --- Tests for agent-pane sessions sidebar -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-pane)
(require 'agent-pane-sessions)

;;; Code:

(defun agent-pane-test--write (file content)
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil file nil 'silent)))

(ert-deftest agent-pane-sessions-scan-groups-by-project-root ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (f1 (expand-file-name "20260101-010101--proj--aaaa1111.md" dir))
         (f2 (expand-file-name "20260102-010101--proj--aaaa1111.md" dir))
         (f3 (expand-file-name "20260103-010101--other--bbbb2222.md" dir)))
    (unwind-protect
        (progn
          (agent-pane-test--write
           f1
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-01 01:01:01"
              "- project_root: /tmp/proj"
              ""
              "---"
              ""
              "## User — 2026-01-01 01:01:02"
              ""
              "hello")
            "\n"))
          (agent-pane-test--write
           f2
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-02 01:01:01"
              "- project_root: /tmp/proj"
              ""
              "---"
              "")
            "\n"))
          (agent-pane-test--write
           f3
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-03 01:01:01"
              "- project_root: /tmp/other"
              ""
              "---"
              "")
            "\n"))
          (let* ((groups (agent-pane-sessions--scan))
                 (proj (directory-file-name (expand-file-name "/tmp/proj")))
                 (other (directory-file-name (expand-file-name "/tmp/other"))))
            (should (= (length groups) 2))
            (should (= (length (cdr (assoc proj groups))) 2))
            (should (= (length (cdr (assoc other groups))) 1))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-mode-renders-project-and-session-lines ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (f (expand-file-name "20260101-010101--proj--aaaa1111.md" dir)))
    (unwind-protect
        (progn
          (agent-pane-test--write
           f
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-01 01:01:01"
              "- project_root: /tmp/proj"
              ""
              "---"
              ""
              "## User — 2026-01-01 01:01:02"
              ""
              "hello")
            "\n"))
          (with-temp-buffer
            (agent-pane-sessions-mode)
            (goto-char (point-min))
            (should (re-search-forward "^\\* " nil t))
            (should (re-search-forward "^\\*\\* " nil t))
            (goto-char (point-min))
            (re-search-forward "^\\*\\* " nil t)
            (should (stringp (get-text-property (point) 'agent-pane-session-file)))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-sidebar-collapses-project ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (f (expand-file-name "20260101-010101--proj--aaaa1111.md" dir)))
    (unwind-protect
        (progn
          (agent-pane-test--write
           f
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-01 01:01:01"
              "- project_root: /tmp/proj"
              ""
              "---"
              "")
            "\n"))
          (with-temp-buffer
            (agent-pane-sessions-mode)
            (goto-char (point-min))
            (re-search-forward "^\\* " nil t)
            (beginning-of-line)
            (outline-toggle-children)
            (forward-line 1)
            (beginning-of-line)
            (should (outline-invisible-p (point)))))
      (ignore-errors (delete-directory dir t)))))

(provide 'agent-pane-sessions-tests)
;;; agent-pane-sessions-tests.el ends here
