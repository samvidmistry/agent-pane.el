;;; agent-pane-sessions-tests.el --- Tests for agent-pane sessions sidebar -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'map)
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
            (should (eq (get-text-property (point) 'face) 'agent-pane-sessions-title))
            (should (re-search-forward "^\\* " nil t))
            (should (eq (get-text-property (line-beginning-position) 'face)
                        'agent-pane-sessions-project))
            (should (re-search-forward "^\\*\\* " nil t))
            (should (eq (get-text-property (line-beginning-position) 'face)
                        'agent-pane-sessions-session))
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

(ert-deftest agent-pane-sessions-parse-transcript-messages-parses-roles ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
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
              "hello"
              ""
              "## Agent — 2026-01-01 01:01:03"
              ""
              "world"
              ""
              "## Tool (completed: bash) — 2026-01-01 01:01:04"
              ""
              "ok")
            "\n"))
          (let ((msgs (agent-pane-sessions--parse-transcript-messages f)))
            (should (= (length msgs) 3))
            (should (eq (map-elt (nth 0 msgs) :role) 'user))
            (should (equal (map-elt (nth 0 msgs) :text) "hello"))
            (should (eq (map-elt (nth 1 msgs) :role) 'assistant))
            (should (equal (map-elt (nth 1 msgs) :text) "world"))
            (should (eq (map-elt (nth 2 msgs) :role) 'tool))
            (should (equal (map-elt (nth 2 msgs) :title) "completed: bash"))
            (should (equal (map-elt (nth 2 msgs) :text) "ok"))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-metadata-title-falls-back-to-you-heading ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
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
              "## You — 2026-01-01 01:01:02"
              ""
              "custom first line"
              ""
              "## Agent — 2026-01-01 01:01:03"
              ""
              "hello")
            "\n"))
          (let ((meta (agent-pane-sessions--parse-transcript-metadata f)))
            (should (equal (plist-get meta :title) "custom first line"))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-open-at-point-replays-in-chat-buffer ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (f (expand-file-name "20260101-010101--proj--aaaa1111.md" dir))
         (agent-pane-buffer-name "*agent-pane replay test*")
         (chat-buf nil))
    (unwind-protect
        (progn
          (agent-pane-test--write
           f
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-01 01:01:01"
              "- project_root: /tmp/proj"
              "- acp_session_id: sid-123"
              ""
              "---"
              ""
              "## User — 2026-01-01 01:01:02"
              ""
              "hello"
              ""
              "## Agent — 2026-01-01 01:01:03"
              ""
              "world")
            "\n"))
          (let (opened)
            (with-temp-buffer
              (insert "** session\n")
              (add-text-properties (point-min) (point-max)
                                   (list 'agent-pane-session-file f
                                         'agent-pane-project-root "/tmp/proj"))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buf &rest _args)
                           (push buf opened)
                           buf)))
                (agent-pane-sessions-open-at-point)))
            (setq chat-buf (car opened)))
          (should (buffer-live-p chat-buf))
          (with-current-buffer chat-buf
            (let ((msgs (map-elt agent-pane--state :messages)))
              (should (= (length msgs) 2))
              (should (eq (map-elt (nth 0 msgs) :role) 'user))
              (should (equal (map-elt (nth 0 msgs) :text) "hello"))
              (should (eq (map-elt (nth 1 msgs) :role) 'assistant))
              (should (equal (map-elt (nth 1 msgs) :text) "world"))
              (should (equal (map-elt agent-pane--state :transcript-file) f))
              (should (equal (map-elt agent-pane--state :resume-session-id) "sid-123")))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-open-at-point-reuses-right-pane-when-sidebar-visible ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (f (expand-file-name "20260101-010101--proj--aaaa1111.md" dir))
         (sessions-buf (get-buffer-create agent-pane-sessions-buffer-name))
         (scratch-right (get-buffer-create "*agent-pane right-pane*"))
         pop-called)
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
          (save-window-excursion
            (delete-other-windows)
            (let* ((main (selected-window))
                   (side (split-window main agent-pane-sessions-sidebar-width 'left)))
              (set-window-buffer side sessions-buf)
              (set-window-dedicated-p side t)
              (set-window-buffer main scratch-right)
              (with-current-buffer sessions-buf
                (erase-buffer)
                (insert "** session\n")
                (add-text-properties (point-min) (point-max)
                                     (list 'agent-pane-session-file f
                                           'agent-pane-project-root "/tmp/proj"))
                (goto-char (point-min))
                (cl-letf (((symbol-function 'pop-to-buffer)
                           (lambda (&rest _args)
                             (setq pop-called t)
                             nil)))
                  (agent-pane-sessions-open-at-point)))
              (should-not pop-called)
              (should (eq (window-buffer side) sessions-buf))
              (should (= (length (cl-remove-if #'window-minibuffer-p
                                               (window-list)))
                         2))
              (with-current-buffer (window-buffer main)
                (should (derived-mode-p 'agent-pane-mode))
                (should (equal (map-elt agent-pane--state :transcript-file) f))))))
      (ignore-errors (kill-buffer sessions-buf))
      (ignore-errors (kill-buffer scratch-right))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-open-at-point-keeps-existing-running-buffer ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (agent-pane-buffer-name "*agent-pane replay test*")
         (f1 (expand-file-name "20260101-010101--proj--aaaa1111.md" dir))
         (f2 (expand-file-name "20260102-010101--proj--bbbb2222.md" dir))
         (opened nil))
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
              "hello 1")
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
              ""
              "## User — 2026-01-02 01:01:02"
              ""
              "hello 2")
            "\n"))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf &rest _args)
                       (push buf opened)
                       buf)))
            (with-temp-buffer
              (insert "** s1\n")
              (add-text-properties (point-min) (point-max)
                                   (list 'agent-pane-session-file f1
                                         'agent-pane-project-root "/tmp/proj"))
              (goto-char (point-min))
              (agent-pane-sessions-open-at-point))
            (let ((buf1 (car opened)))
              (with-current-buffer buf1
                (map-put! agent-pane--state :in-progress 'streaming))
              (with-temp-buffer
                (insert "** s2\n")
                (add-text-properties (point-min) (point-max)
                                     (list 'agent-pane-session-file f2
                                           'agent-pane-project-root "/tmp/proj"))
                (goto-char (point-min))
                (agent-pane-sessions-open-at-point))
              (let ((buf2 (car opened)))
                (should (buffer-live-p buf1))
                (should (buffer-live-p buf2))
                (should-not (eq buf1 buf2))
                (with-current-buffer buf1
                  (should (eq (map-elt agent-pane--state :in-progress) 'streaming))
                  (should (equal (map-elt agent-pane--state :transcript-file) f1)))
                (with-current-buffer buf2
                  (should (equal (map-elt agent-pane--state :transcript-file) f2))))))
          (dolist (b opened)
            (when (buffer-live-p b)
              (kill-buffer b))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-status-indicators-render-live-state ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (agent-pane-buffer-name "*agent-pane replay test*")
         (f-run (expand-file-name "20260101-010101--proj--run.md" dir))
         (f-done (expand-file-name "20260102-010101--proj--done.md" dir))
         (f-wait (expand-file-name "20260103-010101--proj--wait.md" dir))
         (buf-run nil)
         (buf-done nil)
         (buf-wait nil))
    (unwind-protect
        (progn
          (dolist (spec `((,f-run "Running session")
                          (,f-done "Done session")
                          (,f-wait "Waiting session")))
            (agent-pane-test--write
             (car spec)
             (string-join
              `(,"# agent-pane transcript"
                ""
                "- created: 2026-01-01 01:01:01"
                ,(format "- title: %s" (cadr spec))
                "- project_root: /tmp/proj"
                ""
                "---")
              "\n")))
          (setq buf-run (get-buffer-create "*agent-pane status-run*"))
          (with-current-buffer buf-run
            (setq default-directory "/tmp/proj/")
            (agent-pane-mode)
            (map-put! agent-pane--state :transcript-file f-run)
            (map-put! agent-pane--state :in-progress 'streaming))
          (setq buf-done (get-buffer-create "*agent-pane status-done*"))
          (with-current-buffer buf-done
            (setq default-directory "/tmp/proj/")
            (agent-pane-mode)
            (map-put! agent-pane--state :transcript-file f-done)
            (agent-pane--append-message* :role 'assistant :text "done"))
          (setq buf-wait (get-buffer-create "*agent-pane status-wait*"))
          (with-current-buffer buf-wait
            (setq default-directory "/tmp/proj/")
            (agent-pane-mode)
            (map-put! agent-pane--state :transcript-file f-wait))
          (with-temp-buffer
            (agent-pane-sessions-mode)
            (let ((s (buffer-string)))
              (should (string-match-p "^\\*\\* \\[RUN\\] Running session" s))
              (should (string-match-p "^\\*\\* \\[DONE\\] Done session" s))
              (should (string-match-p "^\\*\\* \\[WAIT\\] Waiting session" s)))))
      (dolist (b (list buf-run buf-done buf-wait))
        (when (buffer-live-p b)
          (kill-buffer b)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-filter-shows-matching-session ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (f1 (expand-file-name "20260101-010101--proj--aaaa1111.md" dir))
         (f2 (expand-file-name "20260102-010101--proj--aaaa1111.md" dir)))
    (unwind-protect
        (progn
          (agent-pane-test--write
           f1
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-01 01:01:01"
              "- title: Alpha feature"
              "- project_root: /tmp/proj"
              ""
              "---")
            "\n"))
          (agent-pane-test--write
           f2
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-02 01:01:01"
              "- title: Beta feature"
              "- project_root: /tmp/proj"
              ""
              "---")
            "\n"))
          (with-temp-buffer
            (agent-pane-sessions-mode)
            (agent-pane-sessions-set-filter "beta")
            (let ((s (buffer-string)))
              (should (string-match-p "Beta feature" s))
              (should-not (string-match-p "Alpha feature" s)))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-toggle-sort-switches-to-title-order ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (f1 (expand-file-name "20260102-010101--proj--aaaa1111.md" dir))
         (f2 (expand-file-name "20260101-010101--proj--aaaa1111.md" dir)))
    (unwind-protect
        (progn
          ;; Newer session has title "Zulu", older has "Alpha".
          (agent-pane-test--write
           f1
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-02 01:01:01"
              "- title: Zulu"
              "- project_root: /tmp/proj"
              ""
              "---")
            "\n"))
          (agent-pane-test--write
           f2
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-01 01:01:01"
              "- title: Alpha"
              "- project_root: /tmp/proj"
              ""
              "---")
            "\n"))
          (with-temp-buffer
            (agent-pane-sessions-mode)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* \\(.+\\)$" nil t)
            (should (string-match-p "Zulu" (match-string 1)))
            (agent-pane-sessions-toggle-sort)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* \\(.+\\)$" nil t)
            (should (string-match-p "Alpha" (match-string 1)))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-rename-title-updates-transcript-header ()
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
              "- title: Old title"
              "- project_root: /tmp/proj"
              ""
              "---")
            "\n"))
          (with-temp-buffer
            (agent-pane-sessions-mode)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* " nil t)
            (beginning-of-line)
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _args) "New title")))
              (agent-pane-sessions-rename-title-at-point)))
          (let ((meta (agent-pane-sessions--parse-transcript-metadata f)))
            (should (equal (plist-get meta :title) "New title"))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-delete-at-point-removes-transcript ()
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
              "- title: Delete me"
              "- project_root: /tmp/proj"
              ""
              "---")
            "\n"))
          (with-temp-buffer
            (agent-pane-sessions-mode)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* " nil t)
            (beginning-of-line)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _args) t)))
              (agent-pane-sessions-delete-at-point)))
          (should-not (file-exists-p f)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-delete-keybinding-present ()
  (should (eq (lookup-key agent-pane-sessions-mode-map (kbd "C-k"))
              #'agent-pane-sessions-delete-at-point)))

(provide 'agent-pane-sessions-tests)
;;; agent-pane-sessions-tests.el ends here
