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

(defun agent-pane-test--face-has-p (face target)
  "Return non-nil when FACE includes TARGET."
  (if (listp face)
      (memq target face)
    (eq face target)))

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

(ert-deftest agent-pane-sessions-renders-active-session-with-highlight-face ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (f-active (expand-file-name "20260102-010101--proj--active.md" dir))
         (f-idle (expand-file-name "20260101-010101--proj--idle.md" dir))
         (sessions-buf (get-buffer-create agent-pane-sessions-buffer-name))
         (chat-buf (get-buffer-create "*agent-pane active-session*")))
    (unwind-protect
        (progn
          (dolist (spec `((,f-active "Active session" "2026-01-02 01:01:01")
                          (,f-idle "Idle session" "2026-01-01 01:01:01")))
            (agent-pane-test--write
             (nth 0 spec)
             (string-join
              `(,"# agent-pane transcript"
                ""
                ,(format "- created: %s" (nth 2 spec))
                ,(format "- title: %s" (nth 1 spec))
                "- project_root: /tmp/proj"
                ""
                "---")
              "\n")))
          (save-window-excursion
            (delete-other-windows)
            (let* ((main (selected-window))
                   (side (split-window main agent-pane-sessions-sidebar-width 'left)))
              (set-window-buffer side sessions-buf)
              (set-window-dedicated-p side t)
              (set-window-buffer main chat-buf)
              (with-current-buffer chat-buf
                (setq default-directory "/tmp/proj/")
                (agent-pane-mode)
                (map-put! agent-pane--state :transcript-file f-active))
              (select-window side)
              (with-current-buffer sessions-buf
                (agent-pane-sessions-mode)
                (agent-pane-sessions-refresh)
                (should (agent-pane-sessions--goto-prop-value
                         'agent-pane-session-file f-active))
                (should (eq (get-text-property (point) 'agent-pane-session-active) t))
                (should (agent-pane-test--face-has-p
                         (get-text-property (line-beginning-position) 'face)
                         'agent-pane-sessions-active-session))
                (should (agent-pane-sessions--goto-prop-value
                         'agent-pane-session-file f-idle))
                (should-not (get-text-property (point) 'agent-pane-session-active))
                (should-not (agent-pane-test--face-has-p
                             (get-text-property (line-beginning-position) 'face)
                             'agent-pane-sessions-active-session))))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
      (when (buffer-live-p sessions-buf)
        (kill-buffer sessions-buf))
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

(ert-deftest agent-pane-sessions-open-at-point-estimates-usage-from-replayed-messages ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (f (expand-file-name "20260101-010101--proj--aaaa1111.md" dir))
         (agent-pane-buffer-name "*agent-pane replay usage test*")
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
              "- acp_session_id: sid-usage-123"
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
            (let ((agent-pane-estimate-context-usage t))
              (should-not (map-elt agent-pane--state :usage-metrics))
              (let ((summary (agent-pane--ui-usage-summary)))
                (should (stringp summary))
                (should (string-match-p "ctx~=" summary))
                (should (string-match-p "(~" summary))))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-open-at-point-positions-new-chat-at-input ()
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
              ""
              "---"
              ""
              "## User — 2026-01-01 01:01:02"
              ""
              "hello")
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
            (should (markerp agent-pane--input-marker))
            (should (= (point) (marker-position agent-pane--input-marker)))))
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

(ert-deftest agent-pane-sessions-open-at-point-preserves-point-for-existing-buffer ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (f (expand-file-name "20260101-010101--proj--aaaa1111.md" dir))
         (agent-pane-buffer-name "*agent-pane replay test*")
         (opened nil)
         (chat-buf nil)
         (saved-point nil))
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
              "world")
            "\n"))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf &rest _args)
                       (push buf opened)
                       buf)))
            (with-temp-buffer
              (insert "** session\n")
              (add-text-properties (point-min) (point-max)
                                   (list 'agent-pane-session-file f
                                         'agent-pane-project-root "/tmp/proj"))
              (goto-char (point-min))
              (agent-pane-sessions-open-at-point))
            (setq chat-buf (car opened))
            (with-current-buffer chat-buf
              (goto-char (point-min))
              (setq saved-point (point)))
            (with-temp-buffer
              (insert "** session\n")
              (add-text-properties (point-min) (point-max)
                                   (list 'agent-pane-session-file f
                                         'agent-pane-project-root "/tmp/proj"))
              (goto-char (point-min))
              (agent-pane-sessions-open-at-point)))
          (should (= (length opened) 2))
          (should (eq (nth 0 opened) (nth 1 opened)))
          (should (buffer-live-p chat-buf))
          (with-current-buffer chat-buf
            (should (= (point) saved-point))
            (should (equal (map-elt agent-pane--state :transcript-file) f))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
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
         (agent-pane-sessions-status-style 'text)
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

(ert-deftest agent-pane-sessions-done-status-clears-after-thread-is-seen ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (agent-pane-sessions-status-style 'text)
         (f (expand-file-name "20260101-010101--proj--done.md" dir))
         (chat-buf nil))
    (unwind-protect
        (progn
          (agent-pane-test--write
           f
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-01 01:01:01"
              "- title: Done clears"
              "- project_root: /tmp/proj"
              ""
              "---")
            "\n"))
          (setq chat-buf (get-buffer-create "*agent-pane status-done-clear*"))
          (with-current-buffer chat-buf
            (setq default-directory "/tmp/proj/")
            (agent-pane-mode)
            (map-put! agent-pane--state :transcript-file f)
            (agent-pane--append-message* :role 'assistant :text "done"))
          (let ((session (agent-pane-sessions--parse-transcript-metadata f)))
            (should (eq (agent-pane-sessions--session-status session) 'done))
            (with-current-buffer chat-buf
              (agent-pane--mark-thread-seen))
            (should (eq (agent-pane-sessions--session-status session) 'waiting-input))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-status-indicators-support-icon-style ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (agent-pane-sessions-status-style 'icons)
         (agent-pane-sessions-status-animate-running nil)
         (f-done (expand-file-name "20260101-010101--proj--done.md" dir))
         (chat-buf nil))
    (unwind-protect
        (progn
          (agent-pane-test--write
           f-done
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-01 01:01:01"
              "- title: Icon done"
              "- project_root: /tmp/proj"
              ""
              "---")
            "\n"))
          (setq chat-buf (get-buffer-create "*agent-pane status-icon*"))
          (with-current-buffer chat-buf
            (setq default-directory "/tmp/proj/")
            (agent-pane-mode)
            (map-put! agent-pane--state :transcript-file f-done)
            (agent-pane--append-message* :role 'assistant :text "done"))
          (with-temp-buffer
            (agent-pane-sessions-mode)
            (should (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "^\\*\\* ✓ Icon done" nil t)))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-running-status-uses-static-icon ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (agent-pane-sessions-status-style 'icons)
         (agent-pane-sessions-status-animate-running nil)
         (agent-pane-sessions-running-icon-frames ["•"])
         (f-run (expand-file-name "20260101-010101--proj--run.md" dir))
         (chat-buf nil))
    (unwind-protect
        (progn
          (agent-pane-test--write
           f-run
           (string-join
            '("# agent-pane transcript"
              ""
              "- created: 2026-01-01 01:01:01"
              "- title: Icon running"
              "- project_root: /tmp/proj"
              ""
              "---")
            "\n"))
          (setq chat-buf (get-buffer-create "*agent-pane status-running-icon*"))
          (with-current-buffer chat-buf
            (setq default-directory "/tmp/proj/")
            (agent-pane-mode)
            (map-put! agent-pane--state :transcript-file f-run)
            (map-put! agent-pane--state :in-progress 'streaming))
          (with-temp-buffer
            (agent-pane-sessions-mode)
            (should (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "^\\*\\* • Icon running" nil t)))
            (should-not (timerp agent-pane-sessions--status-animation-timer))))
      (agent-pane-sessions--stop-status-animation)
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
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

(ert-deftest agent-pane-sessions-delete-at-point-kills-open-chat-buffer ()
  (let* ((dir (make-temp-file "agent-pane-transcripts" t))
         (agent-pane-transcript-directory dir)
         (f (expand-file-name "20260101-010101--proj--aaaa1111.md" dir))
         (chat-buf nil))
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
          (setq chat-buf (generate-new-buffer "*agent-pane delete-open*"))
          (with-current-buffer chat-buf
            (setq default-directory "/tmp/proj/")
            (agent-pane-mode)
            (map-put! agent-pane--state :transcript-file f))
          (with-temp-buffer
            (agent-pane-sessions-mode)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* " nil t)
            (beginning-of-line)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _args) t)))
              (agent-pane-sessions-delete-at-point)))
          (should-not (file-exists-p f))
          (should-not (buffer-live-p chat-buf)))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest agent-pane-sessions-new-chat-prompts-for-project-when-no-project-at-point ()
  (let* ((proj (make-temp-file "agent-pane-project" t))
         (agent-pane-buffer-name "*agent-pane project picker*")
         (opened nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _args) proj))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (buf &rest _args)
                         (push buf opened)
                         buf)))
              (agent-pane-sessions-new-chat)))
          (let ((chat-buf (car opened)))
            (should (buffer-live-p chat-buf))
            (with-current-buffer chat-buf
              (should (derived-mode-p 'agent-pane-mode))
              (should (equal default-directory
                             (file-name-as-directory (expand-file-name proj)))))))
      (dolist (buf opened)
        (when (buffer-live-p buf)
          (kill-buffer buf)))
      (ignore-errors (delete-directory proj t)))))

(ert-deftest agent-pane-sessions-delete-keybinding-present ()
  (should (eq (lookup-key agent-pane-sessions-mode-map (kbd "C-k"))
              #'agent-pane-sessions-delete-at-point))
  (should (eq (lookup-key agent-pane-sessions-mode-map (kbd "a"))
              #'agent-pane-sessions-add-project))
  (should (eq (lookup-key agent-pane-sessions-mode-map (kbd "C-c v"))
              #'agent-pane-toggle-header-details-anywhere))
  (should (eq (lookup-key agent-pane-sessions-mode-map (kbd "C-c C-v"))
              #'agent-pane-toggle-header-details-anywhere))
  (should (eq (lookup-key agent-pane-sessions-mode-map (kbd "C-c d"))
              #'agent-pane-view-diff-at-point-anywhere))
  (should (eq (lookup-key agent-pane-sessions-mode-map (kbd "C-c C-d"))
              #'agent-pane-view-diff-at-point-anywhere)))

(provide 'agent-pane-sessions-tests)
;;; agent-pane-sessions-tests.el ends here
