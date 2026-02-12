;;; agent-pane-transcript.el --- Transcript persistence for agent-pane -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;;; Commentary:
;;
;; Transcript files are plain Markdown with a small metadata header.  This module
;; is responsible for creating and appending to those files.
;;
;;; Code:
(require 'agent-pane-config)
(require 'agent-pane-state)
(require 'agent-pane-util)
(require 'map)
(require 'project)
(require 'subr-x)
(declare-function agent-pane-sessions-refresh-if-visible "agent-pane-sessions")
(declare-function agent-pane--acp-command-and-params "agent-pane-util")
(defvar agent-pane-acp-provider)
(defvar agent-pane-session-model-id)
(defun agent-pane--transcripts-enabled-p ()
  "Return non-nil if transcript saving is enabled."
  (pcase agent-pane-save-transcripts
    ('always t)
    ('interactive (not noninteractive))
    (_ nil)))
(defun agent-pane--project-root ()
  "Return the current project root directory, or `default-directory'."
  (or (when-let ((proj (project-current nil)))
        (project-root proj))
      default-directory))
(defun agent-pane--transcript-default-file ()
  "Compute a new transcript file path for the current buffer."
  (let* ((root (directory-file-name (expand-file-name (agent-pane--project-root))))
         (proj-name (file-name-nondirectory root))
         (hash (substring (md5 root) 0 8))
         (ts (format-time-string "%Y%m%d-%H%M%S"))
         (file (format "%s--%s--%s.md" ts (agent-pane--slugify proj-name) hash)))
    (expand-file-name file agent-pane-transcript-directory)))
(defun agent-pane--transcript-ensure-file ()
  "Ensure a transcript file exists for the current agent-pane buffer.
Return the transcript file path, or nil if transcript saving is disabled."
  (when (agent-pane--transcripts-enabled-p)
    (agent-pane--ensure-state)
    (unless (map-elt agent-pane--state :transcript-file)
      (let* ((dir (file-name-as-directory (expand-file-name agent-pane-transcript-directory)))
             (file (agent-pane--transcript-default-file))
             (root (directory-file-name (expand-file-name (agent-pane--project-root))))
             (cmd+params (agent-pane--acp-command-and-params))
             (cmd (car cmd+params))
             (params (cdr cmd+params))
             (header
              (string-join
               (list
                "# agent-pane transcript"
                ""
                (format "- created: %s" (format-time-string "%F %T"))
                (format "- project_root: %s" root)
                (format "- buffer: %s" (buffer-name))
                (format "- acp_provider: %s" (or agent-pane-acp-provider 'codex))
                (format "- auth_method: %s" (or agent-pane-auth-method-id "-"))
                (format "- session_mode: %s" (or agent-pane-session-mode-id "-"))
                (format "- session_model: %s" (or agent-pane-session-model-id "-"))
                (format "- acp_command: %s %s" cmd (string-join (or params nil) " "))
                ""
                "---"
                "")
               "\n")))
        (make-directory dir t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region header nil file nil 'silent))
        (map-put! agent-pane--state :transcript-file file)
        ;; If session id is already known, log it now.
        (agent-pane--transcript-log-session-id)
        ;; Refresh sidebar if it's open.
        (when (fboundp 'agent-pane-sessions-refresh-if-visible)
          (agent-pane-sessions-refresh-if-visible))))
    (map-elt agent-pane--state :transcript-file)))
(defun agent-pane--transcript-append (text)
  "Append TEXT to the current transcript file."
  (when-let ((file (agent-pane--transcript-ensure-file)))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region text nil file 'append 'silent))))
(defun agent-pane--transcript-log-session-id ()
  "Log ACP session id to transcript once, if available.
This does not create a transcript file by itself; callers should ensure the
transcript exists via `agent-pane--transcript-ensure-file' (directly or
indirectly via `agent-pane--transcript-append')."
  (when (and (agent-pane--transcripts-enabled-p)
             (map-elt agent-pane--state :transcript-file)
             (map-elt agent-pane--state :session-id)
             (not (map-elt agent-pane--state :transcript-session-id-logged)))
    (map-put! agent-pane--state :transcript-session-id-logged t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region
       (format "\n- acp_session_id: %s\n\n" (map-elt agent-pane--state :session-id))
       nil
       (map-elt agent-pane--state :transcript-file)
       'append
       'silent))))
(defun agent-pane--transcript-maybe-set-title (text)
  "Persist a transcript title derived from TEXT.
This writes (or updates) a `- title: ...` line in the transcript header.
It is only done once per transcript."
  (when (and (agent-pane--transcripts-enabled-p)
             (not (map-elt agent-pane--state :transcript-title-logged)))
    (when-let ((file (agent-pane--transcript-ensure-file)))
      (let ((title (agent-pane--session-title-from-text text)))
        (map-put! agent-pane--state :session-title title)
        (map-put! agent-pane--state :transcript-title-logged t)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((case-fold-search nil))
            (cond
             ((re-search-forward (rx bol "- title:" (* blank) (group (* nonl)) eol) nil t)
              ;; Only replace if empty/placeholder.
              (let ((existing (string-trim (match-string 1))))
                (when (or (string-empty-p existing)
                          (member existing '("-" "(none)")))
                  (replace-match (concat "- title: " title) t t nil 0))))
             ((re-search-forward (rx bol "- created:" (* nonl) eol) nil t)
              (forward-line 1)
              (insert (format "- title: %s\n" title)))
             (t
              ;; Fallback: insert after the top-level heading.
              (when (re-search-forward (rx bol "# " (* nonl) eol) nil t)
                (forward-line 2)
                (insert (format "- title: %s\n" title))))))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (point-min) (point-max) file nil 'silent))
          (when (fboundp 'agent-pane-sessions-refresh-if-visible)
            (agent-pane-sessions-refresh-if-visible)))))))
(defun agent-pane--transcript-role-heading (role &optional title)
  "Append a markdown heading for ROLE to the transcript.
TITLE, when non-nil, is included in parentheses."
  (let* ((ts (format-time-string "%F %T"))
         (label (pcase role
                  ('user "User")
                  ('assistant "Agent")
                  ('thought "Thought")
                  ('tool "Tool")
                  ('system "System")
                  (_ (format "%s" role))))
         (suffix (if (and title (not (string-empty-p title)))
                     (format " (%s)" title)
                   "")))
    (agent-pane--transcript-append (format "## %s%s — %s\n\n" label suffix ts))))
(provide 'agent-pane-transcript)
;;; agent-pane-transcript.el ends here
