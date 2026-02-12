;;; agent-pane-sessions.el --- Sidebar for agent-pane transcripts -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Provides a collapsible sidebar listing saved agent-pane transcript sessions
;; grouped by project.
;;
;;; Code:

(require 'agent-pane-config)

(require 'cl-lib)
(require 'map)
(require 'outline)
(require 'subr-x)

;; Avoid compile-time warnings; these are defined in agent-pane.el.
(declare-function agent-pane-mode "agent-pane")
(declare-function agent-pane-exit "agent-pane")
(declare-function agent-pane-toggle-header-details-anywhere "agent-pane")
(declare-function agent-pane-view-diff-at-point-anywhere "agent-pane")
(declare-function agent-pane--goto-input "agent-pane")
(declare-function agent-pane--make-state "agent-pane-state")
(declare-function agent-pane--rerender "agent-pane-ui")
(defvar agent-pane-buffer-name)
(defvar agent-pane--state)

(defgroup agent-pane-sessions nil
  "Session sidebar for agent-pane transcripts."
  :group 'agent-pane)

(defcustom agent-pane-sessions-buffer-name "*agent-pane sessions*"
  "Name of the agent-pane sessions sidebar buffer."
  :type 'string
  :group 'agent-pane-sessions)

(defcustom agent-pane-sessions-sidebar-width 40
  "Default width of the sessions sidebar window."
  :type 'integer
  :group 'agent-pane-sessions)

(defcustom agent-pane-sessions-project-collapsed-by-default nil
  "When non-nil, projects in the sessions sidebar start collapsed."
  :type 'boolean
  :group 'agent-pane-sessions)

(defcustom agent-pane-sessions-default-sort-mode 'recency
  "Default sort mode for sessions list."
  :type '(choice (const :tag "Recency" recency)
                 (const :tag "Title" title))
  :group 'agent-pane-sessions)

(defcustom agent-pane-sessions-show-preview t
  "When non-nil, include a preview snippet on each session line."
  :type 'boolean
  :group 'agent-pane-sessions)

(defcustom agent-pane-sessions-preview-max-width 48
  "Maximum width of the preview snippet shown per session."
  :type 'integer
  :group 'agent-pane-sessions)

(defcustom agent-pane-sessions-status-style 'icons
  "How live session status is shown in the sidebar."
  :type '(choice (const :tag "Icons" icons)
                 (const :tag "Text prefixes" text))
  :group 'agent-pane-sessions)

(defcustom agent-pane-sessions-status-animate-running nil
  "When non-nil, animate the running status indicator in the sidebar."
  :type 'boolean
  :group 'agent-pane-sessions)

(defcustom agent-pane-sessions-status-animation-interval 0.4
  "Seconds between running-status animation frames in sidebar."
  :type 'number
  :group 'agent-pane-sessions)

(defcustom agent-pane-sessions-running-icon-frames ["•"]
  "Glyphs used for running status indicator.
When animation is enabled, the icon cycles across frames."
  :type '(vector string)
  :group 'agent-pane-sessions)

(defvar agent-pane-transcript-directory)

(defvar-local agent-pane-sessions--index nil
  "Cached sidebar index.

This is a list of (PROJECT-ROOT . SESSIONS).  SESSIONS is a list of plists.")

(defvar-local agent-pane-sessions--filter-query nil
  "Current text filter query for the sidebar.")

(defvar-local agent-pane-sessions--sort-mode nil
  "Current sort mode symbol for the sidebar.")

(defvar-local agent-pane-sessions--running-frame-index 0
  "Current running-status animation frame index.")

(defvar agent-pane-sessions--status-animation-timer nil
  "Timer driving running-status animation in the sessions sidebar.")

(defun agent-pane-sessions--transcript-files ()
  "Return a list of transcript files in `agent-pane-transcript-directory'."
  (let ((dir (and (boundp 'agent-pane-transcript-directory)
                  agent-pane-transcript-directory)))
    (when (and dir (file-directory-p dir))
      (directory-files dir t "\\.md\\'"))))

(defun agent-pane-sessions--parse-created-from-filename (file)
  "Try to parse a timestamp from FILE name.

Returns an Emacs time value or nil."
  (when (string-match (rx string-start
                          (group (= 8 digit))
                          "-"
                          (group (= 6 digit))
                          "--")
                      (file-name-nondirectory file))
    (let* ((ymd (match-string 1 (file-name-nondirectory file)))
           (hms (match-string 2 (file-name-nondirectory file)))
           (s (format "%s-%s-%s %s:%s:%s"
                      (substring ymd 0 4)
                      (substring ymd 4 6)
                      (substring ymd 6 8)
                      (substring hms 0 2)
                      (substring hms 2 4)
                      (substring hms 4 6))))
      (ignore-errors (date-to-time s)))))

(defun agent-pane-sessions--first-heading-body-line (heading-substr)
  "Return first non-empty body line after heading containing HEADING-SUBSTR.

Search starts at current point and returns nil if no suitable section exists."
  (let ((case-fold-search t)
        (found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward (rx bol "## " (group (*? any)) " — " (* nonl) eol)
                                     nil t))
        (when (string-match-p (regexp-quote heading-substr) (or (match-string 1) ""))
          (forward-line 1)
          (while (and (not (eobp))
                      (string-empty-p (string-trim (or (thing-at-point 'line t) ""))))
            (forward-line 1))
          (setq found (string-trim (or (thing-at-point 'line t) ""))))))
    found))

(defun agent-pane-sessions--parse-transcript-metadata (file)
  "Parse FILE header and return a plist.

Keys:
- :file
- :project-root
- :created (time value)
- :created-string
- :session-id
- :title (first user line when available)
- :preview (short assistant/user snippet when available)"
  (with-temp-buffer
    (insert-file-contents file nil 0 8192)
    (goto-char (point-min))
    (let ((project-root nil)
          (created-string nil)
          (created nil)
          (session-id nil)
          (title nil)
          (preview nil))
      (while (re-search-forward (rx bol "- " (group (+ (not (any "\n")))) ": " (group (* nonl)) eol)
                                nil t)
        (pcase (match-string 1)
          ("project_root"
           (setq project-root
                 (directory-file-name (expand-file-name (match-string 2)))))
          ("created" (setq created-string (match-string 2)))
          ("acp_session_id" (setq session-id (match-string 2)))
          ("title" (setq title (string-trim (match-string 2))))
          (_ nil)))

      (setq created
            (or (and created-string (ignore-errors (date-to-time created-string)))
                (agent-pane-sessions--parse-created-from-filename file)
                (ignore-errors (file-attribute-modification-time (file-attributes file)))))

      (when (or (null title) (string-empty-p title))
        (setq title (or (agent-pane-sessions--first-heading-body-line "User")
                        (agent-pane-sessions--first-heading-body-line "You"))))

      (setq preview
            (or (agent-pane-sessions--first-heading-body-line "Agent")
                (agent-pane-sessions--first-heading-body-line "Assistant")
                title
                ""))

      (list :file file
            :project-root (or project-root "")
            :created created
            :created-string created-string
            :session-id session-id
            :title title
            :preview preview))))

(defun agent-pane-sessions--role-from-heading-label (label)
  "Map transcript heading LABEL to an internal message role symbol."
  (pcase (downcase (string-trim (or label "")))
    ((or "you" "user") 'user)
    ((or "agent" "assistant") 'assistant)
    ("thought" 'thought)
    ("tool" 'tool)
    ("system" 'system)
    ("acp" 'acp)
    (_ 'system)))

(defun agent-pane-sessions--parse-transcript-messages (file)
  "Parse transcript FILE and return a list of message plists.

Each entry includes `:role', `:title', `:text', and `:raw' keys, matching
`agent-pane--state' message shape."
  (let ((heading-rx (rx bol
                        "## "
                        (group (+ (not (any "\n(—"))))
                        (optional " (" (group (*? any)) ")")
                        " — "
                        (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)
                        " "
                        (= 2 digit) ":" (= 2 digit) ":" (= 2 digit)
                        eol))
        messages)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward (rx bol "---" eol) nil t)
        (forward-line 1))
      (while (re-search-forward heading-rx nil t)
        (let* ((label (match-string 1))
               (title0 (or (match-string 2) ""))
               (title (string-trim title0))
               (role (agent-pane-sessions--role-from-heading-label label))
               (body-start (progn
                             (forward-line 1)
                             (when (looking-at-p (rx bol (* blank) eol))
                               (forward-line 1))
                             (point)))
               (body-end (if (re-search-forward heading-rx nil t)
                             (prog1 (match-beginning 0)
                               (goto-char (match-beginning 0)))
                           (point-max)))
               (text (string-trim-right
                      (buffer-substring-no-properties body-start body-end))))
          (push (list (cons :role role)
                      (cons :text text)
                      (cons :title (unless (string-empty-p title) title))
                      (cons :raw nil))
                messages))))
    (nreverse messages)))

(defun agent-pane-sessions--project-name (project-root)
  "Return a human-friendly project name for PROJECT-ROOT."
  (if (and project-root (not (string-empty-p project-root)))
      (file-name-nondirectory (directory-file-name (expand-file-name project-root)))
    "(unknown project)"))

(defun agent-pane-sessions--scan ()
  "Scan transcript directory and return grouped sessions.

Return value is a list of (PROJECT-ROOT . SESSIONS) pairs."
  (let* ((files (agent-pane-sessions--transcript-files))
         (by-root (make-hash-table :test 'equal)))
    (dolist (f files)
      (let* ((meta (agent-pane-sessions--parse-transcript-metadata f))
             (root (plist-get meta :project-root)))
        (puthash root (cons meta (gethash root by-root)) by-root)))
    (let (result)
      (maphash
       (lambda (root sessions)
         (push (cons root sessions) result))
       by-root)
      (sort result
            (lambda (a b)
              (string-lessp (agent-pane-sessions--project-name (car a))
                            (agent-pane-sessions--project-name (car b))))))))

(defun agent-pane-sessions--session-search-text (session)
  "Build searchable text blob for SESSION plist."
  (downcase
   (string-join
    (delq nil
          (list (plist-get session :title)
                (plist-get session :preview)
                (plist-get session :session-id)
                (plist-get session :file)
                (plist-get session :project-root)))
    " ")))

(defun agent-pane-sessions--session-matches-filter-p (session)
  "Return non-nil when SESSION matches current text filter."
  (let ((query (string-trim (or agent-pane-sessions--filter-query ""))))
    (if (string-empty-p query)
        t
      (let* ((haystack (agent-pane-sessions--session-search-text session))
             (parts (split-string (downcase query) "[[:space:]]+" t)))
        (cl-every (lambda (part) (string-match-p (regexp-quote part) haystack))
                  parts)))))

(defun agent-pane-sessions--session-title-sort-key (session)
  "Return normalized title sort key for SESSION."
  (let ((title (string-trim (or (plist-get session :title) ""))))
    (if (string-empty-p title)
        (file-name-nondirectory (or (plist-get session :file) ""))
      title)))

(defun agent-pane-sessions--sort-sessions (sessions)
  "Return SESSIONS sorted according to `agent-pane-sessions--sort-mode'."
  (let ((xs (copy-sequence sessions)))
    (pcase agent-pane-sessions--sort-mode
      ('title
       (sort xs (lambda (a b)
                  (string-lessp (downcase (agent-pane-sessions--session-title-sort-key a))
                                (downcase (agent-pane-sessions--session-title-sort-key b))))))
      (_
       (sort xs (lambda (a b)
                  (time-less-p (or (plist-get b :created) 0)
                               (or (plist-get a :created) 0))))))))

(defun agent-pane-sessions--visible-groups ()
  "Return grouped sessions after applying current filter and sort mode."
  (let (result)
    (dolist (group agent-pane-sessions--index)
      (let* ((root (car group))
             (sessions (cdr group))
             (filtered (cl-remove-if-not #'agent-pane-sessions--session-matches-filter-p sessions))
             (sorted (agent-pane-sessions--sort-sessions filtered)))
        (when sorted
          (push (cons root sorted) result))))
    (nreverse result)))

(defun agent-pane-sessions--normalize-file (file)
  "Return canonical absolute path for FILE, or nil."
  (when (and (stringp file)
             (not (string-empty-p file)))
    (ignore-errors
      (file-truename (expand-file-name file)))))

(defun agent-pane-sessions--chat-buffer-transcript-file (buffer)
  "Return transcript file associated with chat BUFFER, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-pane-mode)
        (let* ((state (and (boundp 'agent-pane--state) agent-pane--state))
               (file (and (listp state) (map-elt state :transcript-file))))
          (agent-pane-sessions--normalize-file file))))))

(defun agent-pane-sessions--active-transcript-file ()
  "Return transcript file for the active chat tied to this sidebar."
  (when-let ((sessions-win (get-buffer-window (current-buffer) t)))
    (let* ((sessions-frame (window-frame sessions-win))
           (selected-win (selected-window))
           (selected-chat-win
            (and (window-live-p selected-win)
                 (eq (window-frame selected-win) sessions-frame)
                 (with-current-buffer (window-buffer selected-win)
                   (derived-mode-p 'agent-pane-mode))
                 selected-win))
           (chat-win (or selected-chat-win
                         (agent-pane-sessions--chat-target-window))))
      (when (window-live-p chat-win)
        (agent-pane-sessions--chat-buffer-transcript-file
         (window-buffer chat-win))))))

(defun agent-pane-sessions--chat-buffer-for-transcript (file)
  "Return live chat buffer associated with transcript FILE, or nil."
  (let ((target (agent-pane-sessions--normalize-file file)))
    (cl-loop for buf in (buffer-list)
             when (buffer-live-p buf)
             thereis
             (with-current-buffer buf
               (when (derived-mode-p 'agent-pane-mode)
                 (let ((tf (agent-pane-sessions--chat-buffer-transcript-file buf)))
                   (when (and tf target
                              (equal tf target))
                     buf)))))))

(defun agent-pane-sessions--close-buffer-and-windows (buffer)
  "Close windows showing BUFFER, then kill BUFFER."
  (when (buffer-live-p buffer)
    (dolist (win (get-buffer-window-list buffer nil t))
      (when (window-live-p win)
        (if (one-window-p t win)
            (with-selected-window win
              (switch-to-buffer (other-buffer buffer t)))
          (delete-window win))))
    (kill-buffer buffer)))

(defun agent-pane-sessions--close-chat-buffers-for-transcript (file)
  "Close all live chat buffers associated with transcript FILE."
  (when file
    (let ((buf (agent-pane-sessions--chat-buffer-for-transcript file)))
      (while (buffer-live-p buf)
        (agent-pane-sessions--close-buffer-and-windows buf)
        (setq buf (agent-pane-sessions--chat-buffer-for-transcript file))))))

(defun agent-pane-sessions--state-has-unseen-agent-output-p (state)
  "Return non-nil when STATE has unseen non-user output for the user."
  (let* ((msgs (and (listp state) (map-elt state :messages)))
         (seen (if (listp state)
                   (or (map-elt state :session-seen-message-count) 0)
                 0))
         (start (max 0 (min seen (length msgs)))))
    (cl-some (lambda (m)
               (memq (map-elt m :role)
                     '(assistant thought tool system acp)))
             (nthcdr start msgs))))

(defun agent-pane-sessions--session-status (session)
  "Return live status symbol for SESSION.

Possible values:
- `running'       active ACP work in progress
- `done'          idle and has produced agent output
- `waiting-input' idle and waiting for user input
- nil             no live chat buffer for this transcript"
  (let* ((file (plist-get session :file))
         (buf (and file (file-exists-p file)
                   (agent-pane-sessions--chat-buffer-for-transcript file))))
    (and buf
         (with-current-buffer buf
           (let* ((state (and (boundp 'agent-pane--state) agent-pane--state))
                  (in-progress (and (listp state) (map-elt state :in-progress)))
                  (running (and (listp state)
                                (or (map-elt state :connecting)
                                    (map-elt state :prompt-in-flight)
                                    (memq in-progress '(waiting streaming cancelling))
                                    (consp (map-elt state :prompt-queue))))))
             (cond
              (running 'running)
              ((agent-pane-sessions--state-has-unseen-agent-output-p state) 'done)
              (t 'waiting-input)))))))

(defun agent-pane-sessions--fixed-width-status-icon (icon)
  "Return ICON normalized to exactly one display column."
  (let ((s (or icon " ")))
    (cond
     ((= (string-width s) 1) s)
     ((< (string-width s) 1) " ")
     (t (truncate-string-to-width s 1 nil nil "")))))

(defun agent-pane-sessions--running-icon ()
  "Return current running-status icon frame."
  (let* ((frames (if (vectorp agent-pane-sessions-running-icon-frames)
                     agent-pane-sessions-running-icon-frames
                   ["•"]))
         (len (max 1 (length frames)))
         (idx (mod agent-pane-sessions--running-frame-index len)))
    (agent-pane-sessions--fixed-width-status-icon
     (or (aref frames idx) "•"))))

(defun agent-pane-sessions--session-status-indicator (session)
  "Return display indicator describing SESSION live status."
  (let ((status (agent-pane-sessions--session-status session)))
    (pcase agent-pane-sessions-status-style
      ('text
       (pcase status
         ('running "[RUN] ")
         ('done "[DONE] ")
         ('waiting-input "[WAIT] ")
         (_ "       ")))
      (_
       (pcase status
         ('running (format "%s " (agent-pane-sessions--running-icon)))
         ('done "✓ ")
         ('waiting-input "… ")
         (_ "  "))))))

(defun agent-pane-sessions--status-legend ()
  "Return human-readable legend for live status indicators."
  (pcase agent-pane-sessions-status-style
    ('text "[RUN]/[DONE]/[WAIT] live status")
    (_ "icons: running(•), done(✓), waiting(…)")))

(defun agent-pane-sessions--running-visible-p ()
  "Return non-nil when any currently visible session is running."
  (cl-some (lambda (group)
             (cl-some (lambda (s)
                        (eq (agent-pane-sessions--session-status s) 'running))
                      (cdr group)))
           (agent-pane-sessions--visible-groups)))

(defun agent-pane-sessions--stop-status-animation ()
  "Stop sidebar status animation timer, if any."
  (when (timerp agent-pane-sessions--status-animation-timer)
    (cancel-timer agent-pane-sessions--status-animation-timer)
    (setq agent-pane-sessions--status-animation-timer nil)))

(defun agent-pane-sessions--status-animation-tick ()
  "Advance running-status animation and re-render sidebar when needed."
  (if-let ((buf (get-buffer agent-pane-sessions-buffer-name)))
      (if (not (buffer-live-p buf))
          (agent-pane-sessions--stop-status-animation)
        (with-current-buffer buf
          (if (not (derived-mode-p 'agent-pane-sessions-mode))
              (agent-pane-sessions--stop-status-animation)
            (if (and (eq agent-pane-sessions-status-style 'icons)
                     agent-pane-sessions-status-animate-running
                     (agent-pane-sessions--running-visible-p))
                (let* ((frames (if (vectorp agent-pane-sessions-running-icon-frames)
                                   agent-pane-sessions-running-icon-frames
                                 ["•"]))
                       (len (max 1 (length frames))))
                  (setq-local agent-pane-sessions--running-frame-index
                              (mod (1+ agent-pane-sessions--running-frame-index) len))
                  (let ((inhibit-read-only t))
                    (agent-pane-sessions--render)
                    (setq buffer-read-only t)))
              (agent-pane-sessions--stop-status-animation)))))
    (agent-pane-sessions--stop-status-animation)))

(defun agent-pane-sessions--ensure-status-animation ()
  "Start or stop sidebar status animation based on current conditions."
  (if (and (derived-mode-p 'agent-pane-sessions-mode)
           (eq agent-pane-sessions-status-style 'icons)
           agent-pane-sessions-status-animate-running
           (agent-pane-sessions--running-visible-p))
      (unless (timerp agent-pane-sessions--status-animation-timer)
        (setq agent-pane-sessions--status-animation-timer
              (run-with-timer agent-pane-sessions-status-animation-interval
                              agent-pane-sessions-status-animation-interval
                              #'agent-pane-sessions--status-animation-tick)))
    (agent-pane-sessions--stop-status-animation)))

(defun agent-pane-sessions--format-session-line (session)
  "Format a SESSION plist for display line text."
  (let* ((created (plist-get session :created))
         (created-s (if created (format-time-string "%F %R" created) ""))
         (title0 (or (plist-get session :title) ""))
         (title (string-trim (replace-regexp-in-string "[\n\t ]+" " " title0)))
         (preview0 (or (plist-get session :preview) ""))
         (preview (string-trim (replace-regexp-in-string "[\n\t ]+" " " preview0)))
         (title* (if (string-empty-p title)
                     (file-name-base (or (plist-get session :file) ""))
                   title))
         (title* (truncate-string-to-width title* 80 nil nil "…"))
         (preview* (truncate-string-to-width preview
                                             agent-pane-sessions-preview-max-width
                                             nil nil "…")))
    (concat
     (agent-pane-sessions--session-status-indicator session)
     (if (string-empty-p created-s)
         title*
       (format "%s (%s)" title* created-s))
     (if (and agent-pane-sessions-show-preview
              (not (string-empty-p preview*))
              (not (equal preview* title*)))
         (format " — %s" preview*)
       ""))))

(defun agent-pane-sessions--sort-mode-label ()
  "Return a short human label for current sort mode."
  (pcase agent-pane-sessions--sort-mode
    ('title "title")
    (_ "recency")))

(defun agent-pane-sessions--goto-prop-value (prop value)
  "Move point to first position where text property PROP equals VALUE.
Return non-nil when a match is found."
  (let ((pos (point-min))
        found)
    (while (and (not found) (< pos (point-max)))
      (when (equal (get-text-property pos prop) value)
        (setq found pos))
      (setq pos (or (next-single-property-change pos prop nil (point-max))
                    (point-max))))
    (when found
      (goto-char found)
      t)))

(defun agent-pane-sessions--render ()
  "Render the sessions sidebar in the current buffer."
  (let* ((inhibit-read-only t)
         (saved-session-file (get-text-property (point) 'agent-pane-session-file))
         (saved-project-root (get-text-property (point) 'agent-pane-project-root))
         (saved-line (line-number-at-pos))
         (saved-col (current-column))
         (active-session-file (agent-pane-sessions--active-transcript-file))
         (visible-groups (agent-pane-sessions--visible-groups))
         (all-count (cl-loop for (_ . xs) in agent-pane-sessions--index sum (length xs)))
         (visible-count (cl-loop for (_ . xs) in visible-groups sum (length xs)))
         (query (string-trim (or agent-pane-sessions--filter-query ""))))
    (erase-buffer)
    (insert (propertize "agent-pane sessions\n" 'face 'agent-pane-sessions-title))
    (insert (propertize (format "sort=%s  filter=%s  shown=%d/%d\n\n"
                                (agent-pane-sessions--sort-mode-label)
                                (if (string-empty-p query) "(none)" query)
                                visible-count
                                all-count)
                        'face 'agent-pane-sessions-meta))
    (insert (propertize (format "Keys: RET open/replay | o open .md | n new chat | a add project | / filter | c clear | s sort | r rename | C-k delete | TAB fold | g refresh | q exit | %s\n\n"
                                (agent-pane-sessions--status-legend))
                        'face 'agent-pane-sessions-meta))
    (if (null visible-groups)
        (insert (propertize "No sessions match current filter.\n" 'face 'agent-pane-sessions-meta))
      (dolist (group visible-groups)
        (let* ((root (car group))
               (sessions (cdr group))
               (name (agent-pane-sessions--project-name root))
               (count (length sessions))
               (start (point)))
          (insert (propertize (format "* %s (%d)\n" name count)
                              'face 'agent-pane-sessions-project))
          (add-text-properties
           start (point)
           (list 'agent-pane-project-root root
                 'help-echo root))
          (dolist (s sessions)
            (let* ((ss (point))
                   (file (plist-get s :file))
                   (active-p (and active-session-file
                                  (equal (agent-pane-sessions--normalize-file file)
                                         active-session-file)))
                   (face (if active-p
                             '(agent-pane-sessions-session
                               agent-pane-sessions-active-session)
                           'agent-pane-sessions-session)))
              (insert (propertize
                       (format "** %s\n" (agent-pane-sessions--format-session-line s))
                       'face face))
              (add-text-properties
               ss (point)
               (list 'agent-pane-session-file file
                     'agent-pane-session-active active-p
                     'agent-pane-project-root root
                     'help-echo (string-join
                                 (delq nil
                                       (list file
                                             (when-let ((sid (plist-get s :session-id)))
                                               (format "sessionId: %s" sid))
                                             (when-let ((created (plist-get s :created)))
                                               (format "created: %s"
                                                       (format-time-string "%F %T" created)))))
                                 "\n"))))))))
    (when (and agent-pane-sessions-project-collapsed-by-default
               (string-empty-p query))
      (outline-hide-sublevels 1))
    (unless (or (and saved-session-file
                     (agent-pane-sessions--goto-prop-value
                      'agent-pane-session-file saved-session-file))
                (and saved-project-root
                     (agent-pane-sessions--goto-prop-value
                      'agent-pane-project-root saved-project-root)))
      (goto-char (point-min))
      (forward-line (max 0 (1- saved-line))))
    (move-to-column saved-col)
    (agent-pane-sessions--ensure-status-animation)))

(defun agent-pane-sessions-refresh ()
  "Refresh the sidebar index and re-render."
  (interactive)
  (setq-local agent-pane-sessions--index (agent-pane-sessions--scan))
  (agent-pane-sessions--render)
  (setq buffer-read-only t))

(defun agent-pane-sessions-refresh-if-visible ()
  "Refresh the sessions sidebar if the buffer exists."
  (when-let ((buf (get-buffer agent-pane-sessions-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'agent-pane-sessions-mode)
          (agent-pane-sessions-refresh))))))

(defun agent-pane-sessions-rerender-if-visible ()
  "Re-render sessions sidebar if visible, without rescanning transcript files."
  (when-let ((buf (get-buffer agent-pane-sessions-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'agent-pane-sessions-mode)
          (let ((inhibit-read-only t))
            (agent-pane-sessions--render)
            (setq buffer-read-only t)))))))

(defun agent-pane-sessions--chat-buffer-name-for-transcript (file)
  "Return deterministic chat buffer name for transcript FILE."
  (format "%s<%s>"
          (if (boundp 'agent-pane-buffer-name)
              agent-pane-buffer-name
            "*agent-pane*")
          (file-name-base (or file "session"))))

(defun agent-pane-sessions--chat-target-window ()
  "Return preferred window for showing chat when sidebar is visible.

When the sessions sidebar is present, prefer any other window on the same
frame (usually the right pane in `agent-pane-app').  Return nil when no
sidebar window is visible."
  (let* ((sessions-buf (get-buffer agent-pane-sessions-buffer-name))
         (sessions-win
          (or (and sessions-buf
                   (eq (current-buffer) sessions-buf)
                   (window-live-p (selected-window))
                   (eq (window-buffer (selected-window)) sessions-buf)
                   (selected-window))
              (and sessions-buf
                   (car (get-buffer-window-list sessions-buf nil t))))))
    (when (window-live-p sessions-win)
      (let* ((frame (window-frame sessions-win))
             (wins (window-list frame nil (frame-first-window frame))))
        (or (cl-find-if (lambda (w)
                          (and (window-live-p w)
                               (not (window-minibuffer-p w))
                               (not (eq w sessions-win))
                               (with-current-buffer (window-buffer w)
                                 (derived-mode-p 'agent-pane-mode))))
                        wins)
            (cl-find-if (lambda (w)
                          (and (window-live-p w)
                               (not (window-minibuffer-p w))
                               (not (eq w sessions-win))))
                        wins))))))

(defun agent-pane-sessions--display-chat-buffer (buf)
  "Display chat BUF without spawning extra windows when sidebar is visible."
  (if-let ((win (agent-pane-sessions--chat-target-window)))
      (progn
        (set-window-buffer win buf)
        (select-window win)
        buf)
    (pop-to-buffer buf)))

(defun agent-pane-sessions--open-transcript-in-chat (file)
  "Open transcript FILE in a dedicated chat buffer as replayed messages."
  (let* ((meta (agent-pane-sessions--parse-transcript-metadata file))
         (messages (agent-pane-sessions--parse-transcript-messages file))
         (root0 (or (plist-get meta :project-root) ""))
         (root (if (string-empty-p root0) default-directory root0))
         (existing (agent-pane-sessions--chat-buffer-for-transcript file))
         (existing-point (and (buffer-live-p existing)
                              (with-current-buffer existing
                                (point))))
         (buf (or existing
                  (get-buffer-create
                   (agent-pane-sessions--chat-buffer-name-for-transcript file)))))
    (require 'agent-pane)
    (agent-pane-sessions--display-chat-buffer buf)
    (with-current-buffer buf
      (setq default-directory
            (file-name-as-directory (expand-file-name (or root default-directory))))
      (unless (derived-mode-p 'agent-pane-mode)
        (agent-pane-mode))
      ;; If this buffer already represents FILE, keep current in-memory state
      ;; (including any active ACP run) instead of resetting it.
      (let* ((state (and (boundp 'agent-pane--state) agent-pane--state))
             (same-file (and (listp state)
                             (map-elt state :transcript-file)
                             (equal (file-truename (expand-file-name (map-elt state :transcript-file)))
                                    (file-truename (expand-file-name file))))))
        (unless same-file
          (setq-local agent-pane--state (agent-pane--make-state :messages messages))
          (map-put! agent-pane--state :session-title (plist-get meta :title))
          (map-put! agent-pane--state :transcript-file file)
          (map-put! agent-pane--state :resume-session-id (plist-get meta :session-id))
          (map-put! agent-pane--state :transcript-session-id-logged
                    (and (plist-get meta :session-id) t))
          (agent-pane--rerender))
        (map-put! agent-pane--state :session-seen-message-count
                  (length (or (map-elt agent-pane--state :messages) nil)))
        (cond
         (same-file
          (when (integerp existing-point)
            (goto-char (min (point-max) (max (point-min) existing-point)))))
         ((fboundp 'agent-pane--goto-input)
          (agent-pane--goto-input))
         (t
          (goto-char (point-max))))))))

(defun agent-pane-sessions-open-at-point ()
  "Open the transcript session at point in the chat view."
  (interactive)
  (let ((file (get-text-property (point) 'agent-pane-session-file)))
    (unless (and file (file-exists-p file))
      (user-error "No session at point"))
    (agent-pane-sessions--open-transcript-in-chat file)
    (agent-pane-sessions-rerender-if-visible)))

(defun agent-pane-sessions-open-file-at-point ()
  "Open the transcript Markdown file at point in view mode."
  (interactive)
  (let ((file (get-text-property (point) 'agent-pane-session-file)))
    (unless (and file (file-exists-p file))
      (user-error "No session at point"))
    (find-file-other-window file)
    (when (fboundp 'view-mode)
      (view-mode 1))))

(defun agent-pane-sessions--set-transcript-title (file title)
  "Set transcript header title in FILE to TITLE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (line (format "- title: %s" title)))
      (cond
       ((re-search-forward (rx bol "- title:" (* blank) (* nonl) eol) nil t)
        (replace-match line t t))
       ((re-search-forward (rx bol "- created:" (* nonl) eol) nil t)
        (forward-line 1)
        (insert line "\n"))
       ((re-search-forward (rx bol "---" eol) nil t)
        (beginning-of-line)
        (insert line "\n"))
       (t
        (goto-char (point-min))
        (insert "# agent-pane transcript\n\n" line "\n\n---\n\n"))))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun agent-pane-sessions-rename-title-at-point ()
  "Rename transcript title at point and persist to transcript header."
  (interactive)
  (let ((file (get-text-property (point) 'agent-pane-session-file)))
    (unless (and file (file-exists-p file))
      (user-error "No session at point"))
    (let* ((meta (agent-pane-sessions--parse-transcript-metadata file))
           (current (or (plist-get meta :title) ""))
           (next (string-trim (read-string "Rename session title: " current))))
      (when (string-empty-p next)
        (user-error "Title cannot be empty"))
      (agent-pane-sessions--set-transcript-title file next)
      (agent-pane-sessions-refresh)
      (message "Renamed session: %s" next))))

(defun agent-pane-sessions--neighbor-session-file ()
  "Return the file path of the nearest neighboring session line.

Prefers the next session line; falls back to the previous one."
  (or (save-excursion
        (forward-line 1)
        (while (and (not (eobp))
                    (not (get-text-property (point) 'agent-pane-session-file)))
          (forward-line 1))
        (get-text-property (point) 'agent-pane-session-file))
      (save-excursion
        (forward-line -1)
        (while (and (not (bobp))
                    (not (get-text-property (point) 'agent-pane-session-file)))
          (forward-line -1))
        (get-text-property (point) 'agent-pane-session-file))))

(defun agent-pane-sessions-delete-at-point ()
  "Delete transcript session at point after confirmation."
  (interactive)
  (let ((file (get-text-property (point) 'agent-pane-session-file)))
    (unless (and file (file-exists-p file))
      (user-error "No session at point"))
    (when (y-or-n-p (format "Delete session transcript %s? "
                            (file-name-nondirectory file)))
      (let ((neighbor (agent-pane-sessions--neighbor-session-file)))
        (agent-pane-sessions--close-chat-buffers-for-transcript file)
        (delete-file file)
        (agent-pane-sessions-refresh)
        (when neighbor
          (agent-pane-sessions--goto-prop-value
           'agent-pane-session-file neighbor))
        (message "Deleted session: %s" (file-name-nondirectory file))))))

(defun agent-pane-sessions-set-filter (query)
  "Set sidebar filter QUERY and re-render.

When QUERY is empty, clear the filter."
  (interactive
   (list (read-string "Session filter: " (or agent-pane-sessions--filter-query ""))))
  (setq-local agent-pane-sessions--filter-query
              (let ((q (string-trim (or query ""))))
                (unless (string-empty-p q) q)))
  (agent-pane-sessions--render)
  (setq buffer-read-only t))

(defun agent-pane-sessions-clear-filter ()
  "Clear sidebar filter and re-render."
  (interactive)
  (setq-local agent-pane-sessions--filter-query nil)
  (agent-pane-sessions--render)
  (setq buffer-read-only t))

(defun agent-pane-sessions-toggle-sort ()
  "Toggle sidebar sort mode and re-render."
  (interactive)
  (setq-local agent-pane-sessions--sort-mode
              (pcase agent-pane-sessions--sort-mode
                ('title 'recency)
                (_ 'title)))
  (agent-pane-sessions--render)
  (setq buffer-read-only t)
  (message "Sort mode: %s" (agent-pane-sessions--sort-mode-label)))

(defun agent-pane-sessions--normalize-project-root (root)
  "Return normalized absolute project ROOT without trailing slash."
  (directory-file-name (expand-file-name root)))

(defun agent-pane-sessions--read-project-root (&optional initial-root)
  "Prompt for a project root and return normalized absolute path.
INITIAL-ROOT seeds the prompt when non-nil."
  (let* ((seed0 (or initial-root default-directory))
         (seed (file-name-as-directory
                (expand-file-name
                 (if (string-empty-p (or seed0 ""))
                     default-directory
                   seed0))))
         (dir (read-directory-name "Project root: " seed seed t)))
    (agent-pane-sessions--normalize-project-root dir)))

(defun agent-pane-sessions--start-new-chat (root)
  "Start a new agent-pane chat rooted at ROOT.
Always creates a fresh chat buffer, leaving existing runs untouched."
  (let* ((root* (agent-pane-sessions--normalize-project-root root)))
    (unless (file-directory-p root*)
      (user-error "Project root does not exist: %s" root*))
    (require 'agent-pane)
    (let* ((proj (file-name-nondirectory root*))
           (base (format "%s<%s-new>"
                         (if (boundp 'agent-pane-buffer-name)
                             agent-pane-buffer-name
                           "*agent-pane*")
                         proj))
           (buf (generate-new-buffer base)))
      (agent-pane-sessions--display-chat-buffer buf)
      (with-current-buffer buf
        (setq default-directory (file-name-as-directory root*))
        (agent-pane-mode)
        (when (fboundp 'agent-pane--goto-input)
          (agent-pane--goto-input))))))

(defun agent-pane-sessions-new-chat ()
  "Start a new agent-pane chat for project at point.
When point is not on a known project, prompt for a project root."
  (interactive)
  (let ((root (or (get-text-property (point) 'agent-pane-project-root)
                  (agent-pane-sessions--read-project-root))))
    (agent-pane-sessions--start-new-chat root)))

(defun agent-pane-sessions-add-project (root)
  "Start a new chat for ROOT selected via directory prompt."
  (interactive
   (list (agent-pane-sessions--read-project-root
          (get-text-property (point) 'agent-pane-project-root))))
  (agent-pane-sessions--start-new-chat root))

(defun agent-pane-sessions--on-buffer-killed ()
  "Handle sessions buffer teardown."
  (agent-pane-sessions--stop-status-animation))

(defvar agent-pane-sessions-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m special-mode-map)
    (define-key m (kbd "g") #'agent-pane-sessions-refresh)
    (define-key m (kbd "RET") #'agent-pane-sessions-open-at-point)
    (define-key m (kbd "o") #'agent-pane-sessions-open-file-at-point)
    (define-key m (kbd "r") #'agent-pane-sessions-rename-title-at-point)
    (define-key m (kbd "C-k") #'agent-pane-sessions-delete-at-point)
    (define-key m (kbd "/") #'agent-pane-sessions-set-filter)
    (define-key m (kbd "c") #'agent-pane-sessions-clear-filter)
    (define-key m (kbd "s") #'agent-pane-sessions-toggle-sort)
    (define-key m (kbd "TAB") #'outline-toggle-children)
    (define-key m (kbd "n") #'agent-pane-sessions-new-chat)
    (define-key m (kbd "a") #'agent-pane-sessions-add-project)
    (define-key m (kbd "C-c v") #'agent-pane-toggle-header-details-anywhere)
    (define-key m (kbd "C-c C-v") #'agent-pane-toggle-header-details-anywhere)
    (define-key m (kbd "C-c d") #'agent-pane-view-diff-at-point-anywhere)
    (define-key m (kbd "C-c C-d") #'agent-pane-view-diff-at-point-anywhere)
    (define-key m (kbd "q") #'agent-pane-exit)
    (define-key m (kbd "C-c C-q") #'agent-pane-exit)
    m)
  "Keymap for `agent-pane-sessions-mode'.")

(define-derived-mode agent-pane-sessions-mode special-mode "agent-pane-sessions"
  "Major mode for agent-pane sessions sidebar."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local outline-regexp "\\*+ ")
  (setq-local outline-level
              (lambda ()
                (let ((m (match-string 0)))
                  (if m (length (string-trim-right m)) 0))))
  (setq-local agent-pane-sessions--sort-mode
              (or agent-pane-sessions--sort-mode
                  agent-pane-sessions-default-sort-mode))
  (outline-minor-mode 1)
  (use-local-map agent-pane-sessions-mode-map)
  (add-hook 'kill-buffer-hook #'agent-pane-sessions--on-buffer-killed nil t)
  (agent-pane-sessions-refresh))

;;;###autoload
(defun agent-pane-sessions ()
  "Open the agent-pane sessions sidebar buffer."
  (interactive)
  (let ((buf (get-buffer-create agent-pane-sessions-buffer-name)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-pane-sessions-mode)
        (agent-pane-sessions-mode)))))

(provide 'agent-pane-sessions)
;;; agent-pane-sessions.el ends here
