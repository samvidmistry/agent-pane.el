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

(defvar agent-pane-transcript-directory)

(defvar-local agent-pane-sessions--index nil
  "Cached sidebar index.

This is a list of (PROJECT-ROOT . SESSIONS).  SESSIONS is a list of plists.")

(defvar-local agent-pane-sessions--filter-query nil
  "Current text filter query for the sidebar.")

(defvar-local agent-pane-sessions--sort-mode nil
  "Current sort mode symbol for the sidebar.")

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

(defun agent-pane-sessions--chat-buffer-for-transcript (file)
  "Return live chat buffer associated with transcript FILE, or nil."
  (let ((target (file-truename (expand-file-name file))))
    (cl-loop for buf in (buffer-list)
             when (buffer-live-p buf)
             thereis
             (with-current-buffer buf
               (when (derived-mode-p 'agent-pane-mode)
                 (let ((state (and (boundp 'agent-pane--state) agent-pane--state))
                       (tf nil))
                   (setq tf (and (listp state) (map-elt state :transcript-file)))
                   (when (and tf (file-exists-p tf)
                              (equal (file-truename (expand-file-name tf)) target))
                     buf)))))))

(defun agent-pane-sessions--session-running-p (session)
  "Return non-nil when SESSION currently has an active running agent job."
  (let* ((file (plist-get session :file))
         (buf (and file (file-exists-p file)
                   (agent-pane-sessions--chat-buffer-for-transcript file))))
    (and buf
         (with-current-buffer buf
           (let ((state (and (boundp 'agent-pane--state) agent-pane--state)))
             (and (listp state)
                  (or (map-elt state :connecting)
                      (map-elt state :prompt-in-flight)
                      (memq (map-elt state :in-progress)
                            '(waiting streaming cancelling)))))))))

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
     (if (agent-pane-sessions--session-running-p session)
         "● "
       "  ")
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

(defun agent-pane-sessions--render ()
  "Render the sessions sidebar in the current buffer."
  (let* ((inhibit-read-only t)
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
    (insert (propertize "Keys: RET open/replay | o open .md | n new | / filter | c clear | s sort | r rename | C-k delete | TAB fold | g refresh | q exit | ● running\n\n"
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
            (let ((ss (point)))
              (insert (propertize
                       (format "** %s\n" (agent-pane-sessions--format-session-line s))
                       'face 'agent-pane-sessions-session))
              (add-text-properties
               ss (point)
               (list 'agent-pane-session-file (plist-get s :file)
                     'agent-pane-project-root root
                     'help-echo (string-join
                                 (delq nil
                                       (list (plist-get s :file)
                                             (when-let ((sid (plist-get s :session-id)))
                                               (format "sessionId: %s" sid))
                                             (when-let ((created (plist-get s :created)))
                                               (format "created: %s"
                                                       (format-time-string "%F %T" created)))))
                                 "\n"))))))))
    (goto-char (point-min))
    (forward-line 4)
    (when (and agent-pane-sessions-project-collapsed-by-default
               (string-empty-p query))
      (outline-hide-sublevels 1))))

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

(defun agent-pane-sessions--open-transcript-in-chat (file)
  "Open transcript FILE in a dedicated chat buffer as replayed messages."
  (let* ((meta (agent-pane-sessions--parse-transcript-metadata file))
         (messages (agent-pane-sessions--parse-transcript-messages file))
         (root0 (or (plist-get meta :project-root) ""))
         (root (if (string-empty-p root0) default-directory root0))
         (existing (agent-pane-sessions--chat-buffer-for-transcript file))
         (buf (or existing
                  (get-buffer-create
                   (agent-pane-sessions--chat-buffer-name-for-transcript file)))))
    (require 'agent-pane)
    (pop-to-buffer buf)
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
          (agent-pane--rerender)))
      (when (fboundp 'agent-pane--goto-input)
        (agent-pane--goto-input)))))

(defun agent-pane-sessions-open-at-point ()
  "Open the transcript session at point in the chat view."
  (interactive)
  (let ((file (get-text-property (point) 'agent-pane-session-file)))
    (unless (and file (file-exists-p file))
      (user-error "No session at point"))
    (agent-pane-sessions--open-transcript-in-chat file)))

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

(defun agent-pane-sessions-delete-at-point ()
  "Delete transcript session at point after confirmation."
  (interactive)
  (let ((file (get-text-property (point) 'agent-pane-session-file)))
    (unless (and file (file-exists-p file))
      (user-error "No session at point"))
    (when (y-or-n-p (format "Delete session transcript %s? "
                            (file-name-nondirectory file)))
      (delete-file file)
      (agent-pane-sessions-refresh)
      (message "Deleted session: %s" (file-name-nondirectory file)))))

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

(defun agent-pane-sessions-new-chat ()
  "Start a new agent-pane chat for the project at point.
Always creates a fresh chat buffer, leaving existing runs untouched."
  (interactive)
  (let ((root (get-text-property (point) 'agent-pane-project-root)))
    (unless root
      (user-error "No project at point"))
    (require 'agent-pane)
    (let* ((proj (file-name-nondirectory (directory-file-name (expand-file-name root))))
           (base (format "%s<%s-new>"
                         (if (boundp 'agent-pane-buffer-name)
                             agent-pane-buffer-name
                           "*agent-pane*")
                         proj))
           (buf (generate-new-buffer base)))
      (pop-to-buffer buf)
      (with-current-buffer buf
        (setq default-directory (file-name-as-directory (expand-file-name root)))
        (agent-pane-mode)
        (when (fboundp 'agent-pane--goto-input)
          (agent-pane--goto-input))))))

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
