;;; agent-pane-sessions.el --- Sidebar for agent-pane transcripts -*- lexical-binding: t; -*-



;; Package-Requires: ((emacs "29.1"))



;;; Commentary:

;;

;; Provides a collapsible sidebar listing all saved agent-pane transcript

;; sessions grouped by project.

;;

;;; Code:



(require 'agent-pane-config)



(require 'cl-lib)

(require 'map)

(require 'subr-x)

(require 'outline)



;; Avoid compile-time warnings; these are defined in agent-pane.el.

(declare-function agent-pane-mode "agent-pane")

(declare-function agent-pane--goto-input "agent-pane")

(defvar agent-pane-buffer-name)



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



(defvar agent-pane-transcript-directory)



(defvar-local agent-pane-sessions--index nil

  "Cached sidebar index.



This is a list of (PROJECT-ROOT . SESSIONS).  SESSIONS is a list of plists.")



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



(defun agent-pane-sessions--parse-transcript-metadata (file)

  "Parse FILE header and return a plist.



Keys:

- :file

- :project-root

- :created (time value)

- :created-string

- :session-id

- :title (first user line when available)"

  (with-temp-buffer

    (insert-file-contents file nil 0 8192)

    (goto-char (point-min))

    (let ((project-root nil)

          (created-string nil)

          (created nil)

          (session-id nil)

          (title nil))

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



      ;; Fallback: created from filename.

      (setq created

            (or (and created-string (ignore-errors (date-to-time created-string)))

                (agent-pane-sessions--parse-created-from-filename file)

                (ignore-errors (file-attribute-modification-time (file-attributes file)))))



      ;; Fallback: Find first user message line as a session title.

      (when (or (null title) (string-empty-p title))

        (goto-char (point-min))

        (when (re-search-forward (rx bol "## " (+? any) "User" (*? any) eol) nil t)

          (forward-line 1)

          (while (and (not (eobp)) (string-empty-p (string-trim (or (thing-at-point 'line t) ""))))

            (forward-line 1))

          (setq title (string-trim (or (thing-at-point 'line t) "")))))



      (list :file file

            :project-root (or project-root "")

            :created created

            :created-string created-string

            :session-id session-id

            :title title))))



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

         (setq sessions

               (sort sessions

                     (lambda (a b)

                       (time-less-p (or (plist-get b :created) 0)

                                    (or (plist-get a :created) 0)))))

         (push (cons root sessions) result))

       by-root)

      (setq result

            (sort result (lambda (a b)

                           (string-lessp (agent-pane-sessions--project-name (car a))

                                         (agent-pane-sessions--project-name (car b))))))

      result)))



(defun agent-pane-sessions--format-session-line (session)

  "Format a SESSION plist for display.



Prefer showing a human-readable title; the timestamp is secondary.

Session ids are available via `help-echo`."

  (let* ((created (plist-get session :created))

         (created-s (if created (format-time-string "%F %R" created) ""))

         (title0 (or (plist-get session :title) ""))

         (title (string-trim (replace-regexp-in-string "[\n\t ]+" " " title0))))

    (cond

     ((not (string-empty-p title))

      (let ((t1 (truncate-string-to-width title 80 nil nil "…")))

        (if (string-empty-p created-s)

            t1

          (format "%s (%s)" t1 created-s))))

     (t

      created-s))))



(defun agent-pane-sessions--render ()

  "Render the sessions sidebar in the current buffer."

  (let ((inhibit-read-only t))

    (erase-buffer)

    (insert "agent-pane sessions\n\n")

    (dolist (group agent-pane-sessions--index)

      (let* ((root (car group))

             (sessions (cdr group))

             (name (agent-pane-sessions--project-name root))

             (count (length sessions))

             (start (point)))

        (insert (format "* %s (%d)\n" name count))

        (add-text-properties

         start (point)

         (list 'agent-pane-project-root root

               'help-echo root))

        (dolist (s sessions)

          (let ((ss (point)))

            (insert (format "** %s\n" (agent-pane-sessions--format-session-line s)))

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

                                             (format "created: %s" (format-time-string "%F %T" created)))))

                               "\n")))))))

    (goto-char (point-min))

    (forward-line 2)

    (when agent-pane-sessions-project-collapsed-by-default

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



(defun agent-pane-sessions-open-at-point ()

  "Open the transcript session at point."

  (interactive)

  (let ((file (get-text-property (point) 'agent-pane-session-file)))

    (unless (and file (file-exists-p file))

      (user-error "No session at point"))

    (find-file-other-window file)

    (when (fboundp 'view-mode)

      (view-mode 1))))



(defun agent-pane-sessions-new-chat ()

  "Start a new agent-pane chat for the project at point."

  (interactive)

  (let ((root (get-text-property (point) 'agent-pane-project-root)))

    (unless root

      (user-error "No project at point"))

    (require 'agent-pane)

    (let ((buf (get-buffer-create (if (boundp 'agent-pane-buffer-name)

                                      agent-pane-buffer-name

                                    "*agent-pane*"))))

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

    (define-key m (kbd "TAB") #'outline-toggle-children)

    (define-key m (kbd "n") #'agent-pane-sessions-new-chat)

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

