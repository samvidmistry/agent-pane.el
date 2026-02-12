;;; agent-pane-diff.el --- Diff extraction and viewing helpers -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;;; Commentary:
;;
;; Helpers for extracting structured file-change diffs from ACP tool-call
;; payloads and rendering those diffs inside Emacs.
;;
;;; Code:

(require 'agent-pane-config)
(require 'cl-lib)
(require 'diff)
(require 'diff-mode)
(require 'ediff)
(require 'map)
(require 'seq)
(require 'subr-x)

(defvar agent-pane-diff-viewer)

(defun agent-pane--parse-unified-diff (diff-string)
  "Parse unified DIFF-STRING into old and new text.
Return a cons cell (OLD-TEXT . NEW-TEXT)."
  (let (old-lines new-lines in-hunk)
    (dolist (line (split-string (or diff-string "") "\n"))
      (cond
       ((string-match-p "^@@.*@@" line)
        (setq in-hunk t))
       ((and in-hunk (string-prefix-p " " line))
        (push (substring line 1) old-lines)
        (push (substring line 1) new-lines))
       ((and in-hunk (string-prefix-p "-" line))
        (push (substring line 1) old-lines))
       ((and in-hunk (string-prefix-p "+" line))
        (push (substring line 1) new-lines))))
    (cons (string-join (nreverse old-lines) "\n")
          (string-join (nreverse new-lines) "\n"))))

(defun agent-pane--map-elt-any (obj keys)
  "Return first non-nil value in OBJ for any symbol in KEYS."
  (let (val)
    (while (and keys (null val))
      (setq val (map-elt obj (car keys)))
      (setq keys (cdr keys)))
    val))

(defun agent-pane--diff-map-like-p (obj)
  "Return non-nil when OBJ is an alist map object."
  (and (listp obj)
       (consp obj)
       (consp (car obj))
       (symbolp (caar obj))))

(defun agent-pane--find-diff-item (obj)
  "Recursively find first ACP diff object inside OBJ."
  (cond
   ((null obj) nil)
   ((and (agent-pane--diff-map-like-p obj)
         (equal (map-elt obj 'type) "diff"))
    obj)
   ((agent-pane--diff-map-like-p obj)
    (or (agent-pane--find-diff-item (map-elt obj 'content))
        (agent-pane--find-diff-item (map-elt obj 'rawOutput))))
   ((vectorp obj)
    (seq-some #'agent-pane--find-diff-item (append obj nil)))
   ((listp obj)
    (seq-some #'agent-pane--find-diff-item obj))
   (t nil)))

(defun agent-pane--extract-diff-info (tool-call)
  "Extract structured diff info from TOOL-CALL payload.
Return plist with keys `:old', `:new', and optional `:file', or nil."
  (let* ((content (map-elt tool-call 'content))
         (raw-output (map-elt tool-call 'rawOutput))
         (raw-input (map-elt tool-call 'rawInput))
         (diff-item
          (or (agent-pane--find-diff-item content)
              (agent-pane--find-diff-item raw-output)
              (when-let ((new-str (agent-pane--map-elt-any raw-input '(newText new_str newString new_text))))
                `((oldText . ,(or (agent-pane--map-elt-any raw-input '(oldText old_str oldString old_text)) ""))
                  (newText . ,new-str)
                  (path . ,(or (agent-pane--map-elt-any raw-input '(path file fileName file_name uri))
                               ""))))
              (when-let ((diff-str (agent-pane--map-elt-any raw-input '(diff patch unifiedDiff))))
                (let ((parsed (agent-pane--parse-unified-diff diff-str)))
                  `((oldText . ,(car parsed))
                    (newText . ,(cdr parsed))
                    (path . ,(or (agent-pane--map-elt-any raw-input '(fileName file_name file path uri))
                                 ""))))))))
    (when-let* ((new-text (agent-pane--map-elt-any diff-item '(newText new_str newString new_text))))
      (let ((file (or (agent-pane--map-elt-any diff-item '(path file fileName file_name uri))
                      (agent-pane--map-elt-any raw-input '(path file fileName file_name uri))))
            (old-text (or (agent-pane--map-elt-any diff-item '(oldText old_str oldString old_text)) "")))
        (append (list :old old-text :new new-text)
                (when (and file (not (string-empty-p (format "%s" file))))
                  (list :file (format "%s" file))))))))

(defun agent-pane--diff-summary (diff)
  "Return a compact summary string for DIFF plist."
  (when diff
    (let* ((file (or (plist-get diff :file) "(unknown file)"))
           (old-lines (length (split-string (or (plist-get diff :old) "") "\n" t)))
           (new-lines (length (split-string (or (plist-get diff :new) "") "\n" t))))
      (format "%s (%d → %d lines)" file old-lines new-lines))))

(defun agent-pane--diff-temp-file (prefix file content)
  "Create temp file using PREFIX, FILE extension, and CONTENT."
  (let* ((ext (or (and (stringp file) (file-name-extension file)) "txt"))
         (path (make-temp-file prefix nil (format ".%s" ext))))
    (with-temp-file path
      (insert (or content "")))
    path))

(defun agent-pane--diff-line-count (text)
  "Return number of logical lines in TEXT."
  (if (string-empty-p (or text ""))
      0
    (+ (cl-count ?\n text)
       (if (string-suffix-p "\n" text) 0 1))))

(defun agent-pane--diff-lines (text)
  "Return logical lines for TEXT without trailing newline sentinel."
  (if (string-empty-p (or text ""))
      nil
    (let ((lines (split-string text "\n" nil)))
      (if (string-suffix-p "\n" text)
          (butlast lines)
        lines))))

(defun agent-pane--insert-unified-diff (diff)
  "Insert unified diff text for DIFF into current buffer."
  (let* ((file (or (plist-get diff :file) "unknown"))
         (old (or (plist-get diff :old) ""))
         (new (or (plist-get diff :new) ""))
         (old-count (agent-pane--diff-line-count old))
         (new-count (agent-pane--diff-line-count new))
         (old-start (if (zerop old-count) 0 1))
         (new-start (if (zerop new-count) 0 1)))
    (insert (format "--- a/%s\n" file))
    (insert (format "+++ b/%s\n" file))
    (insert (format "@@ -%d,%d +%d,%d @@\n"
                    old-start old-count new-start new-count))
    (dolist (line (agent-pane--diff-lines old))
      (insert "-" line "\n"))
    (dolist (line (agent-pane--diff-lines new))
      (insert "+" line "\n"))))

(defun agent-pane--show-diff-diff-mode (diff &optional title)
  "Show DIFF in a unified `diff-mode' buffer.
TITLE is shown in the header line when non-nil."
  (let ((buf (get-buffer-create "*agent-pane diff*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (diff-mode-read-only nil))
        (erase-buffer)
        (agent-pane--insert-unified-diff diff)
        (goto-char (point-min))
        (diff-mode)
        (read-only-mode 1)
        (setq-local header-line-format
                    (concat
                     (when title (format "%s  |  " title))
                     "q quit | n/p hunk navigation"))))
    (pop-to-buffer buf)))

(defun agent-pane--show-diff-ediff (diff)
  "Show DIFF using `ediff-files'."
  (let* ((file (plist-get diff :file))
         (old-file (agent-pane--diff-temp-file "agent-pane-old" file (plist-get diff :old)))
         (new-file (agent-pane--diff-temp-file "agent-pane-new" file (plist-get diff :new)))
         cleanup)
    (setq cleanup
          (lambda ()
            (remove-hook 'ediff-after-quit-hook-internal cleanup)
            (ignore-errors (delete-file old-file))
            (ignore-errors (delete-file new-file))))
    (add-hook 'ediff-after-quit-hook-internal cleanup)
    (ediff-files old-file new-file)))

(defun agent-pane--show-diff (diff &optional title)
  "Show DIFF using the configured viewer.
TITLE is used for diff buffer metadata when supported."
  (unless (and (listp diff) (stringp (plist-get diff :new)))
    (error "No diff data available"))
  (pcase agent-pane-diff-viewer
    ('ediff (agent-pane--show-diff-ediff diff))
    (_ (agent-pane--show-diff-diff-mode diff title))))

(provide 'agent-pane-diff)
;;; agent-pane-diff.el ends here
