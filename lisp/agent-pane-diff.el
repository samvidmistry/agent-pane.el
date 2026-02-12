;;; agent-pane-diff.el --- Diff extraction and viewing helpers -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;;; Commentary:
;;
;; Helpers for extracting structured file-change diffs from ACP tool-call
;; payloads and rendering those diffs inside Emacs.
;;
;;; Code:

(require 'agent-pane-config)
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

(defun agent-pane--extract-diff-info (tool-call)
  "Extract structured diff info from TOOL-CALL payload.
Return plist with keys `:old', `:new', and optional `:file', or nil."
  (let* ((content (map-elt tool-call 'content))
         (raw-input (map-elt tool-call 'rawInput))
         (diff-item
          (cond
           ((and (listp content)
                 (equal (map-elt content 'type) "diff"))
            content)
           ((vectorp content)
            (seq-find (lambda (item)
                        (equal (map-elt item 'type) "diff"))
                      (append content nil)))
           ((and (listp content)
                 (listp (car-safe content)))
            (seq-find (lambda (item)
                        (equal (map-elt item 'type) "diff"))
                      content))
           ((and raw-input (map-elt raw-input 'new_str))
            `((oldText . ,(or (map-elt raw-input 'old_str) ""))
              (newText . ,(map-elt raw-input 'new_str))
              (path . ,(or (map-elt raw-input 'path)
                           (map-elt raw-input 'fileName)))) )
           ((and raw-input (map-elt raw-input 'newText))
            `((oldText . ,(or (map-elt raw-input 'oldText) ""))
              (newText . ,(map-elt raw-input 'newText))
              (path . ,(or (map-elt raw-input 'path)
                           (map-elt raw-input 'fileName)))) )
           ((and raw-input (map-elt raw-input 'diff))
            (let ((parsed (agent-pane--parse-unified-diff (map-elt raw-input 'diff))))
              `((oldText . ,(car parsed))
                (newText . ,(cdr parsed))
                (path . ,(or (map-elt raw-input 'fileName)
                             (map-elt raw-input 'path))))))
           (t nil))))
    (when-let* ((new-text (map-elt diff-item 'newText)))
      (let ((file (map-elt diff-item 'path))
            (old-text (or (map-elt diff-item 'oldText) "")))
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

(defun agent-pane--show-diff-diff-mode (diff &optional title)
  "Show DIFF in a unified `diff-mode' buffer.
TITLE is shown in the header line when non-nil."
  (let* ((file (plist-get diff :file))
         (old-file (agent-pane--diff-temp-file "agent-pane-old" file (plist-get diff :old)))
         (new-file (agent-pane--diff-temp-file "agent-pane-new" file (plist-get diff :new)))
         (buf (get-buffer-create "*agent-pane diff*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (let ((inhibit-read-only t)
                  (diff-mode-read-only nil))
              (erase-buffer)
              (diff-no-select old-file new-file "-U3" t buf)
              (goto-char (point-min))
              (when (looking-at "^diff .*\n")
                (delete-region (point) (progn (forward-line 1) (point))))
              (diff-mode)
              (read-only-mode 1)
              (setq-local header-line-format
                          (concat
                           (when title (format "%s  |  " title))
                           "q quit | n/p hunk navigation"))))
          (pop-to-buffer buf))
      (ignore-errors (delete-file old-file))
      (ignore-errors (delete-file new-file)))))

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
