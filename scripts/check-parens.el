;;; check-parens.el --- Batch parens/syntax check for Elisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Invoked by scripts/check-parens.sh.
;;
;; This file computes the project root from `load-file-name` to avoid
;; MSYS path translation issues on Windows.
;;
;; Why: In this repo we often make quick iterative edits to large .el files.
;; A single missing ')' makes `require` fail with an unhelpful end-of-file
;; error.  This script runs `check-parens` on all project Elisp files and
;; prints actionable file:line:col diagnostics.

;;; Code:

(require 'cl-lib)

(defun agent-pane--check-parens--line-text ()
  "Return the current line's text (no properties)."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun agent-pane--check-parens--loc (pos)
  "Return (LINE . COL) for POS in current buffer."
  (save-excursion
    (goto-char pos)
    (cons (line-number-at-pos)
          (current-column))))

(defun agent-pane--check-parens--format-location (file pos)
  "Format FILE and POS as a file:line:col string."
  (pcase-let ((`(,line . ,col) (agent-pane--check-parens--loc pos)))
    (format "%s:%d:%d" file line (1+ col))))

(defun agent-pane--check-parens--diagnose (file err)
  "Diagnose a paren/syntax ERR in FILE.

Returns a human-readable multi-line string."
  (let* ((err-pos (point))
         (at (agent-pane--check-parens--format-location file err-pos))
         (line (agent-pane--check-parens--line-text))
         ;; Often, unmatched open parens manifest as errors at EOF.
         (ppss-eof (syntax-ppss (point-max)))
         (unclosed (nth 1 ppss-eof))
         (unclosed-str
          (when (and (numberp unclosed) (> unclosed 0))
            (save-excursion
              (goto-char unclosed)
              (format "%s: likely unclosed '(' here\n  %s"
                      (agent-pane--check-parens--format-location file unclosed)
                      (string-trim-right (agent-pane--check-parens--line-text)))))))
    (string-join
     (delq nil
           (list
            (format "%s: %s" at (error-message-string err))
            (format "  %s" (string-trim-right line))
            unclosed-str))
     "\n")))

(defun agent-pane--check-parens-file (file)
  "Return nil if FILE passes `check-parens`, else a diagnostic string."
  (with-temp-buffer
    (insert-file-contents file)
    ;; Ensure proper syntax table.
    (emacs-lisp-mode)
    (goto-char (point-min))
    (condition-case err
        (progn
          (check-parens)
          nil)
      ((user-error scan-error error)
       ;; `check-parens` signals `user-error` with point set to the mismatch.
       (agent-pane--check-parens--diagnose file err)))))

(let* ((root (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))
       (dirs (list (expand-file-name "lisp" root)
                   (expand-file-name "test" root)
                   (expand-file-name "scripts" root)))
       (files (cl-loop for d in dirs
                       when (file-directory-p d)
                       append (directory-files d t "\\.el\\'")))
       (failed nil))
  (dolist (f files)
    (when-let ((diag (agent-pane--check-parens-file f)))
      (setq failed t)
      (princ diag)
      (princ "\n\n")))
  (when failed
    (kill-emacs 1)))

;;; check-parens.el ends here
