;;; agent-pane-markdown.el --- Markdown-ish fontification for agent-pane -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;;; Commentary:
;;
;; A tiny, streaming-friendly markdown-ish renderer.
;;
;; We intentionally implement only a few constructs (strong/code/fences) using
;; text properties.  The logic is designed to be safe when applied repeatedly to
;; a moving window of text during streaming.
;;
;;; Code:
(require 'agent-pane-config)
(require 'cl-lib)
(require 'subr-x)

(defun agent-pane--md--normalize-role (role)
  "Return ROLE normalized to a known symbol for markdown fontification."
  (cond
   ((memq role '(assistant thought)) role)
   ((stringp role)
    (pcase (downcase role)
      ("assistant" 'assistant)
      ("thought" 'thought)
      (_ nil)))
   (t nil)))

(defun agent-pane--md--strong-face-for-role (role)
  "Return the face used for markdown strong for ROLE."
  (pcase role
    ('thought 'agent-pane-markdown-strong-thought)
    (_ 'agent-pane-markdown-strong-assistant)))

(defun agent-pane--md--table-cells (line)
  "Parse markdown table cells from LINE."
  (let* ((raw (split-string (or line "") "|" nil))
         (cells raw))
    (when (and cells (string-prefix-p "|" (or line "")))
      (setq cells (cdr cells)))
    (when (and cells (string-suffix-p "|" (or line "")))
      (setq cells (butlast cells)))
    (mapcar #'string-trim cells)))

(defun agent-pane--md--table-separator-cell-p (cell)
  "Return non-nil if CELL is a markdown table separator cell."
  (string-match-p (rx string-start (? ":") (+ "-") (? ":") string-end)
                  (string-trim (or cell ""))))

(defun agent-pane--md--table-separator-row-p (cells)
  "Return non-nil if CELLS form a markdown separator row."
  (and cells
       (not (null (cdr cells)))
       (cl-every #'agent-pane--md--table-separator-cell-p cells)))

(defun agent-pane--md--table-cell-align (cell)
  "Infer alignment symbol from table separator CELL."
  (let ((s (string-trim (or cell ""))))
    (cond
     ((and (string-prefix-p ":" s) (string-suffix-p ":" s)) 'center)
     ((string-suffix-p ":" s) 'right)
     (t 'left))))

(defun agent-pane--md--table-pad-cell (text width align)
  "Pad TEXT to WIDTH using ALIGN (`left', `right', or `center')."
  (let* ((s (or text ""))
         (pad (max 0 (- width (string-width s)))))
    (pcase align
      ('right (concat (make-string pad ?\s) s))
      ('center (let* ((l (/ pad 2))
                      (r (- pad l)))
                 (concat (make-string l ?\s) s (make-string r ?\s))))
      (_ (concat s (make-string pad ?\s))))))

(defun agent-pane--md--table-separator-segment (width align)
  "Return a separator segment string for WIDTH and ALIGN."
  (let* ((w (max 3 width))
         (dashes (make-string w ?-)))
    (pcase align
      ('right (concat (substring dashes 0 (1- w)) ":"))
      ('center (if (= w 1)
                   ":"
                 (concat ":" (substring dashes 1 (1- w)) ":")))
      (_ dashes))))

(defun agent-pane--md--table-render-row (cells widths aligns separator-p)
  "Render markdown table row from CELLS.
WIDTHS and ALIGNS are column metadata.  SEPARATOR-P indicates a separator row."
  (let ((segments
         (cl-loop for i from 0 below (length widths)
                  for cell = (or (nth i cells) "")
                  for width = (or (nth i widths) 1)
                  for align = (or (nth i aligns) 'left)
                  collect (if separator-p
                              (agent-pane--md--table-separator-segment width align)
                            (agent-pane--md--table-pad-cell cell width align)))))
    (concat "| " (string-join segments " | ") " |")))

(defun agent-pane--md--fontify-table-block (rows)
  "Apply aligned table rendering to ROWS.
ROWS is a list of plists with keys `:beg', `:end', and `:text'."
  (let* ((parsed (mapcar (lambda (r) (agent-pane--md--table-cells (plist-get r :text))) rows))
         (sep-idx (cl-position-if #'agent-pane--md--table-separator-row-p parsed)))
    (when (and sep-idx (>= (length rows) 2))
      (let* ((col-count (apply #'max 1 (mapcar #'length parsed)))
             (rows* (mapcar (lambda (cells)
                              (append cells (make-list (max 0 (- col-count (length cells))) "")))
                            parsed))
             (aligns (cl-loop for i from 0 below col-count
                              collect (agent-pane--md--table-cell-align
                                       (or (nth i (nth sep-idx rows*)) "---"))))
             (widths (make-list col-count 3)))
        ;; Widths from non-separator rows.
        (cl-loop for ridx from 0 below (length rows*)
                 for cells in rows*
                 unless (= ridx sep-idx)
                 do (cl-loop for i from 0 below col-count
                             for cell = (or (nth i cells) "")
                             do (setf (nth i widths)
                                      (max (nth i widths) (string-width cell)))))
        ;; Apply aligned display line by line.
        (cl-loop for ridx from 0 below (length rows)
                 for row in rows
                 for cells in rows*
                 for rendered = (agent-pane--md--table-render-row
                                 cells widths aligns (= ridx sep-idx))
                 do (add-text-properties
                     (plist-get row :beg)
                     (plist-get row :end)
                     (list 'display (propertize rendered 'face 'agent-pane-markdown-table)
                           'face 'agent-pane-markdown-table
                           'agent-pane-md-table t)))))))

(defun agent-pane--md--fontify-tables ()
  "Find markdown table blocks in narrowed region and align them."
  (let (block)
    (cl-labels ((flush-block ()
                  (when block
                    (agent-pane--md--fontify-table-block (nreverse block))
                    (setq block nil))))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((lbeg (line-beginning-position))
               (lend (line-end-position))
               (line (buffer-substring-no-properties lbeg lend))
               (in-code (get-text-property lbeg 'agent-pane-md-code-block))
               (candidate (and (not in-code)
                               (string-match-p "|" line)
                               (not (string-empty-p (string-trim line))))))
          (if candidate
              (push (list :beg lbeg :end lend :text line) block)
            (flush-block))
          (forward-line 1)))
      (flush-block))))

(defun agent-pane--md-fontify-range (role beg end)
  "Apply minimal markdown fontification between BEG and END.
ROLE controls how some constructs are rendered (assistant vs thought).
This is intentionally lightweight and only handles a few common patterns:
- Strong: **text**
- Inline code: `code`
- Fenced code blocks: ``` ... ```"
  (let ((normalized-role (agent-pane--md--normalize-role role)))
    (when (and (integerp beg)
               (integerp end)
               (< beg end)
               normalized-role)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (let ((inhibit-read-only t)
                (strong-face (agent-pane--md--strong-face-for-role normalized-role)))
            ;; Ensure our invisibility spec is present/absent.
            (if agent-pane-markdown-hide-markup
                (add-to-invisibility-spec 'agent-pane-markup)
              (remove-from-invisibility-spec 'agent-pane-markup))
            ;; Clear prior markdown properties in this region.
            (remove-text-properties (point-min) (point-max)
                                    '(face nil
                                      invisible nil
                                      display nil
                                      mouse-face nil
                                      help-echo nil
                                      agent-pane-md-code-block nil
                                      agent-pane-md-link-url nil
                                      agent-pane-md-table nil))
            ;; Thought text should keep a distinct base styling even outside
            ;; markdown constructs.
            (when (eq normalized-role 'thought)
              (add-text-properties (point-min) (point-max)
                                   '(face agent-pane-role-thought)))
            ;; Fenced code blocks.
            (goto-char (point-min))
            (while (re-search-forward "^```.*$" nil t)
              (let* ((fence1-beg (match-beginning 0))
                     (fence1-end (match-end 0))
                     (block-beg (min (point-max) (1+ fence1-end))))
                (add-text-properties
                 fence1-beg fence1-end
                 `(face agent-pane-markdown-delimiter
                   ,@(when agent-pane-markdown-hide-markup
                       '(invisible agent-pane-markup))))
                (when (re-search-forward "^```.*$" nil t)
                  (let* ((fence2-beg (match-beginning 0))
                         (fence2-end (match-end 0))
                         (block-end (max block-beg (1- fence2-beg))))
                    (when (< block-beg block-end)
                      (add-text-properties block-beg block-end
                                           '(face agent-pane-markdown-code
                                             agent-pane-md-code-block t)))
                    (add-text-properties
                     fence2-beg fence2-end
                     `(face agent-pane-markdown-delimiter
                       ,@(when agent-pane-markdown-hide-markup
                           '(invisible agent-pane-markup))))))))
            ;; Inline code.
            (goto-char (point-min))
            (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
              (unless (get-text-property (match-beginning 0) 'agent-pane-md-code-block)
                (add-text-properties
                 (match-beginning 0) (1+ (match-beginning 0))
                 `(face agent-pane-markdown-delimiter
                   ,@(when agent-pane-markdown-hide-markup
                       '(invisible agent-pane-markup))))
                (add-text-properties (match-beginning 1) (match-end 1)
                                     '(face agent-pane-markdown-code))
                (add-text-properties
                 (1- (match-end 0)) (match-end 0)
                 `(face agent-pane-markdown-delimiter
                   ,@(when agent-pane-markdown-hide-markup
                       '(invisible agent-pane-markup))))))
            ;; Headings.
            (goto-char (point-min))
            (while (re-search-forward "^\\(#+\\)\\([ \t]+\\)\\(.+\\)$" nil t)
              (unless (get-text-property (match-beginning 0) 'agent-pane-md-code-block)
                (add-text-properties
                 (match-beginning 1) (match-end 1)
                 `(face agent-pane-markdown-delimiter
                   ,@(when agent-pane-markdown-hide-markup
                       '(invisible agent-pane-markup))))
                (add-text-properties (match-beginning 3) (match-end 3)
                                     '(face agent-pane-markdown-heading))))
            ;; Unordered list markers.
            (goto-char (point-min))
            (while (re-search-forward "^\\([ \t]*\\)\\([-*+]\\)\\([ \t]+\\)" nil t)
              (unless (get-text-property (match-beginning 0) 'agent-pane-md-code-block)
                (add-text-properties (match-beginning 2) (match-end 2)
                                     '(face agent-pane-markdown-list-marker))))
            ;; Links: [label](url)
            (goto-char (point-min))
            (while (re-search-forward "\\[\\([^]\\n]+\\)\\](\\([^)\\n]+\\))" nil t)
              (unless (get-text-property (match-beginning 0) 'agent-pane-md-code-block)
                (add-text-properties
                 (match-beginning 0) (1+ (match-beginning 0))
                 `(face agent-pane-markdown-delimiter
                   ,@(when agent-pane-markdown-hide-markup
                       '(invisible agent-pane-markup))))
                (add-text-properties (match-beginning 1) (match-end 1)
                                     '(face agent-pane-markdown-link-label
                                       mouse-face highlight
                                       help-echo "Markdown link"))
                (add-text-properties
                 (match-end 1) (1+ (match-end 1))
                 `(face agent-pane-markdown-delimiter
                   ,@(when agent-pane-markdown-hide-markup
                       '(invisible agent-pane-markup))))
                (add-text-properties
                 (1+ (match-end 1)) (+ (match-end 1) 2)
                 `(face agent-pane-markdown-delimiter
                   ,@(when agent-pane-markdown-hide-markup
                       '(invisible agent-pane-markup))))
                (add-text-properties (match-beginning 2) (match-end 2)
                                     '(face agent-pane-markdown-code
                                       agent-pane-md-link-url t))
                (add-text-properties
                 (1- (match-end 0)) (match-end 0)
                 `(face agent-pane-markdown-delimiter
                   ,@(when agent-pane-markdown-hide-markup
                       '(invisible agent-pane-markup))))))
            ;; Strong emphasis.
            (goto-char (point-min))
            (while (re-search-forward "\\*\\*\\([^*\n][^*\n]*?\\)\\*\\*" nil t)
              (unless (get-text-property (match-beginning 0) 'agent-pane-md-code-block)
                (add-text-properties
                 (match-beginning 0) (+ (match-beginning 0) 2)
                 `(face agent-pane-markdown-delimiter
                   ,@(when agent-pane-markdown-hide-markup
                       '(invisible agent-pane-markup))))
                (add-text-properties (match-beginning 1) (match-end 1)
                                     `(face ,strong-face))
                (add-text-properties
                 (- (match-end 0) 2) (match-end 0)
                 `(face agent-pane-markdown-delimiter
                   ,@(when agent-pane-markdown-hide-markup
                       '(invisible agent-pane-markup))))))
            ;; Tables.
            (agent-pane--md--fontify-tables)))))))
(provide 'agent-pane-markdown)
;;; agent-pane-markdown.el ends here
