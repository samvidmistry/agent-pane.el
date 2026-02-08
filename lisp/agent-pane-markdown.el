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

(require 'subr-x)

(defun agent-pane--md--strong-face-for-role (role)

  "Return the face used for markdown strong for ROLE."

  (pcase role

    ('thought 'agent-pane-markdown-strong-thought)

    (_ 'agent-pane-markdown-strong-assistant)))

(defun agent-pane--md-fontify-range (role beg end)

  "Apply minimal markdown fontification between BEG and END.

ROLE controls how some constructs are rendered (assistant vs thought).

This is intentionally lightweight and only handles a few common patterns:

- Strong: **text**

- Inline code: `code`

- Fenced code blocks: ``` ... ```"

  (when (and (integerp beg)

             (integerp end)

             (< beg end)

             (memq role '(assistant thought)))

    (save-excursion

      (save-restriction

        (narrow-to-region beg end)

        (let ((inhibit-read-only t)

              (strong-face (agent-pane--md--strong-face-for-role role)))

          ;; Ensure our invisibility spec is present/absent.

          (if agent-pane-markdown-hide-markup

              (add-to-invisibility-spec 'agent-pane-markup)

            (remove-from-invisibility-spec 'agent-pane-markup))

          ;; Clear prior markdown properties in this region.

          (remove-text-properties (point-min) (point-max)

                                  '(face nil

                                    invisible nil

                                    display nil

                                    agent-pane-md-code-block nil))

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

                     '(invisible agent-pane-markup)))))))))))

(provide 'agent-pane-markdown)

;;; agent-pane-markdown.el ends here

