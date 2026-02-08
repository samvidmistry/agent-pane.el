;;; byte-compile.el --- Batch byte compilation -*- lexical-binding: t; -*-

;;; Commentary:
;; Invoked by scripts/byte-compile.sh.
;;
;; This file computes the project root from `load-file-name` to avoid
;; MSYS path translation issues on Windows.

;;; Code:

(let* ((root (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))
       (lisp-dir (expand-file-name "lisp" root)))
  (add-to-list 'load-path lisp-dir)
  (add-to-list 'load-path (expand-file-name "vendor/acp.el" root))
  (setq byte-compile-error-on-warn t)
  (byte-recompile-directory lisp-dir 0))

;;; byte-compile.el ends here

