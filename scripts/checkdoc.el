;;; checkdoc.el --- Batch checkdoc -*- lexical-binding: t; -*-

;;; Commentary:
;; Invoked by scripts/checkdoc.sh.
;;
;; This file computes the project root from `load-file-name` to avoid
;; MSYS path translation issues on Windows.

;;; Code:

(require 'checkdoc)

(let* ((root (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))
       (lisp-dir (expand-file-name "lisp" root)))
  (dolist (f (directory-files lisp-dir t "\\.el\\'"))
    (checkdoc-file f)))

;;; checkdoc.el ends here

