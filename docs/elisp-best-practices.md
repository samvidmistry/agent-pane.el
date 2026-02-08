# Emacs Lisp Best Practices (Local)

This is a short, pragmatic checklist for this repo (not a general essay).

## File Basics

- Always enable lexical binding: `-*- lexical-binding: t; -*-`
- End files with: `(provide 'feature)` and `;;; file.el ends here`
- Keep headers accurate: package name, summary, `Package-Requires`.

## Dependencies

- Prefer built-in libs: `map`, `subr-x`, `seq`, `json`, `cl-lib` (compile-time when possible).
- Use `(eval-when-compile (require 'cl-lib))` when only macros are needed.

## Customization

- Use `defgroup` + `defcustom` for user-facing config.
- Document defaults and behavior in docstrings.

## Docstrings and strings

- Escape `"` inside docstrings (or prefer `\=`\=`foo'\=` style quoting).
- Avoid trailing whitespace in docstrings; `checkdoc` will complain.
- Prefer `\=`\=`like-this'\=` for symbols/commands in docstrings.

(See also the “docstring pitfalls” section in `AGENTS.md`.)

## Buffer/UI Code

- Keep “model/state” separate from “view/render”.
- When mutating UI buffers:
  - Use `let ((inhibit-read-only t)) ...`
  - Preserve point/mark with `save-mark-and-excursion` or marker-based helpers
  - Consider disabling undo for large rewrites (`buffer-undo-list`)
- Avoid storing large strings in text properties. Use an auxiliary store (hash table) keyed by ids.

## Error Handling

- Fail loudly and early for programmer errors (`error`), and use `user-error` for interactive issues.
- Prefer `when-let` / `if-let` to avoid deeply nested conditionals.

## Docs Worth Knowing

- Emacs Lisp Manual: `(info "(elisp)")`
- ERT: `(info "(ert)")` and `M-x ert`
- Checkdoc: `M-x checkdoc`
