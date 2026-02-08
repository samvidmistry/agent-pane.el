# Notes From `agent-shell` (Vendored Reference)

This repo vendors `vendor/agent-shell/` as a reference implementation. These notes summarize patterns worth copying and pitfalls to avoid.

## Packaging + Compatibility

- Uses `-*- lexical-binding: t; -*-` everywhere.
- Targets Emacs `29.1` in `Package-Requires` (`vendor/agent-shell/agent-shell.el`).
- Uses `eval-when-compile` for `cl-lib` to avoid runtime cost when possible.
- Adds `;;;###autoload` to entrypoints.

## State Management

- Uses buffer-local state alists with keyword keys and `map.el` accessors.
  - Example: `defvar-local agent-shell--state (agent-shell--make-state)`.
- Uses `cl-defun` with keyword args for internal constructors and update functions.

## UI Rendering

From `vendor/agent-shell/agent-shell-ui.el`:

- Uses `text-property-search` and text properties to find and update “blocks”.
- Uses a buffer-local “content store” (hash table) to avoid storing large text blobs in text properties.
- Modifies buffers with `inhibit-read-only`, and selectively disables undo (`buffer-undo-list`) for bulk updates.
- Keeps edits localized and safe via `save-mark-and-excursion` and narrow/replace patterns.

## Tests

From `vendor/agent-shell/tests/`:

- Pure `ert` tests, batch-friendly.
- Uses `cl-letf` to stub functions.
- Uses `unwind-protect` for cleanup (temp files, temp buffers).
- Includes replay-style traffic tests for tricky ordering cases.

## What we’ve adopted in `agent-pane`

- Buffer-local state alist accessed via `map.el`.
- Incremental UI updates instead of full-buffer rerenders during streaming.
- “Side channel” debugging buffers for protocol traffic/logs.

Note: `agent-pane` currently uses marker-tracked message regions for updates. This keeps streaming stable and avoids flicker, while still being simple enough to reason about in tests.
