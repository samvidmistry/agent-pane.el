# Testing With ERT (Local)

## Rules

- Tests must run headless: `emacs -Q --batch`.
- Avoid network, timers, and long sleeps in tests.
- Prefer testing pure state/render helpers, not interactive UI behaviors.

## Repository conventions

- Paren/syntax sanity is enforced via `scripts/check-parens.el` and is run as part of:
  - `./scripts/test.sh`
  - `./scripts/byte-compile.sh`
  - `./scripts/checkdoc.sh`

## Patterns

- Temp buffers:
  - Use `(with-temp-buffer ...)` or create buffers and `unwind-protect` + `kill-buffer`.
- Stubbing:
  - Use `cl-letf` to replace `symbol-function` or vars temporarily.
- Temp files:
  - `make-temp-file`, cleanup in `unwind-protect`.

## Running

- `./scripts/test.sh`
- In Emacs: `M-x agent-pane-run-all-tests` (from `.dir-locals.el`).

## ACP integration tests

ACP-level integration tests are replay-based and do not require `codex-acp`.
See `docs/testing-acp.md`.
