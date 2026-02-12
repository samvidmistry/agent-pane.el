# Agent Notes (agent-pane)

This repository is intentionally set up so an automated coding agent can work effectively with minimal external context.

## Environment

- You are typically running in **Git Bash** on Windows.
- Prefer **bash commands** over PowerShell when possible.
- Emacs might not be on `PATH`. Scripts accept `EMACS=/path/to/emacs.exe`.
  - Example: `EMACS="/c/Program Files/Emacs/emacs-30.2/bin/emacs.exe" ./scripts/test.sh`

## Project Goal

Build a Codex-app-like UI inside Emacs, implemented in Emacs Lisp.

- The UI is **based on ACP** (Agent Client Protocol) via vendored `acp.el`.
- `vendor/agent-shell/` is vendored as a **reference implementation** by a veteran Emacs developer.
  - Use it for patterns (buffer state, UI blocks, text-properties, tests), but do not cargo-cult its architecture.

## Repo Layout

- `lisp/` project source
- `test/` ERT tests (batch-friendly)
- `vendor/acp.el/` vendored ACP implementation (pinned git checkout)
- `vendor/agent-shell/` vendored agent-shell reference (pinned git checkout)
- `docs/` internal guides and extracted practices
- `scripts/` bash scripts to compile/lint/test

## Quick Commands

All commands below assume repo root.

- Launch a clean Emacs with this project loaded:
  - `./scripts/dev.sh`
  - From PowerShell: `.\scripts\dev.ps1`
- Run tests (ERT, batch):
  - `./scripts/test.sh`
  - From PowerShell: `.\scripts\test.ps1`
- Replay ACP traffic and snapshot rendered UI text (headless):
  - `./scripts/ui-replay.sh`
  - From PowerShell: `.\scripts\ui-replay.ps1`
- Byte-compile project:
  - `./scripts/byte-compile.sh`
  - From PowerShell: `.\scripts\byte-compile.ps1`
- Run checkdoc:
  - `./scripts/checkdoc.sh`
  - From PowerShell: `.\scripts\checkdoc.ps1`

## Coding Conventions (Local)

- Every `.el` file: `-*- lexical-binding: t; -*-`
- Prefer modern built-ins available in Emacs 29.1+:
  - `map.el` (`map-elt`, `map-put!`, `map-nested-elt`)
  - `subr-x` helpers (`when-let`, `if-let`, `string-empty-p`, `string-trim`)
  - `text-property-search` for efficient navigation
- UI edits:
  - Use `inhibit-read-only` and keep edits localized (`save-mark-and-excursion`, `save-restriction`, `narrow-to-region`).
  - Avoid storing huge strings in text properties; use a side store (see `vendor/agent-shell/agent-shell-ui.el`).
- Tests:
  - Use `ert`, `cl-letf` for stubs, and `unwind-protect` for cleanup.

### Emacs Lisp docstrings & string-literal pitfalls (common failures)

When editing `.el` files, be extra careful with docstrings and string literals:

- **Escape double quotes inside strings/docstrings**: write `\"` inside the Elisp string.
  - Bad: `"This is what we show after "Tool"."` (breaks the string)
  - Good: `"This is what we show after `Tool'."` (avoid quotes) or
    `"... after \\\"Tool\\\" ..."`
- Prefer **Elisp docstring quoting style** for symbols/commands:
  - Use backticks + apostrophe: `See `agent-pane'.` (i.e. `` `foo' ``), not `"foo"`.
- **Avoid smart quotes / unicode punctuation** in docstrings (e.g. `…`, “ ”) unless intentional.
  They can trigger `checkdoc` warnings or make matching/grepping harder.
- Docstrings should **not start or end with whitespace** and should not contain trailing spaces.
- After touching docstrings/strings, always run:
  - `./scripts/checkdoc.sh`
  - `./scripts/byte-compile.sh`
  These catch issues like: `probable \"\" without \\ in doc string`.

### Line endings + whitespace normalization (required)

Do not use ad-hoc `sed`/PowerShell/newline hacks. Use the repo helper:

- Normalize specific files/dirs:
  - `./scripts/normalize.sh lisp/ docs/ README.md`
- Normalize all common text files (skips `.git/` and `vendor/`):
  - `./scripts/normalize.sh --all`
- Check mode (CI-friendly):
  - `./scripts/normalize.sh --check --all`

If invoking from PowerShell/tooling, run via:
- `bash -lc "./scripts/normalize.sh --all"`

## Change Validation (Required)

For every new feature or code change:

- Close the loop with verification:
  - Prefer an automated test (ERT) when feasible, OR
  - Perform a documented dry run (step-by-step, reproducible commands) when the behavior is inherently interactive.
- If you cannot properly test a change with existing tools, first build the tooling needed to test it well.
  - Example: add deterministic fakes, traffic capture/replay, or batch-mode scripts so the agent can validate changes autonomously.
- For sessions/sidebar UX changes (`lisp/agent-pane-sessions.el`), add/extend ERTs in
  `test/agent-pane-sessions-tests.el` for:
  - filtering/sorting behavior,
  - transcript replay loading,
  - title rename persistence in transcript headers.
- For input and keyboard UX changes (`agent-pane.el` input/history/jump/copy helpers),
  add/extend tests in `test/agent-pane-tests.el` covering history traversal,
  editor commit, and message navigation/copy helpers.
- For permission-flow changes (`session/request_permission`), add/extend tests in
  `test/agent-pane-acp-tests.el` for:
  - noninteractive safety,
  - prompt decision paths,
  - session/project rule persistence.
- For rendering/state-flow changes, run both:
  - `./scripts/test.sh`
  - `./scripts/ui-replay.sh`

## ACP Integration (Current)

- `agent-pane` will attempt to connect to the selected ACP provider on submit when:
  - `agent-pane-enable-acp` is non-nil, and
  - Emacs is interactive (`noninteractive` is nil).
- Provider profile is selected via `agent-pane-acp-provider`:
  - `codex` → `agent-pane-codex-command` (default `("codex-acp")`)
  - `copilot` → `agent-pane-copilot-command` (default `("copilot" "--acp")`)
  - `claude-code` → `agent-pane-claude-code-command` (default `("claude-code-acp")`)
  - `custom` → manual command via `agent-pane-codex-command`
- Codex-only overrides (`agent-pane-codex-config-overrides`) are appended as
  `codex-acp -c ...` flags when provider is `codex`.
- Current protocol coverage is intentionally minimal:
  - Streams `"session/update"` → `"agent_message_chunk"` into the active assistant message.
  - Authentication request is sent only when `agent-pane-auth-method-id` is non-nil.
  - After `session/new`, sends `session/set_mode` using `agent-pane-session-mode-id` (default: `"bypassPermissions"`).
  - Handles `"session/request_permission"` according to `agent-pane-permission-policy`.
    - `auto-allow` / `auto-cancel` are non-interactive policies.
    - `prompt` asks for explicit choice and can persist allow rules by tool-key
      for session scope and project scope (project rules stored in
      `agent-pane-permission-rules-file`).

## ACP Testing

- Integration tests use `acp-fakes` + replay traffic (no `codex-acp` required).
  - Docs: `docs/testing-acp.md`
  - Tests: `test/agent-pane-acp-tests.el`
  - Traffic: `test/traffic/agent-pane-minimal.traffic`

## Reference Code (Read First)

- ACP client: `vendor/acp.el/acp.el`
- agent-shell main: `vendor/agent-shell/agent-shell.el`
- agent-shell UI block rendering: `vendor/agent-shell/agent-shell-ui.el`
- agent-shell tests + patterns: `vendor/agent-shell/tests/`
- extracted notes: `docs/agent-shell-practices.md`
- backlog: `docs/backlog.md`

## Vendored Dependencies

Vendoring is deliberate so the agent has local, inspectable source.

- `vendor/acp.el` pinned commit: recorded in `vendor/README.md`
- `vendor/agent-shell` pinned commit: recorded in `vendor/README.md`

To update vendor checkouts, do it intentionally and refresh `vendor/README.md`.

## Terminal Note

The tool runner is often invoked from PowerShell. If you need bash semantics, run commands through `bash -lc "..."`.
If `.sh` files open in VSCode instead of running, use the `.ps1` wrappers above or explicitly run `bash -lc "./scripts/test.sh"`.
