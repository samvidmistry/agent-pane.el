# agent-pane (Emacs)

Build a Codex-app-like UI inside Emacs, implemented in Emacs Lisp.

This repo vendors:

- `vendor/acp.el/` for ACP (Agent Client Protocol) client support.
- `vendor/agent-shell/` as a reference implementation (UI + tests).

## Requirements

- Emacs 29.1+ (intentionally targeting modern APIs).
- `codex-acp` available on PATH (or customize `agent-pane-codex-command`).

## What works today

- Stable streaming UI (no full-buffer rerenders while streaming).
- Interleaved rendering of assistant / thought / tool blocks.
- Progress indicator:
  - footer status line
  - `header-line-format` (visible even when scrolled)
- Lightweight Markdown-ish styling (strong/code/fences) with optional hiding of markup.
- Transcript persistence (Markdown files) including per-session title + project root.
- Collapsible sessions sidebar grouped by `project.el` projects.
- Follow-up messages:
  - prompts submitted while the agent is busy are queued and sent sequentially.
- Cancel:
  - `C-c C-k` sends ACP `session/cancel`.

Auth/login UX is not implemented yet; see `docs/backlog.md`.

## Key bindings (chat buffer)

- `C-c C-c` send
- `C-c C-k` cancel
- `C-c C-t` open ACP traffic
- `C-c C-l` open ACP logs
- `C-c C-e` open ACP stderr
- `C-c C-s` show internal state
- `C-c C-r` show raw payload at point

## Development

Launch a clean Emacs with this project loaded:

```bash
./scripts/dev.sh
```

From PowerShell:

```powershell
.\scripts\dev.ps1
```

Run tests:

```bash
./scripts/test.sh
```

From PowerShell:

```powershell
.\scripts\test.ps1
```

Byte compile:

```bash
./scripts/byte-compile.sh
```

From PowerShell:

```powershell
.\scripts\byte-compile.ps1
```

If `emacs` is not on your PATH:

```bash
EMACS="/c/Program Files/Emacs/emacs-30.2/bin/emacs-30.2/bin/emacs.exe" ./scripts/test.sh
```

## ACP testing

ACP interaction is covered by replay-based integration tests (no `codex-acp` required) via `acp-fakes`.

See `docs/testing-acp.md`.
