# agent-pane (Emacs)

Build a Codex-app-like UI inside Emacs, implemented in Emacs Lisp.

> Note: This entire project is vibe-coded with GPT-5.3-Codex.

This repo vendors:

- `vendor/acp.el/` for ACP (Agent Client Protocol) client support.
- `vendor/agent-shell/` as a reference implementation (UI + tests).

## Requirements

- Emacs 29.1+ (intentionally targeting modern APIs).
- One ACP backend available on PATH, for example:
  - Codex: `codex-acp`
  - GitHub Copilot CLI: `copilot --acp`
  - Claude Code: `claude-code-acp`

## What works today

- Stable streaming UI (no full-buffer rerenders while streaming).
- Interleaved rendering of assistant / thought / tool blocks.
- Progress indicator:
  - footer status line
  - `header-line-format` (visible even when scrolled)
- ACP header now has a compact summary (provider/model) with collapsible details.
- Lightweight Markdown-ish styling (headings/lists/links/strong/code/fences) with optional hiding of markup.
- Transcript persistence (Markdown files) including per-session title + project root.
- Collapsible sessions sidebar grouped by `project.el` projects.
  - replay transcript in chat view
  - true ACP resume when transcript has `acp_session_id` and server advertises `loadSession`
  - filter/sort/rename session titles
- Permission handling modes:
  - non-interactive (`auto-allow` / `auto-cancel`)
  - interactive prompt mode (`prompt`) with optional session/project allow persistence
  - optional diff review in prompt for file-changing tool calls
- Follow-up messages:
  - prompts submitted while the agent is busy are queued and sent sequentially.
- Cancel:
  - `C-c C-k` sends ACP `session/cancel`.

Auth/login UX is partial (actionable authenticate-failure guidance exists, full login flow UX is pending); see `docs/backlog.md`.

## ACP provider profiles

`agent-pane` supports provider profiles via `agent-pane-acp-provider`:

- `codex` (default)
- `copilot`
- `claude-code`
- `custom`

Use `M-x customize-group RET agent-pane RET` or `C-c C-a` in chat buffer to switch.

Model selection is configurable via `agent-pane-session-model-id`.
Use `C-c m` in chat to set/clear it interactively; when a session is active,
agent-pane sends ACP `session/set_model` immediately.

Diff review is configurable via:
- `agent-pane-diff-viewer` (`diff-mode` or `ediff`)
- `agent-pane-permission-review-diff` (include review option in permission prompt)

## Key bindings (chat buffer)

- `C-c .` open command menu (transient dispatch)
  - alias: `C-c C-.`
- `C-c C-c` send
- `C-c C-k` cancel
- `C-c C-q` exit app (close chat + sessions buffers)
- `C-c C-t` open ACP traffic
- `C-c C-l` open ACP logs
- `C-c C-e` open ACP stderr
- `C-c C-s` show internal state
- `C-c C-r` show raw payload at point
- `C-c C-a` switch ACP provider profile (codex/copilot/claude-code/custom)
- `C-c v` toggle ACP header details (summary-only vs full debug)
- `C-c m` set ACP model id (`session/set_model`; empty input clears preference)
  - alias: `C-c C-m` (same as `C-c RET` in many terminals)
- `C-c d` view structured file-change diff at point (`diff-mode` or `ediff`)
  - alias: `C-c C-d`
- `C-c o` toggle tool output mode (preview tail/full output)
  - alias: `C-c C-o`
- `C-c i` open multiline input editor
  - alias: `C-c C-i`
- `C-c C-y` copy last user prompt into input (edit-and-resend helper)
- `C-c C-w` copy last tool output to clipboard
- `C-c C-b` copy fenced code block body at point
- `M-p` / `M-n` browse input history
- `C-c C-p` / `C-c C-n` jump to previous/next message block
- `C-c C-f` fold/unfold message body at point

## Key bindings (sessions sidebar)

- `RET` replay transcript into chat buffer (reuses right chat pane when sidebar is open)
  - live status prefixes: `[RUN]`, `[DONE]`, `[WAIT]`
- `o` open raw transcript `.md`
- `n` new chat for project at point
- `/` set filter, `c` clear filter
- `s` toggle sort (recency/title)
- `r` rename transcript title (writes header)
- `C-k` delete transcript at point (with confirmation)
- `TAB` fold/unfold project group
- `q` exit app (close chat + sessions buffers)

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

Replay ACP traffic and dump rendered UI text snapshot:

```bash
./scripts/ui-replay.sh
```

From PowerShell:

```powershell
.\scripts\ui-replay.ps1
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
