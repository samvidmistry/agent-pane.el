# Project Plan

This is a living plan/roadmap for `agent-pane`.

For a grab-bag of non-urgent ideas, see `docs/backlog.md`.

## Phase 0: Repo + Tooling (done)

- Vendored reference source:
  - `vendor/acp.el/`
  - `vendor/agent-shell/`
- Agent guidance: `AGENTS.md`
- Scripts: `scripts/test.sh`, `scripts/byte-compile.sh`, `scripts/checkdoc.sh`, `scripts/dev.sh`
- CI: `.github/workflows/ci.yml`
- Dev sanity tooling:
  - `scripts/check-parens.{el,sh,ps1}` integrated into test/compile/checkdoc scripts
- Code structure refactor:
  - `lisp/` is split into focused modules (`agent-pane-{config,state,ui,acp,transcript,markdown}.el`) plus the entrypoint `agent-pane.el`.

## Phase 1: Transcript Model + Rendering (done)

### Message model

Implemented an explicit message list in buffer-local state:

- roles: `user`, `assistant`, `thought`, `tool`, `system`, `acp`
- fields: `:title`, `:text`, `:raw` (for inspection)

### Rendering

- Marker-based incremental rendering to avoid flicker:
  - full render only for initial layout or corruption recovery
  - streaming updates patch message regions in-place
- Correct ordering/interleaving of assistant/thought/tool blocks based on ACP update ordering.
- Stable footer/input area:
  - no full-buffer erase during streaming
  - transcript-end marker handling prevents the footer from “jumping”

### Progress indicator

- Per-turn progress state: `waiting` → `streaming` → `nil`
- Rendered in:
  - footer status line, updated in-place
  - `header-line-format` (so it’s visible even when scrolled)

### Markdown-ish output styling

Streaming-safe lightweight fontification:

- `**strong**`
- inline code `` `code` ``
- fenced code blocks ```...```

Role-dependent strong styling:

- assistant strong: colorized, not bold
- thought strong: bold

Optional hiding of markup delimiters in the UI (`agent-pane-markdown-hide-markup`).

### Transcript persistence (done)

All chats can be persisted as Markdown transcripts:

- per-session transcript files in `agent-pane-transcript-directory`
- header metadata includes:
  - `project_root` (from `project.el`)
  - `created`, `buffer`, `codex_command`, `session_mode`, etc.
  - `acp_session_id` logged once when available
  - `title` derived from the first user turn

## Phase 2: ACP Wiring (done; minimal surface but functional)

- ACP client lifecycle:
  - `initialize` → `authenticate` → `session/new` → `session/set_mode`
- Prompt sending:
  - `session/prompt` + streaming notifications
- Streaming coverage:
  - `agent_message_chunk`
  - optional `agent_thought_chunk`
  - tool calls: `tool_call` + `tool_call_update`
  - permission requests: `session/request_permission` (policy-driven, default auto-allow)
- Follow-ups while busy:
  - prompt queue in Emacs Lisp; follow-ups are sent sequentially when the active turn finishes
- Cancellation:
  - `session/cancel` (command `agent-pane-cancel`, bound to `C-c C-k`)

ACP replay integration tests cover these behaviors (see `docs/testing-acp.md`).

## Phase 3: UX Polish (in progress)

Done:

- Collapsible sessions sidebar grouped by `project.el` project:
  - `lisp/agent-pane-sessions.el`
  - transcript title shown in sidebar entries
- App layout command opens sidebar + chat:
  - `agent-pane-app`
  - `agent-pane` defaults to app layout when `agent-pane-use-sidebar` is non-nil
- Debug/inspection commands:
  - open traffic/logs/stderr, show internal state, show raw payload at point

Next:

- Sidebar UX: search/filter, sorting toggles, rename session title.
- “Open transcript in chat view” (replay) rather than only opening the `.md` file.
- Permissions UI (explicit choices + “always allow” persistence).
- Richer Markdown handling (headings/lists/links) without compromising streaming performance.
- Input improvements (multiline editor, history, edit-and-resend).
