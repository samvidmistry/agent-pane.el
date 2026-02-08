# Code structure notes (for humans)

This document is a “how I think about the code” guide for `agent-pane`.
It’s not API documentation; it’s meant to help you (future you) re-orient quickly.

## Top-level map

### Source (`lisp/`)

`agent-pane` is intentionally split into small modules so a coding agent (and a
human) can reason about changes without wading through one giant file.

- `lisp/agent-pane.el`
  - package entrypoint / user-facing commands
  - major mode, keymap
  - app layout (sidebar + chat)
  - thin glue that calls into other modules
- `lisp/agent-pane-config.el`
  - `defgroup`, `defcustom`, faces
- `lisp/agent-pane-state.el`
  - buffer-local state (`agent-pane--state`)
  - message model helpers (`agent-pane--append-message*`, setters, etc.)
- `lisp/agent-pane-ui.el`
  - buffer layout markers
  - incremental rendering (anti-flicker)
  - footer + header-line progress rendering
  - in-place message updates (`agent-pane--ui-append-to-message-text`, etc.)
- `lisp/agent-pane-markdown.el`
  - streaming-safe markdown-ish fontification (strong / inline code / fences)
- `lisp/agent-pane-transcript.el`
  - transcript creation + append
  - transcript metadata (session id, title)
- `lisp/agent-pane-acp.el`
  - ACP client lifecycle + handshake
  - `session/prompt` sending
  - streaming notifications + interleaving logic
  - tool calls + permission request plumbing
  - follow-up queue + cancel
- `lisp/agent-pane-sessions.el`
  - sessions sidebar (outline UI)
  - transcript scanning/indexing grouped by `project_root`

### Tests (`test/`)

- `test/agent-pane-tests.el`
  - UI/model unit tests
- `test/agent-pane-acp-tests.el`
  - ACP replay integration tests (`acp-fakes` + `.traffic`)
- `test/agent-pane-sessions-tests.el`
  - sidebar parsing/render tests
- `test/traffic/*.traffic`
  - recorded/synthetic ACP traffic replays

## Mental model: state → render

The core design is:

1. Maintain a buffer-local state object (`agent-pane--state`).
2. Mutate that state with small helpers (mostly in `agent-pane-state.el`).
3. Keep the buffer synchronized without erasing the whole buffer (in `agent-pane-ui.el`).

### State (`agent-pane--state`)

`agent-pane--state` is an alist keyed by keywords. Important buckets:

- `:messages`
  - list of message objects
  - each message is a map/alist with keys like `:role`, `:title`, `:text`, `:raw`
- ACP lifecycle:
  - `:client`, `:session-id`, `:connecting`, `:subscribed`
  - handshake results (`:init-result`, `:auth-result`, ...)
- Streaming bookkeeping:
  - `:streaming-assistant-index` / `:streaming-thought-index`
  - `:last-session-update-kind` (used for correct interleaving)
- Turn progress + follow-ups:
  - `:in-progress` = `waiting` / `streaming` / `cancelling` / nil
  - `:prompt-in-flight` + `:prompt-queue`
- Tool calls:
  - `:tool-calls` (hash table keyed by toolCallId)
  - tool call messages are normal messages but backed by this table
- Transcript:
  - `:transcript-file`
  - flags to avoid duplicate metadata writes (`:transcript-session-id-logged`, `:transcript-title-logged`)

## Rendering strategy (anti-flicker)

### Layout

The buffer has a stable layout:

- (optional) ACP header
- transcript area (rendered messages)
- footer separator
- status line
- input help text
- editable input region

Markers track “structural” positions:

- `agent-pane--header-end-marker`
- `agent-pane--transcript-end-marker`
- `agent-pane--status-beg-marker` / `agent-pane--status-end-marker`
- `agent-pane--input-marker`

### Message regions

Each rendered message has recorded marker locations stored in `agent-pane--msg-locs`.
That enables targeted edits for:

- appending streaming text into an existing message body
- replacing a message body
- re-rendering a message label when the title changes (e.g. tool call title arrives later)

### Rerender entrypoint

`agent-pane--rerender` (in `agent-pane-ui.el`) is the main “sync state → view” function.
Streaming chunk handlers call the in-place update helpers directly.

## ACP wiring (what happens on submit)

1. `agent-pane-submit` (entrypoint, in `agent-pane.el`)
   - appends the user message to the model
   - writes transcript heading + user text
   - sets transcript title on the first user turn
   - enqueues the prompt (`:prompt-queue`)
   - sets progress to `waiting` if idle
   - calls `agent-pane--pump-prompt-queue`

2. `agent-pane--pump-prompt-queue` (in `agent-pane-acp.el`)
   - ensures handshake exists (or starts it)
   - if idle, pops the next prompt
   - resets per-turn streaming state (`agent-pane--begin-turn`)
   - sets `:prompt-in-flight` and `:in-progress`
   - calls `agent-pane--send-prompt`

3. `agent-pane--send-prompt`
   - sends `session/prompt`
   - on completion (stopReason), ends the turn and pumps the queue again

4. Streaming events (`session/update` notifications)
   - handled by `agent-pane--on-notification`
   - uses `:last-session-update-kind` to decide whether a chunk continues the current
     message or starts a new one, preserving arrival-order interleaving.

## Where to extend next

- Permissions UI (`session/request_permission`): build an interactive chooser and add
  project-scoped “always allow” persistence.
- Transcript replay: parse transcript Markdown into message objects and reuse the renderer.
- Sidebar search/filter: keep the index in memory; don’t re-scan the filesystem on each keystroke.
