# Backlog

This is a deliberately non-urgent list of features and hardening work.

For what’s already implemented, see `docs/plan.md`.

## Auth and Login

- Codex auth UX:
  - Detect when `codex-acp` is not authenticated and guide the user through login.
  - Surface ACP `authenticate` failures with actionable next steps.
  - Support multiple auth methods (login vs `OPENAI_API_KEY` vs `CODEX_API_KEY`) and per-project configuration.
- Credential handling:
  - Never log secrets in buffers or transcripts.
  - Prefer OS keychain / auth helpers when available.

## UX / UI

- Sidebar improvements:
  - search/filter
  - sorting (by recency/title)
  - rename session titles (write back to transcript header)
  - preview snippet on the session line
- Transcript replay:
  - load a transcript into the chat buffer (read-only replay or a “continue chat from here”).
- Keyboard navigation:
  - jump between blocks
  - fold/unfold message bodies
  - copy code blocks / copy last tool output
- Input editor:
  - multiline input (dedicated edit buffer)
  - history
  - edit-and-resend

## Agent / Protocol

- Permission UI for `session/request_permission`:
  - present choices
  - support “always allow” policies scoped by project/session/tool kind
  - show pending requests in the UI
- Better tool-call visualization:
  - structured rendering of command/args
  - clickable file/line locations when provided
  - show tool streaming updates incrementally (if the server sends them)
- Cancellation semantics:
  - surface server acknowledgement / stop reasons more clearly
  - make cancel resilient if a prompt is only “waiting” (no updates yet)
- Prompt capabilities:
  - embedded context
  - image/audio prompts (when supported by the server)

## Dev / Quality

- Benchmark rendering under long transcripts.
- More ERT coverage for:
  - tool_call title updates
  - cancellation + follow-up queue
  - transcript title parsing/updating
- Replace synthetic ACP traffic with captured real `codex-acp` traffic sessions as the protocol surface grows.
