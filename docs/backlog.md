# Backlog

This is a deliberately non-urgent list of features and hardening work.

For what’s already implemented, see `docs/plan.md`.

## Auth and Login

- Codex auth UX:
  - Detect when `codex-acp` is not authenticated and guide the user through login.
  - Support multiple auth methods (login vs `OPENAI_API_KEY` vs `CODEX_API_KEY`) and per-project configuration.
- Credential handling:
  - Never log secrets in buffers or transcripts.
  - Prefer OS keychain / auth helpers when available.

## UX / UI

- Transcript replay follow-up:
  - "continue chat from here" semantics after replaying a past transcript.
- Sidebar follow-ups:
  - optional alternate grouping/sorting modes (for very large transcript sets).
- Input UX follow-ups:
  - richer dedicated editor features (e.g., mode hooks, draft persistence)
  - improved history discoverability/UX in header/help

## Agent / Protocol

- Permission UI follow-ups for `session/request_permission`:
  - richer pending-request presentation in UI blocks
  - management commands for stored allow rules (inspect/remove)
  - finer policy scope controls beyond current project/session tool-key rules
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
