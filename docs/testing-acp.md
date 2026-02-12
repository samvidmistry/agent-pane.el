# Testing ACP Interactions

Goal: test `agent-pane`’s ACP handshake, streaming, and block interleaving deterministically, without `codex-acp`, network access, or login.

## Strategy

Use `acp-fakes` (vendored via `vendor/acp.el/`) plus recorded or synthetic traffic sessions.

- `acp-fakes` replays a queue of traffic objects and routes notifications/requests through the real `acp.el` routing code.
- `agent-pane` supports injecting a client via `agent-pane-acp-client-maker` so tests can run without spawning processes.

## What we test today

Traffic files:

- `test/traffic/agent-pane-minimal.traffic`
  - handshake
  - `session/prompt` + streamed assistant chunks
- `test/traffic/agent-pane-interleaved.traffic`
  - interleaved thought/assistant/tool/assistant updates
  - tool call updates and display title behavior

Tests:

- `test/agent-pane-acp-tests.el`
  - handshake + prompt streaming via traffic replay
  - interleaved thought/tool/assistant rendering behavior
  - permission decision flows (`auto-allow`, prompt mode, session/project rule persistence)

## Adding / Updating Traffic

When you have `codex-acp` working end-to-end, you can record a session and replace the synthetic traffic with a real one.

High-level approach:

1. Enable ACP logging (`acp-logging-enabled`) while running `agent-pane`.
2. Use the ACP traffic buffer save command to write a `.traffic` file.
3. Drop the file into `test/traffic/` and adjust tests to replay it.

This keeps integration tests accurate while remaining offline/replayable.

## Inspecting rendered UI output from replay

For a deterministic, headless UI snapshot (without interactive Emacs), see
`docs/testing-ui.md` and run:

- `./scripts/ui-replay.sh`
