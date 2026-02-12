# Testing UI Output in Headless Mode

Goal: let an automated agent load current Elisp code and observe the rendered
`agent-pane` UI output after a deterministic ACP replay.

## Why this approach

- Works in `emacs -Q --batch` (no interactive desktop needed).
- Deterministic (replay traffic file + fixed prompt).
- Produces an artifact file that the agent can read and compare.

For this repository, this is more reliable than screenshots.

## Command

From repo root:

```bash
./scripts/ui-replay.sh
```

Default inputs:

- traffic: `test/traffic/agent-pane-interleaved.traffic`
- output: `test/artifacts/agent-pane-ui-replay.txt`
- prompt: `hi`

Custom inputs:

```bash
./scripts/ui-replay.sh test/traffic/agent-pane-minimal.traffic test/artifacts/minimal.txt "hello"
```

PowerShell:

```powershell
.\scripts\ui-replay.ps1
```

## Typical close-the-loop workflow

1. Make code changes in `lisp/*.el`.
2. Run `./scripts/test.sh`.
3. Run `./scripts/ui-replay.sh`.
4. Inspect `test/artifacts/agent-pane-ui-replay.txt` and verify the rendered
   buffer output matches expectations.

## About screenshots

Screenshots are optional and environment-dependent (GUI build/runtime support).
In this project, text snapshots from replay are the primary validation method
for agent-visible UI output.
