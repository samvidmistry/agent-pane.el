# Vendored Source

This repo vendors upstream projects so the coding agent can read their source and tests locally.

## Included

- `acp.el` (Agent Client Protocol implementation)
  - Upstream: https://github.com/xenodium/acp.el
  - Version header: `0.9.1`
  - Pinned commit: `771e01e8605c7ce1b9618ba503a8f4139cbb4f95`

- `agent-shell` (reference ACP-based agent shell UI)
  - Upstream: https://github.com/xenodium/agent-shell
  - Version header: `0.33.2`
  - Pinned commit: `823ddf92f22e59ff9ddbed6600ce426d361f6ebb`

## Update Policy

Only update vendored projects when there is a clear reason (bugfix, needed API change).
When updating:

1. Pull upstream.
2. Record new pinned commits here.
3. Re-run `./scripts/test.sh` and `./scripts/byte-compile.sh`.

