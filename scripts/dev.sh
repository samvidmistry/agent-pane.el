#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EMACS_BIN="$("$ROOT/scripts/_emacs.sh")"

exec "$EMACS_BIN" -Q \
  -L "$ROOT/lisp" \
  -L "$ROOT/vendor/acp.el" \
  -L "$ROOT/vendor/agent-shell" \
  --eval "(require 'agent-pane)" \
  --eval "(agent-pane)"
