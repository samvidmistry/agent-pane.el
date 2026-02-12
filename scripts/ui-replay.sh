#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EMACS_BIN="$($ROOT/scripts/_emacs.sh)"

TRAFFIC_FILE="${1:-$ROOT/test/traffic/agent-pane-interleaved.traffic}"
OUTPUT_FILE="${2:-$ROOT/test/artifacts/agent-pane-ui-replay.txt}"
PROMPT_TEXT="${3:-hi}"

mkdir -p "$(dirname "$OUTPUT_FILE")"

exec "$EMACS_BIN" -Q --batch \
  -L "$ROOT/lisp" \
  -L "$ROOT/vendor/acp.el" \
  -L "$ROOT/test" \
  -l "$ROOT/scripts/ui-replay.el" \
  -- "$TRAFFIC_FILE" "$OUTPUT_FILE" "$PROMPT_TEXT"
