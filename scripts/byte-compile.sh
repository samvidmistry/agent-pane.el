#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EMACS_BIN="$("$ROOT/scripts/_emacs.sh")"

exec "$EMACS_BIN" -Q --batch \
  -L "$ROOT/lisp" \
  -L "$ROOT/vendor/acp.el" \
  -l "$ROOT/scripts/check-parens.el" \
  -l "$ROOT/scripts/byte-compile.el"
