#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EMACS_BIN="$("$ROOT/scripts/_emacs.sh")"

exec "$EMACS_BIN" -Q --batch \
  -L "$ROOT/lisp" \
  -l "$ROOT/scripts/check-parens.el" \
  -l "$ROOT/scripts/checkdoc.el"
